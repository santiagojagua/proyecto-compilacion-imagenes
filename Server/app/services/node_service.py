# Server/app/services/node_service.py
import os
import time
import traceback
from typing import List, Dict, Any
import Pyro4
from concurrent.futures import ThreadPoolExecutor, as_completed

Pyro4.config.DETAILED_TRACEBACK = True

# Timeouts
CONNECT_TIMEOUT = 3      # para health checks
CALL_TIMEOUT = 60        # para procesamiento

def _parse_nodes_from_env() -> List[Dict[str, Any]]:
    env = os.getenv("PYRO_NODES", "").strip()
    nodes = []
    if env:
        for i, token in enumerate(env.split(","), start=1):
            token = token.strip()
            if not token:
                continue
            if ":" not in token:
                continue
            host, port = token.split(":", 1)
            nodes.append({"id": i, "host": host.strip(), "port": int(port.strip())})
    if not nodes:
        # Fallback: single local node
        nodes = [{"id": 1, "host": "localhost", "port": 9090}]
    return nodes

def _uri(host: str, port: int, service: str) -> str:
    return f"PYRO:{service}@{host}:{port}"

def _connect(uri: str, timeout: float):
    p = Pyro4.Proxy(uri)
    p._pyroTimeout = timeout
    return p

def _probe_node(node: Dict[str, Any]) -> Dict[str, Any]:
    """
    Devuelve métricas del nodo o error. Score bajo = menos carga.
    Métricas usadas:
      - memoria (procesador.obtener_estado_servidor)
      - queue_size (procesador.get_status)
      - tareas_activas/cola_pendientes/workers (gestor_hilos.obtener_estado)
      - latency_ms
    """
    host, port = node["host"], node["port"]
    t0 = time.time()
    try:
        # procesador.imagenes
        with _connect(_uri(host, port, "procesador.imagenes"), CONNECT_TIMEOUT) as p:
            estado = p.obtener_estado_servidor()
            status = p.get_status()
        # gestor.hilos (opcional)
        gh_estado = {}
        try:
            with _connect(_uri(host, port, "gestor.hilos"), CONNECT_TIMEOUT) as gh:
                gh_estado = gh.obtener_estado()
        except Exception:
            gh_estado = {}

        latency_ms = int((time.time() - t0) * 1000)

        memoria = float(estado.get("memoria_usage", 0.0))
        queue_size = int(status.get("queue_size", 0))
        tareas_activas = int(gh_estado.get("tareas_activas", 0))
        cola_pendientes = int(gh_estado.get("cola_pendientes", 0))
        max_workers = int(gh_estado.get("max_workers", gh_estado.get("workers_activos", 0)) or 0)

        # Normalización simple
        mem_n = memoria / 100.0
        workers_n = (tareas_activas / max_workers) if max_workers else 0.0
        queue_n = min((queue_size + cola_pendientes) / 100.0, 1.0)

        # Score ponderado (ajusta pesos a tu gusto)
        score = 0.5 * mem_n + 0.3 * workers_n + 0.2 * queue_n

        return {
            "ok": True,
            "node": node,
            "latency_ms": latency_ms,
            "memoria": memoria,
            "queue_size": queue_size,
            "tareas_activas": tareas_activas,
            "cola_pendientes": cola_pendientes,
            "max_workers": max_workers,
            "score": round(score, 4),
            "raw": {"estado": estado, "status": status, "gestor": gh_estado},
        }
    except Exception as e:
        return {
            "ok": False,
            "node": node,
            "error": str(e),
            "latency_ms": int((time.time() - t0) * 1000),
        }

class NodeManager:
    def __init__(self, ttl_status: float = 5.0):
        self.nodes = _parse_nodes_from_env()
        self.ttl = ttl_status
        self._cache = {}  # key -> {"ts": float, "data": dict}

    def list_nodes(self) -> List[Dict[str, Any]]:
        return self.nodes

    def status_all(self, force: bool = False) -> List[Dict[str, Any]]:
        now = time.time()
        results = []

        def key_for(n): return f"{n['host']}:{n['port']}"

        with ThreadPoolExecutor(max_workers=min(8, len(self.nodes))) as ex:
            futures = {}
            for n in self.nodes:
                k = key_for(n)
                if not force and k in self._cache and (now - self._cache[k]["ts"] < self.ttl):
                    results.append(self._cache[k]["data"])
                else:
                    futures[ex.submit(_probe_node, n)] = n

            for fut in as_completed(futures):
                data = fut.result()
                k = key_for(data["node"])
                self._cache[k] = {"ts": now, "data": data}
                results.append(data)

        # Normalize ordering by score/latency
        def sort_key(x):
            if not x.get("ok"):
                return (1, 999999, x["node"]["host"])
            return (0, x["score"], x["latency_ms"])
        results.sort(key=sort_key)
        return results

    def pick_best(self) -> Dict[str, Any]:
        statuses = self.status_all(force=True)
        candidates = [s for s in statuses if s.get("ok")]
        if not candidates:
            raise ConnectionError("No hay nodos Pyro disponibles")
        # menor score, luego menor latencia
        candidates.sort(key=lambda s: (s["score"], s["latency_ms"]))
        return candidates[0]  # {'ok':True, 'node': {...}, ...}

# Instancia global del manager
_manager = NodeManager()

def conectar_servidor_imagenes(node: Dict[str, Any] = None):
    """
    Conecta al mejor nodo (o al indicado) y retorna el proxy y metadatos.
    """
    try:
        if node is None:
            best = _manager.pick_best()
            node = best["node"]
        host, port = node["host"], node["port"]
        uri = _uri(host, port, "procesador.imagenes")
        procesador = _connect(uri, CALL_TIMEOUT)
        # Probar conexión mínima
        saludo = procesador.saludar()
        return procesador, node, uri, saludo
    except Exception as e:
        raise ConnectionError(f"No se pudo conectar a un servidor Pyro4: {e}")

def procesar_imagenes_pyro(lista_imagenes: List[Dict], user_id: int, user_name: str) -> Dict[str, Any]:
    """
    Envía imágenes al mejor nodo disponible. Si falla, intenta con el siguiente.
    """
    last_error = None
    estados = _manager.status_all(force=True)
    # Ordenamos nodos por score/latencia
    nodos = [s for s in estados if s.get("ok")]
    nodos.sort(key=lambda s: (s["score"], s["latency_ms"]))
    if not nodos:
        return {
            "success": False,
            "error": "No hay nodos Pyro disponibles",
            "mensaje": "No hay nodos Pyro disponibles",
        }

    for cand in nodos:
        node = cand["node"]
        try:
            procesador, node_used, uri, saludo = conectar_servidor_imagenes(node)
            resultado = procesador.procesar_imagen_cambios(
                usuario_id=user_id,
                usuario_nombre=user_name,
                imagenes=lista_imagenes
            )
            return {
                "success": True,
                "resultado": resultado,
                "mensaje": f'Procesadas {resultado.get("total_procesadas", 0)} imágenes correctamente',
                "node_used": node_used,
                "uri_used": uri
            }
        except Exception as e:
            last_error = traceback.format_exc()
            continue

    return {
        "success": False,
        "error": "Falló el procesamiento en todos los nodos disponibles",
        "error_details": last_error,
        "mensaje": "Error en el procesamiento de imágenes con Pyro4 (multinodo)"
    }

def obtener_estado_servidor() -> Dict[str, Any]:
    """
    Retorna el estado agregado y el nodo recomendado.
    """
    try:
        statuses = _manager.status_all(force=True)
        best = _manager.pick_best()
        return {
            "success": True,
            "nodos": statuses,
            "recomendado": best,
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
        }

def probar_conexion_pyro() -> Dict[str, Any]:
    """
    Prueba conexión a todos los nodos y retorna metadatos básicos.
    """
    out = []
    sts = _manager.status_all(force=True)
    for s in sts:
        d = {"node": s["node"], "ok": s["ok"]}
        if s.get("ok"):
            d.update({
                "score": s["score"],
                "latency_ms": s["latency_ms"],
                "memoria": s.get("memoria"),
                "queue_size": s.get("queue_size"),
                "tareas_activas": s.get("tareas_activas"),
            })
        else:
            d["error"] = s.get("error")
        out.append(d)
    return {
        "success": True,
        "nodes": out
    }