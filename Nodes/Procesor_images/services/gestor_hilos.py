import threading
import queue
import cv2
import time
from typing import List, Dict, Any
from concurrent.futures import ThreadPoolExecutor
import Pyro4
import base64
import numpy as np

from ..core.config import Config
from ..utils.logger import setup_logging

@Pyro4.expose
class GestorHilos:
    def __init__(self, max_hilos=Config.MAX_HILOS):
        self.max_hilos = max_hilos
        self.cola_tareas = queue.Queue()
        self.hilos_activos = []
        self.lock = threading.Lock()
        self.tareas_activas = 0
        self.executor = ThreadPoolExecutor(max_workers=max_hilos)
        self.logger = setup_logging(__name__)
    
    @Pyro4.expose
    def agregar_tarea_individual(self, image_path, output_dir):
        """Agrega una tarea de procesamiento individual"""
        tarea = {
            'tipo': 'individual',
            'image_path': image_path,
            'output_dir': output_dir
        }
        self.cola_tareas.put(tarea)
        return {"status": "en_cola", "tarea_id": image_path}
        
    @Pyro4.expose
    def agregar_tarea_lote(self, input_dir, output_dir, batch_size=10):
        """Agrega una tarea de procesamiento por lotes"""
        tarea = {
            'tipo': 'lote',
            'input_dir': input_dir,
            'output_dir': output_dir,
            'batch_size': batch_size
        }
        self.cola_tareas.put(tarea)
        return {"status": "en_cola", "tarea_id": f"lote_{input_dir}"}
    
    @Pyro4.expose
    def agregar_tarea_base64(self, imagen_base64: str, nombre_archivo: str, transformaciones: List[Dict] = None) -> Dict:
        """Agrega una tarea de procesamiento de imagen en base64"""
        tarea = {
            'tipo': 'base64',
            'imagen_base64': imagen_base64,
            'nombre_archivo': nombre_archivo,
            'transformaciones': transformaciones or []
        }
        self.cola_tareas.put(tarea)
        return {"status": "en_cola", "tarea_id": nombre_archivo}
    
    @Pyro4.expose
    def procesar_tarea_individual(self, tarea):
        """Procesa una tarea individual"""
        try:
            from .procesador_archivos import ProcesadorArchivos  # ✅ CORRECTO
            procesador = ProcesadorArchivos()
            resultado = procesador.procesar_imagen(
                tarea['image_path'],
                tarea['output_dir']
            )
            return resultado
        except Exception as e:
            return {'status': 'error', 'message': str(e)}
    
    @Pyro4.expose
    def procesar_tarea_lote(self, tarea):
        """Procesa una tarea de lote"""
        try:
            from .procesador_archivos import ProcesadorArchivos  # ✅ CORRECTO
            procesador = ProcesadorArchivos()
            resultado = procesador.procesar_lote(
                tarea['input_dir'],
                tarea['output_dir'],
                tarea['batch_size']
            )
            return resultado
        except Exception as e:
            return {'status': 'error', 'message': str(e)}
    
    @Pyro4.expose
    def procesar_tarea_base64(self, tarea):
        """Procesa una tarea de imagen base64"""
        try:
            # Decodificar base64 y procesar
            imagen_data = base64.b64decode(tarea['imagen_base64'])
            nparr = np.frombuffer(imagen_data, np.uint8)
            img = cv2.imdecode(nparr, cv2.IMREAD_COLOR)
            
            if img is None:
                return {'status': 'error', 'message': 'No se pudo decodificar la imagen base64'}
            
            # Aplicar transformaciones
            for transformacion in tarea['transformaciones']:
                img = self._aplicar_transformacion(img, transformacion)
            
            # Codificar resultado a base64
            _, buffer = cv2.imencode('.jpg', img)
            imagen_procesada_base64 = base64.b64encode(buffer).decode('utf-8')
            
            return {
                'status': 'success',
                'nombre_archivo': tarea['nombre_archivo'],
                'imagen_procesada': imagen_procesada_base64,
                'dimensiones': f"{img.shape[1]}x{img.shape[0]}"
            }
            
        except Exception as e:
            return {'status': 'error', 'message': f"Error procesando base64: {str(e)}"}
    
    def _aplicar_transformacion(self, imagen, transformacion: Dict):
        """Aplica una transformación específica a la imagen"""
        tipo = transformacion.get('tipo', '')
        parametros = transformacion.get('parametros', {})
        
        if tipo == 'redimensionar':
            ancho = parametros.get('ancho', 800)
            alto = parametros.get('alto', 600)
            return cv2.resize(imagen, (ancho, alto), interpolation=cv2.INTER_LANCZOS4)
        
        elif tipo == 'escala_grises':
            return cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        elif tipo == 'rotar':
            angulo = parametros.get('angulo', 90)
            # Lógica de rotación según el ángulo
            if angulo == 90:
                return cv2.rotate(imagen, cv2.ROTATE_90_CLOCKWISE)
            elif angulo == 180:
                return cv2.rotate(imagen, cv2.ROTATE_180)
            elif angulo == 270:
                return cv2.rotate(imagen, cv2.ROTATE_90_COUNTERCLOCKWISE)
            else:
                # Rotación personalizada
                h, w = imagen.shape[:2]
                centro = (w // 2, h // 2)
                matriz = cv2.getRotationMatrix2D(centro, angulo, 1.0)
                return cv2.warpAffine(imagen, matriz, (w, h))
        
        elif tipo == 'brillo':
            factor = parametros.get('factor', 1.0)
            return cv2.convertScaleAbs(imagen, alpha=factor, beta=0)
        
        elif tipo == 'contraste':
            factor = parametros.get('factor', 1.0)
            return cv2.convertScaleAbs(imagen, alpha=factor, beta=0)
        
        elif tipo == 'nitidez':
            kernel = np.array([[-1,-1,-1], [-1,9,-1], [-1,-1,-1]])
            return cv2.filter2D(imagen, -1, kernel)
        
        elif tipo == 'desenfocar':
            radio = int(parametros.get('radio', 3))
            if radio < 1:
                radio = 1
            if radio % 2 == 0:
                radio += 1
            return cv2.GaussianBlur(imagen, (radio, radio), 0)

        # Agregar más transformaciones según sea necesario
        return imagen
    
    def worker(self):
        """Worker que procesa tareas de la cola"""
        while True:
            try:
                tarea = self.cola_tareas.get(timeout=1)
                
                if tarea['tipo'] == 'individual':
                    resultado = self.procesar_tarea_individual(tarea)
                elif tarea['tipo'] == 'lote':
                    resultado = self.procesar_tarea_lote(tarea)
                elif tarea['tipo'] == 'base64':
                    resultado = self.procesar_tarea_base64(tarea)
                
                self.logger.info(f"Tarea completada: {resultado.get('status', 'unknown')}")
                self.cola_tareas.task_done()
                
            except queue.Empty:
                continue
            except Exception as e:
                self.logger.error(f"Error en worker: {e}")
                self.cola_tareas.task_done()
    
    @Pyro4.expose
    def iniciar_workers(self):
        """Inicia los workers"""
        for i in range(self.max_hilos):
            thread = threading.Thread(target=self.worker, name=f"Worker-{i+1}")
            thread.daemon = True
            thread.start()
            self.hilos_activos.append(thread)
        
        self.logger.info(f"Iniciados {self.max_hilos} workers")

    @Pyro4.expose
    def procesar_paralelo(self, tareas: List[Dict]) -> Dict[str, Any]:
        """
        Procesa una lista de tareas en paralelo usando hilos
        """
        resultados = {}
        futures = {}
        
        with self.lock:
            self.tareas_activas += len(tareas)
        
        self.logger.info(f"Procesando {len(tareas)} tareas en paralelo...")
        
        # Enviar cada tarea a un hilo
        for tarea in tareas:
            future = self.executor.submit(self._ejecutar_tarea, tarea)
            futures[future] = tarea.get('nombre', 'sin_nombre')
        
        # Recoger resultados
        for future in futures:
            nombre = futures[future]
            try:
                resultado = future.result(timeout=Config.WORKER_TIMEOUT)
                resultados[nombre] = resultado
            except Exception as e:
                resultados[nombre] = {'error': str(e)}
        
        with self.lock:
            self.tareas_activas -= len(tareas)
        
        return resultados
    
    def _ejecutar_tarea(self, tarea: Dict) -> Any:
        """
        Ejecuta una tarea individual en un hilo
        """
        funcion = tarea['funcion']
        args = tarea.get('args', ())
        kwargs = tarea.get('kwargs', {})
        
        try:
            return funcion(*args, **kwargs)
        except Exception as e:
            return {'error': f"Error en tarea: {str(e)}"}
    
    @Pyro4.expose
    def obtener_estado(self) -> Dict[str, Any]:
        """
        Retorna el estado actual del gestor de hilos
        """
        return {
            'max_workers': self.max_hilos,
            'tareas_activas': self.tareas_activas,
            'hilos_disponibles': self.max_hilos - self.tareas_activas,
            'cola_pendientes': self.cola_tareas.qsize(),
            'workers_activos': len([t for t in self.hilos_activos if t.is_alive()]),
            'timestamp': time.time()
        }
    
    @Pyro4.expose
    def shutdown(self):
        """
        Apaga el ejecutor de hilos
        """
        self.logger.info("Apagando gestor de hilos...")
        self.executor.shutdown(wait=True)
        self.logger.info("Gestor de hilos apagado correctamente")