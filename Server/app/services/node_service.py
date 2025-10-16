import Pyro4
from typing import List, Dict, Any
import traceback

# Configuración de conexión Pyro4
PYRO_URI = "PYRO:procesador.imagenes@localhost:9090"

def conectar_servidor_imagenes():
    """Conecta al servidor Pyro4 de procesamiento de imágenes"""
    try:
        print("🔗 Intentando conectar con Pyro4...")
        procesador = Pyro4.Proxy(PYRO_URI)
        # Configurar timeout
        procesador._pyroTimeout = 60  # Aumentado para procesamiento de lotes
        # Probar conexión
        saludo = procesador.saludar()
        print(f"✅ Conexión Pyro4 establecida: {saludo}")
        return procesador
    except Exception as e:
        print(f"❌ Error detallado en conexión Pyro4:")
        print(f"URI intentada: {PYRO_URI}")
        print(traceback.format_exc())
        raise ConnectionError(f"No se pudo conectar al servidor Pyro4: {e}")

def procesar_imagenes_pyro(lista_imagenes: List[Dict], user_id: int, user_name: str) -> Dict[str, Any]:
    """
    Envía las imágenes al servidor Pyro4 para procesamiento con información de usuario
    """
    try:
        print(f"📤 Preparando {len(lista_imagenes)} imágenes para Pyro4 (Usuario: {user_name}, ID: {user_id})...")
        
        Pyro4.config.DETAILED_TRACEBACK = True
        # Conectar al servidor
        procesador = conectar_servidor_imagenes()
        
        # ✅ CORREGIDO: Llamar al método procesar_imagen_cambios con información real del usuario
        resultado = procesador.procesar_imagen_cambios(
            usuario_id=user_id,
            usuario_nombre=user_name,
            imagenes=lista_imagenes
        )
        
        print(f"✅ Procesamiento Pyro4 completado: {resultado.get('total_procesadas', 0)} imágenes procesadas")
        
        return {
            'success': True,
            'resultado': resultado,
            'mensaje': f'Procesadas {resultado.get("total_procesadas", 0)} imágenes correctamente'
        }
        
    except Exception as e:
        print(f"❌ ERROR en procesar_imagenes_pyro:")
        error_details = traceback.format_exc()
        print(error_details)
        
        return {
            'success': False,
            'error': str(e),
            'error_details': error_details,
            'mensaje': 'Error en el procesamiento de imágenes con Pyro4'
        }

def obtener_estado_servidor() -> Dict[str, Any]:
    """Obtiene el estado del servidor Pyro4"""
    try:
        procesador = conectar_servidor_imagenes()
        estado = procesador.obtener_estado_servidor()
        return {
            'success': True,
            'estado': 'conectado',
            'detalles': estado
        }
    except Exception as e:
        return {
            'success': False,
            'estado': 'desconectado',
            'error': str(e)
        }

def call_node(node_id: int, data: dict) -> Dict[str, Any]:
    """
    Función de compatibilidad para llamadas al nodo
    """
    try:
        print(f"🔗 Llamando al nodo {node_id} con datos: {data}")
        
        # Para compatibilidad con node_routes existente
        if data.get('action') == 'status':
            estado = obtener_estado_servidor()
            return {
                'success': True,
                'node_id': node_id,
                'status': 'connected' if estado['success'] else 'disconnected',
                'details': estado.get('detalles', {})
            }
        else:
            return {
                'success': False,
                'error': f'Acción no soportada: {data.get("action")}'
            }
            
    except Exception as e:
        return {
            'success': False,
            'error': str(e),
            'node_id': node_id
        }

def probar_conexion_pyro() -> Dict[str, Any]:
    """Función para probar la conexión Pyro desde el servidor Flask"""
    try:
        procesador = conectar_servidor_imagenes()
        
        # Probar todos los métodos disponibles
        saludo = procesador.saludar()
        estado = procesador.obtener_estado_servidor()
        status = procesador.get_status()
        
        return {
            'success': True,
            'saludo': saludo,
            'estado_servidor': estado,
            'status_procesador': status,
            'message': 'Conexión Pyro funcionando correctamente'
        }
    except Exception as e:
        return {
            'success': False,
            'error': str(e),
            'message': 'Error en conexión Pyro'
        }