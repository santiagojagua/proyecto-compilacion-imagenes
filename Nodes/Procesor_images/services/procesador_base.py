from queue import Queue
import Pyro4
from typing import List, Dict, Any
from datetime import datetime

from ..core.config import Config
from ..utils.logger import setup_logging

@Pyro4.expose
class ProcesadorBase:
    """Clase base con funcionalidades comunes para el procesamiento de imágenes"""
    
    def __init__(self, max_memory_usage=Config.MAX_MEMORY_USAGE):
        self.max_memory_usage = max_memory_usage
        self.setup_logging()
        self.processing_queue = Queue()
        self.is_processing = False
        self.gestor_hilos = None
        
    def setup_logging(self):
        """Configura el sistema de logging"""
        self.logger = setup_logging(self.__class__.__name__)
    
    def set_gestor_hilos(self, gestor):
        """Establece el gestor de hilos para procesamiento paralelo"""
        self.gestor_hilos = gestor
    
    # ========== MÉTODOS DE ESTADO Y MONITOREO ==========
    
    @Pyro4.expose
    def saludar(self) -> str:
        """Método de prueba para verificar conexión"""
        return f"Procesador de Imágenes activo - {datetime.now().isoformat()}"
    
    @Pyro4.expose
    def obtener_estado_servidor(self) -> Dict[str, Any]:
        """Retorna el estado del servidor"""
        return {
            'estado': 'activo',
            'memoria_usage': self.check_memory_usage(),
            'timestamp': datetime.now().isoformat(),
            'procesando': self.is_processing,
            'max_memory_usage': self.max_memory_usage
        }
    
    @Pyro4.expose
    def get_status(self) -> Dict[str, Any]:
        """Retorna el estado actual del procesador"""
        return {
            "is_processing": self.is_processing,
            "memory_usage": self.check_memory_usage(),
            "queue_size": self.processing_queue.qsize(),
            "max_memory_usage": self.max_memory_usage
        }
    
    # ========== MÉTODOS DE UTILIDAD ==========
    
    def _crear_respuesta_error(self, mensaje: str, total_imagenes: int) -> Dict:
        """Crea una respuesta de error estandarizada"""
        return {
            'status': 'error',
            'message': mensaje,
            'total_procesadas': 0,
            'exitosas': 0,
            'fallidas': total_imagenes
        }

    def _crear_respuesta_error_con_usuario(self, mensaje: str, usuario_id: int, usuario_nombre: str, total_imagenes: int) -> Dict:
        """Crea una respuesta de error con información de usuario"""
        respuesta = self._crear_respuesta_error(mensaje, total_imagenes)
        respuesta['usuario_id'] = usuario_id
        respuesta['usuario_nombre'] = usuario_nombre
        return respuesta

    def _preparar_imagenes_para_procesamiento(self, imagenes: List[Dict]) -> List[Dict]:
        from ..utils.helpers import parsear_especificaciones
        imagenes_procesar = []
        for img in imagenes:
            imagen_data = {
                'nombre': img.get('nombre', 'sin_nombre'),
                'imagen': img.get('contenido_base64', '') or img.get('imagen', ''),
                'transformaciones': []
            }
            items = img.get('cambios')
            if items is None:
                items = img.get('transformaciones', [])
            for item in items:
                if 'tipo' in item or 'parametros' in item:
                    transformacion = {
                        'tipo': item.get('tipo') or item.get('nombre', ''),
                        'parametros': item.get('parametros', {})
                    }
                else:
                    transformacion = {
                        'tipo': item.get('nombre', ''),
                        'parametros': parsear_especificaciones(item.get('especificaciones', ''))
                    }
                imagen_data['transformaciones'].append(transformacion)
            imagenes_procesar.append(imagen_data)
        return imagenes_procesar