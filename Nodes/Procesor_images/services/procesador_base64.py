import cv2
import numpy as np
import base64
import time
import psutil
from typing import List, Dict, Any
import Pyro4

from .procesador_base import ProcesadorBase
from .procesador_transformaciones import TransformacionesCV2

@Pyro4.expose
class ProcesadorBase64(ProcesadorBase, TransformacionesCV2):
    """Clase especializada en procesamiento de imágenes en base64"""
    
    def __init__(self, max_memory_usage=0.8):
        super().__init__(max_memory_usage)
    
    # ========== MÉTODOS PRINCIPALES PARA BASE64 ==========
    
    @Pyro4.expose
    def procesar_imagenes(self, imagenes: List[Dict]) -> Dict[str, Any]:
        """Procesa múltiples imágenes en base64 con transformaciones"""
        try:
            self.logger.info(f"Procesando {len(imagenes)} imágenes desde base64")
            resultados = []
            procesadas_exitosas = 0
            
            for img_data in imagenes:
                resultado = self.procesar_imagen_base64_individual(
                    img_data.get('imagen', ''),
                    img_data.get('nombre', 'sin_nombre'),
                    img_data.get('transformaciones', [])
                )
                
                if resultado.get('status') == 'success':
                    procesadas_exitosas += 1
                
                resultados.append(resultado)
            
            return {
                'status': 'completed',
                'total_procesadas': len(imagenes),
                'exitosas': procesadas_exitosas,
                'fallidas': len(imagenes) - procesadas_exitosas,
                'resultados': resultados,
                'timestamp': self._get_timestamp()
            }
            
        except Exception as e:
            self.logger.error(f"Error en procesar_imagenes: {e}")
            return self._crear_respuesta_error(str(e), len(imagenes))
    
    @Pyro4.expose
    def procesar_imagen_cambios(self, usuario_id: int, usuario_nombre: str, imagenes: List[Dict]) -> Dict[str, Any]:
        """Procesa imágenes con información de usuario (compatibilidad SOAP)"""
        try:
            self.logger.info(f"📤 Procesando {len(imagenes)} imágenes para usuario {usuario_id} ({usuario_nombre})")
            
            imagenes_procesar = self._preparar_imagenes_para_procesamiento(imagenes)
            resultado = self.procesar_imagenes(imagenes_procesar)
            
            # Agregar información del usuario al resultado
            resultado['usuario_id'] = usuario_id
            resultado['usuario_nombre'] = usuario_nombre
            
            self.logger.info(f"✅ Procesamiento completado para usuario {usuario_id}")
            return resultado
            
        except Exception as e:
            self.logger.error(f"❌ Error en procesar_imagen_cambios: {e}")
            return self._crear_respuesta_error_con_usuario(str(e), usuario_id, usuario_nombre, len(imagenes))

    @Pyro4.expose
    def procesar_imagen_base64(self, imagen_base64: str, nombre_archivo: str, transformaciones: List[Dict] = None) -> Dict:
        """Procesa una sola imagen base64"""
        return self.procesar_imagen_base64_individual(imagen_base64, nombre_archivo, transformaciones or [])

    @Pyro4.expose
    def procesar_imagen_base64_individual(self, imagen_base64: str, nombre: str, transformaciones: List[Dict]) -> Dict:
        """Procesa una imagen individual en base64"""
        try:
            # Verificar memoria antes de procesar
            if self.check_memory_usage() > self.max_memory_usage * 100:
                self.logger.warning("Alto uso de memoria, esperando...")
                time.sleep(2)
            
            # Decodificar base64
            img = self._decodificar_base64(imagen_base64)
            if img is None:
                return {'status': 'error', 'message': 'No se pudo decodificar la imagen base64'}
            
            self.logger.info(f"Procesando base64: {nombre} - {img.shape[1]}x{img.shape[0]}")
            
            # Aplicar transformaciones
            img_procesada, transformaciones_aplicadas = self._aplicar_transformaciones(img, transformaciones)
            
            # Codificar resultado a base64
            imagen_procesada_base64 = self._codificar_a_base64(img_procesada)
            
            return {
                'status': 'success',
                'nombre': nombre,
                'imagen_procesada': imagen_procesada_base64,
                'dimensiones_originales': f"{img.shape[1]}x{img.shape[0]}",
                'dimensiones_procesadas': f"{img_procesada.shape[1]}x{img_procesada.shape[0]}",
                'transformaciones_aplicadas': transformaciones_aplicadas,
                'tamaño_original_bytes': len(imagen_base64),
                'tamaño_procesado_bytes': len(imagen_procesada_base64)
            }
            
        except Exception as e:
            self.logger.error(f"Error procesando {nombre}: {e}")
            return {'status': 'error', 'message': str(e), 'nombre': nombre}

    @Pyro4.expose
    def procesar_imagen_directo(self, imagen_base64: str, transformaciones: List[Dict] = None) -> Dict:
        """Procesa una imagen base64 directamente sin estructura compleja"""
        try:
            self.logger.info(f"Procesando imagen directa - Base64: {len(imagen_base64)} chars")
            
            # Verificar memoria antes de procesar
            if self.check_memory_usage() > self.max_memory_usage * 100:
                self.logger.warning("Alto uso de memoria, esperando...")
                time.sleep(2)
            
            # Decodificar base64
            img = self._decodificar_base64(imagen_base64)
            if img is None:
                return {'status': 'error', 'message': 'No se pudo decodificar la imagen base64'}
            
            self.logger.info(f"Procesando imagen directa - {img.shape[1]}x{img.shape[0]}")
            
            # Aplicar transformaciones
            img_procesada, transformaciones_aplicadas = self._aplicar_transformaciones(img, transformaciones or [])
            
            # Codificar resultado a base64
            imagen_procesada_base64 = self._codificar_a_base64(img_procesada)
            
            return {
                'status': 'success',
                'nombre': "imagen_directa",
                'imagen_procesada': imagen_procesada_base64,
                'dimensiones_originales': f"{img.shape[1]}x{img.shape[0]}",
                'dimensiones_procesadas': f"{img_procesada.shape[1]}x{img_procesada.shape[0]}",
                'transformaciones_aplicadas': transformaciones_aplicadas,
                'tamaño_original_bytes': len(imagen_base64),
                'tamaño_procesado_bytes': len(imagen_procesada_base64)
            }
            
        except Exception as e:
            self.logger.error(f"Error procesando imagen directa: {e}")
            return {'status': 'error', 'message': str(e), 'nombre': "imagen_directa"}

    # ========== MÉTODOS AUXILIARES BASE64 ==========

    def _decodificar_base64(self, imagen_base64: str):
        try:
            if imagen_base64.startswith("data:"):
                imagen_base64 = imagen_base64.split(",", 1)[-1]
            imagen_data = base64.b64decode(imagen_base64)
            nparr = np.frombuffer(imagen_data, np.uint8)
            return cv2.imdecode(nparr, cv2.IMREAD_COLOR)
        except Exception as e:
            self.logger.error(f"Error decodificando base64: {e}")
            return None
        
    def _codificar_a_base64(self, imagen) -> str:
        """Codifica una imagen numpy array a base64"""
        try:
            _, buffer = cv2.imencode('.jpg', imagen, [cv2.IMWRITE_JPEG_QUALITY, 95])
            return base64.b64encode(buffer).decode('utf-8')
        except Exception as e:
            self.logger.error(f"Error codificando a base64: {e}")
            return ""

    def _aplicar_transformaciones(self, imagen, transformaciones: List[Dict]):
        """Aplica una lista de transformaciones a una imagen"""
        img_procesada = imagen.copy()
        transformaciones_aplicadas = []
        
        for transformacion in transformaciones:
            img_anterior = img_procesada.copy()
            img_procesada = self.aplicar_transformacion_cv2(img_procesada, transformacion)
            
            # Verificar si la transformación se aplicó correctamente
            if not np.array_equal(img_anterior, img_procesada):
                transformaciones_aplicadas.append(transformacion.get('tipo', 'desconocido'))
        
        return img_procesada, transformaciones_aplicadas

    def _get_timestamp(self):
        """Retorna el timestamp actual"""
        from datetime import datetime
        return datetime.now().isoformat()

    @Pyro4.expose
    def check_memory_usage(self):
        """Verifica el uso actual de memoria"""
        memory_info = psutil.virtual_memory()
        return memory_info.percent