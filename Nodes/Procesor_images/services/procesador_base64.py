import cv2
import numpy as np
import base64
import time
import psutil
from typing import List, Dict, Any
import Pyro4
from pathlib import Path

from .procesador_base import ProcesadorBase
from .procesador_transformaciones import TransformacionesCV2
from ..core.config import Config

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
        try:
            # Verificar memoria
            if self.check_memory_usage() > self.max_memory_usage * 100:
                self.logger.warning("Alto uso de memoria, esperando...")
                time.sleep(2)

            # Decodificar
            img = self._decodificar_base64(imagen_base64)
            if img is None:
                return {'status': 'error', 'message': 'No se pudo decodificar la imagen base64', 'nombre': nombre}

            self.logger.info(f"Procesando base64: {nombre} - {img.shape[1]}x{img.shape[0]}")

            # Transformar
            img_procesada, transformaciones_aplicadas = self._aplicar_transformaciones(img, transformaciones)

            # Formato de salida
            ext_out = self._resolver_formato_salida(transformaciones)

            # Codificar a base64 con ese formato
            imagen_procesada_base64 = self._codificar_a_base64(img_procesada, ext_out)

            # Guardar a disco
            abs_path, rel_path = self._guardar_imagen(img_procesada, nombre, ext_out)

            return {
                'status': 'success',
                'nombre': nombre,
                'imagen_procesada': imagen_procesada_base64,
                'dimensiones_originales': f"{img.shape[1]}x{img.shape[0]}",
                'dimensiones_procesadas': f"{img_procesada.shape[1]}x{img_procesada.shape[0]}",
                'transformaciones_aplicadas': transformaciones_aplicadas,
                'tamaño_original_bytes': len(imagen_base64),
                'tamaño_procesado_bytes': len(imagen_procesada_base64),
                'processed': rel_path,          # <- relativo: images/archivo.ext
                'processed_abs': abs_path       # <- absoluto (por si lo necesitas)
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

    def _resolver_formato_salida(self, transformaciones: List[Dict]) -> str:
        """
        Determina el formato de salida a partir de las transformaciones.
        Soporta: jpg/jpeg, png, webp, bmp. Por defecto 'jpg'.
        """
        ext = 'jpg'
        for t in transformaciones or []:
            tipo = (t.get('tipo') or t.get('nombre') or '').lower()
            if tipo == 'convertir_formato':
                fmt = (t.get('parametros', {}) or {}).get('formato') or ''
                f = str(fmt).lower()
                if f in ('jpeg', 'jpg'): ext = 'jpg'
                elif f in ('png',): ext = 'png'
                elif f in ('webp',): ext = 'webp'
                elif f in ('bmp',): ext = 'bmp'
        return ext

    def _codificar_a_base64(self, imagen, ext: str = 'jpg') -> str:
        """Codifica con el contenedor adecuado según ext."""
        try:
            ext = ext.lower()
            dot = '.' + ('jpg' if ext == 'jpeg' else ext)
            params = []
            if dot in ('.jpg', '.jpeg'):
                params = [cv2.IMWRITE_JPEG_QUALITY, 95]
            elif dot == '.png':
                params = [cv2.IMWRITE_PNG_COMPRESSION, 3]
            elif dot == '.webp':
                params = [cv2.IMWRITE_WEBP_QUALITY, 95]

            ok, buffer = cv2.imencode(dot, imagen, params)
            if not ok:
                raise RuntimeError(f"No se pudo codificar imagen como {dot}")
            return base64.b64encode(buffer).decode('utf-8')
        except Exception as e:
            self.logger.error(f"Error codificando a base64: {e}")
            return ""

    def _guardar_imagen(self, imagen, nombre_original: str, ext: str = 'jpg'):
        """Guarda la imagen en Config.IMAGES_DIR y retorna (abs_path, rel_path)."""
        try:
            if not Config.SAVE_IMAGES:
                return "", ""
            out_dir = Path(Config.IMAGES_DIR)
            out_dir.mkdir(parents=True, exist_ok=True)

            base = Path(nombre_original).stem or 'imagen'
            safe = "".join(c if c.isalnum() or c in ('-', '_') else '_' for c in base)[:80]
            ts = int(time.time() * 1000)
            ext = ext.lower()
            if ext == 'jpeg': ext = 'jpg'
            filename = f"{safe}_{ts}.{ext}"
            path = out_dir / filename

            if ext in ('jpg', 'jpeg'):
                cv2.imwrite(str(path), imagen, [cv2.IMWRITE_JPEG_QUALITY, 95])
            elif ext == 'png':
                cv2.imwrite(str(path), imagen, [cv2.IMWRITE_PNG_COMPRESSION, 3])
            elif ext == 'webp':
                cv2.imwrite(str(path), imagen, [cv2.IMWRITE_WEBP_QUALITY, 95])
            elif ext == 'bmp':
                cv2.imwrite(str(path), imagen)
            else:
                cv2.imwrite(str(path), imagen, [cv2.IMWRITE_JPEG_QUALITY, 95])

            rel = f"images/{filename}"
            return str(path), rel
        except Exception as e:
            self.logger.warning(f"No se pudo guardar imagen {nombre_original}: {e}")
            return "", ""

    @Pyro4.expose
    def check_memory_usage(self):
        """Verifica el uso actual de memoria"""
        memory_info = psutil.virtual_memory()
        return memory_info.percent