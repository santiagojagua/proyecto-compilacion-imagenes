import cv2
import numpy as np
from typing import Dict
import Pyro4

@Pyro4.expose
class TransformacionesCV2:
    """Clase con transformaciones de imágenes usando OpenCV"""
    
    @Pyro4.expose
    def aplicar_transformacion_cv2(self, imagen, transformacion: Dict):
        """Aplica una transformación específica a la imagen usando OpenCV"""
        tipo = transformacion.get('tipo', '')
        parametros = transformacion.get('parametros', {})
        
        try:
            if tipo == 'redimensionar':
                ancho = parametros.get('ancho', 800)
                alto = parametros.get('alto', 600)
                return cv2.resize(imagen, (ancho, alto), interpolation=cv2.INTER_LANCZOS4)
            
            elif tipo == 'escala_grises':
                if len(imagen.shape) == 3:  # Solo convertir si es color
                    return cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
                return imagen
            
            elif tipo == 'rotar':
                angulo = parametros.get('angulo', 90)
                return self.rotar_imagen(imagen, angulo)
            
            elif tipo == 'recortar':
                x = parametros.get('x', 0)
                y = parametros.get('y', 0)
                ancho = parametros.get('ancho', imagen.shape[1])
                alto = parametros.get('alto', imagen.shape[0])
                return imagen[y:y+alto, x:x+ancho]
            
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
                if radio < 1: radio = 1
                if radio % 2 == 0: radio += 1
                return cv2.GaussianBlur(imagen, (radio, radio), 0)
            
            # Agregar más transformaciones según sea necesario
            return imagen
            
        except Exception as e:
            # Usar el logger del padre si está disponible
            if hasattr(self, 'logger'):
                self.logger.warning(f"Error aplicando transformación {tipo}: {e}")
            return imagen

    @Pyro4.expose
    def rotar_imagen(self, imagen, angulo: float):
        """Rota una imagen según el ángulo especificado"""
        h, w = imagen.shape[:2]
        centro = (w // 2, h // 2)
        
        if angulo in [90, 180, 270]:
            # Rotaciones de 90 grados (más eficientes)
            if angulo == 90:
                return cv2.rotate(imagen, cv2.ROTATE_90_CLOCKWISE)
            elif angulo == 180:
                return cv2.rotate(imagen, cv2.ROTATE_180)
            elif angulo == 270:
                return cv2.rotate(imagen, cv2.ROTATE_90_COUNTERCLOCKWISE)
        else:
            # Rotación personalizada
            matriz = cv2.getRotationMatrix2D(centro, angulo, 1.0)
            return cv2.warpAffine(imagen, matriz, (w, h))