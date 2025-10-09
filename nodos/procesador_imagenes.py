import Pyro4
from gestor_hilos import GestorHilos
from transformaciones import TransformacionesImagen
from PIL import Image
import io
import base64
import time
from typing import List, Dict, Any

@Pyro4.expose
class ProcesadorImagenes:
    def __init__(self, max_workers: int = 5):
        self.gestor_hilos = GestorHilos(max_workers)
        self.transformaciones = TransformacionesImagen()
        
    def procesar_imagenes(self, lista_imagenes: List[Dict]) -> Dict[str, Any]:
        """
        Procesa una lista de imágenes con transformaciones en paralelo
        Cada imagen puede tener hasta 5 transformaciones
        """
        print(f"Recibido lote de {len(lista_imagenes)} imágenes para procesar")
        
        # Preparar tareas para procesamiento paralelo
        tareas = []
        for img_data in lista_imagenes:
            tarea = {
                'nombre': img_data['nombre'],
                'funcion': self._procesar_imagen_individual,
                'args': (img_data,),
                'kwargs': {}
            }
            tareas.append(tarea)
        
        # Procesar en paralelo
        resultados = self.gestor_hilos.procesar_paralelo(tareas)
        
        return {
            'total_procesadas': len(resultados),
            'resultados': resultados,
            'timestamp': time.time(),
            'estado': 'completado'
        }
    
    def _procesar_imagen_individual(self, img_data: Dict) -> Dict[str, Any]:
        """Procesa una imagen individual con sus transformaciones"""
        nombre = img_data['nombre']
        imagen_b64 = img_data['imagen']
        transformaciones = img_data.get('transformaciones', [])
        
        # Limitar a máximo 5 transformaciones
        if len(transformaciones) > 5:
            transformaciones = transformaciones[:5]
            print(f"Advertencia: Se limitaron las transformaciones a 5 para {nombre}")
        
        try:
            # Decodificar imagen base64
            imagen_bytes = base64.b64decode(imagen_b64)
            imagen = Image.open(io.BytesIO(imagen_bytes))
            
            formato_original = imagen.format
            resultados_transformaciones = []
            
            print(f"Procesando: {nombre} con {len(transformaciones)} transformaciones")
            
            # Aplicar cada transformación en secuencia
            for i, transformacion in enumerate(transformaciones):
                tipo = transformacion['tipo']
                parametros = transformacion.get('parametros', {})
                
                try:
                    imagen = self._aplicar_transformacion(imagen, tipo, parametros)
                    resultados_transformaciones.append({
                        'transformacion': tipo,
                        'numero': i + 1,
                        'estado': 'completada',
                        'parametros': parametros
                    })
                except Exception as e:
                    resultados_transformaciones.append({
                        'transformacion': tipo,
                        'numero': i + 1,
                        'estado': 'error',
                        'error': str(e),
                        'parametros': parametros
                    })
                    # Continuar con las siguientes transformaciones
                    continue
            
            # Convertir a base64 para retornar
            buffer = io.BytesIO()
            
            # Determinar formato de salida
            formato_salida = 'PNG'  # Por defecto
            for transformacion in reversed(transformaciones):
                if transformacion['tipo'] == 'convertir_formato':
                    formato_salida = transformacion['parametros'].get('formato', 'PNG')
                    break
            
            if formato_salida.upper() == 'JPG':
                formato_salida = 'JPEG'
            
            imagen.save(buffer, format=formato_salida)
            imagen_procesada_b64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
            
            return {
                'estado': 'completado',
                'transformaciones_aplicadas': resultados_transformaciones,
                'imagen_procesada': imagen_procesada_b64,
                'formato_salida': formato_salida,
                'dimensiones_finales': imagen.size,
                'tamaño_bytes': len(imagen_procesada_b64)
            }
            
        except Exception as e:
            return {
                'estado': 'error',
                'error': f"Error procesando imagen {nombre}: {str(e)}"
            }
    
    def _aplicar_transformacion(self, imagen: Image.Image, tipo: str, parametros: Dict) -> Image.Image:
        """Aplica una transformación específica usando la clase Transformaciones"""
        
        metodos = {
            'escala_grises': self.transformaciones.aplicar_escala_grises,
            'redimensionar': self.transformaciones.aplicar_redimensionar,
            'recortar': self.transformaciones.aplicar_recortar,
            'rotar': self.transformaciones.aplicar_rotar,
            'reflejar': self.transformaciones.aplicar_reflejar,
            'desenfocar': self.transformaciones.aplicar_desenfocar,
            'perfilar': self.transformaciones.aplicar_perfilar,
            'ajustar_brillo_contraste': self.transformaciones.aplicar_ajustar_brillo_contraste,
            'marca_agua': self.transformaciones.aplicar_marca_agua,
            'convertir_formato': self.transformaciones.aplicar_convertir_formato
        }
        
        if tipo in metodos:
            return metodos[tipo](imagen, parametros)
        else:
            raise ValueError(f"Tipo de transformación no soportada: {tipo}")
    
    def obtener_estado_servidor(self) -> Dict[str, Any]:
        """Retorna el estado actual del servidor"""
        estado_hilos = self.gestor_hilos.obtener_estado()
        
        return {
            'estado': 'activo',
            'hilos': estado_hilos,
            'transformaciones_disponibles': self.transformaciones.obtener_transformaciones_disponibles(),
            'timestamp': time.time()
        }
    
    def saludar(self) -> str:
        """Método simple para probar la conexión"""
        return "Servidor de procesamiento de imágenes activo y listo"