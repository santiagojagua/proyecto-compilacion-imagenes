import Pyro4
from Procesor_images.gestor_hilos import GestorHilos
from Procesor_images.transformaciones import TransformacionesImagen
from PIL import Image
import io
import base64
import time
import os
from typing import List, Dict, Any

@Pyro4.expose
class ProcesadorImagenes:
    def __init__(self, max_workers: int = 5, carpeta_salida: str = "images"):
        self.gestor_hilos = GestorHilos(max_workers)
        self.transformaciones = TransformacionesImagen()
        self.carpeta_salida = carpeta_salida
        
        # Crear carpeta de salida si no existe
        if not os.path.exists(self.carpeta_salida):
            os.makedirs(self.carpeta_salida)
        
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
            'estado': 'completado',
            'carpeta_salida': self.carpeta_salida
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
            
            # Determinar formato de salida
            formato_salida = 'PNG'  # Por defecto
            for transformacion in reversed(transformaciones):
                if transformacion['tipo'] == 'convertir_formato':
                    formato_salida = transformacion['parametros'].get('formato', 'PNG')
                    break
            
            if formato_salida.upper() == 'JPG':
                formato_salida = 'JPEG'
            
            # Convertir a base64 para retornar
            buffer = io.BytesIO()
            imagen.save(buffer, format=formato_salida)
            imagen_procesada_b64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
            
            # Guardar imagen en archivo
            nombre_archivo = self._guardar_imagen(imagen, nombre, formato_salida)
            
            return {
                'estado': 'completado',
                'transformaciones_aplicadas': resultados_transformaciones,
                'imagen_procesada': imagen_procesada_b64,
                'formato_salida': formato_salida,
                'dimensiones_finales': imagen.size,
                'tamaño_bytes': len(imagen_procesada_b64),
                'archivo_guardado': nombre_archivo,
                'ruta_completa': os.path.join(self.carpeta_salida, nombre_archivo)
            }
            
        except Exception as e:
            return {
                'estado': 'error',
                'error': f"Error procesando imagen {nombre}: {str(e)}"
            }
    
    def _guardar_imagen(self, imagen: Image.Image, nombre_original: str, formato: str) -> str:
        """Guarda la imagen procesada en la carpeta de salida"""
        # Crear nombre de archivo único con timestamp
        nombre_base = os.path.splitext(nombre_original)[0]
        timestamp = int(time.time())
        
        # Determinar extensión
        if formato.upper() == 'JPEG':
            extension = 'jpg'
        else:
            extension = formato.lower()
        
        nombre_archivo = f"{nombre_base}_{timestamp}.{extension}"
        ruta_completa = os.path.join(self.carpeta_salida, nombre_archivo)
        
        # Guardar imagen
        imagen.save(ruta_completa, format=formato)
        print(f"✓ Imagen guardada: {ruta_completa}")
        
        return nombre_archivo
    
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
        
        # Verificar si la carpeta de salida existe y contar archivos
        cantidad_archivos = 0
        if os.path.exists(self.carpeta_salida):
            cantidad_archivos = len([f for f in os.listdir(self.carpeta_salida) 
                                   if os.path.isfile(os.path.join(self.carpeta_salida, f))])
        
        return {
            'estado': 'activo',
            'hilos': estado_hilos,
            'carpeta_salida': self.carpeta_salida,
            'archivos_procesados': cantidad_archivos,
            'transformaciones_disponibles': self.transformaciones.obtener_transformaciones_disponibles(),
            'timestamp': time.time()
        }
    
    def listar_archivos_procesados(self) -> List[Dict[str, Any]]:
        """Lista todos los archivos procesados en la carpeta de salida"""
        archivos = []
        if os.path.exists(self.carpeta_salida):
            for archivo in os.listdir(self.carpeta_salida):
                ruta_completa = os.path.join(self.carpeta_salida, archivo)
                if os.path.isfile(ruta_completa):
                    stat = os.stat(ruta_completa)
                    archivos.append({
                        'nombre': archivo,
                        'ruta': ruta_completa,
                        'tamaño_bytes': stat.st_size,
                        'fecha_modificacion': time.ctime(stat.st_mtime)
                    })
        
        return archivos
    
    def obtener_imagen_como_base64(self, nombre_archivo: str) -> Dict[str, Any]:
        """Obtiene una imagen procesada específica como base64"""
        try:
            ruta_archivo = os.path.join(self.carpeta_salida, nombre_archivo)
            if not os.path.exists(ruta_archivo):
                return {'error': f'Archivo no encontrado: {nombre_archivo}'}
            
            with open(ruta_archivo, 'rb') as f:
                imagen_bytes = f.read()
                imagen_b64 = base64.b64encode(imagen_bytes).decode('utf-8')
            
            return {
                'nombre_archivo': nombre_archivo,
                'imagen_base64': imagen_b64,
                'tamaño_bytes': len(imagen_b64),
                'ruta': ruta_archivo
            }
            
        except Exception as e:
            return {'error': f'Error leyendo archivo: {str(e)}'}
    
    def saludar(self) -> str:
        """Método simple para probar la conexión"""
        return "Servidor de procesamiento de imágenes activo y listo"