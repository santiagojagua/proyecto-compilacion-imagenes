import Pyro4
import base64
from typing import List, Dict, Any

# Configuración de conexión Pyro4
PYRO_URI = "PYRO:procesador.imagenes@localhost:9090"

def conectar_servidor_imagenes():
    """Conecta al servidor Pyro4 de procesamiento de imágenes"""
    try:
        procesador = Pyro4.Proxy(PYRO_URI)
        # Probar conexión
        procesador.saludar()
        return procesador
    except Exception as e:
        raise ConnectionError(f"No se pudo conectar al servidor Pyro4: {e}")

def procesar_imagenes_pyro(lista_imagenes: List[Dict]) -> Dict[str, Any]:
    """
    Envía las imágenes al servidor Pyro4 para procesamiento
    """
    try:
        # Conectar al servidor
        procesador = conectar_servidor_imagenes()
        
        # Preparar datos para Pyro4
        imagenes_pyro = []
        
        for img_data in lista_imagenes:
            # Convertir cambios a formato Pyro4
            transformaciones = []
            for cambio in img_data.get('cambios', []):
                transformacion = {
                    'tipo': cambio['nombre'],
                    'parametros': _parsear_especificaciones(cambio['especificaciones'])
                }
                transformaciones.append(transformacion)
            
            imagen_pyro = {
                'nombre': img_data['nombre'],
                'imagen': img_data['contenido_base64'],
                'transformaciones': transformaciones
            }
            imagenes_pyro.append(imagen_pyro)
        
        print(f"Enviando {len(imagenes_pyro)} imágenes a Pyro4...")
        
        # Enviar al servidor Pyro4
        resultado = procesador.procesar_imagenes(imagenes_pyro)
        
        print(f"Resultado de Pyro4: {resultado}")
        
        return {
            'success': True,
            'resultado': resultado,
            'mensaje': f'Procesadas {resultado.get("total_procesadas", 0)} imágenes correctamente'
        }
        
    except Exception as e:
        import traceback
        print(f"Error en procesar_imagenes_pyro: {traceback.format_exc()}")
        return {
            'success': False,
            'error': str(e),
            'mensaje': 'Error en el procesamiento de imágenes'
        }

def _parsear_especificaciones(especificaciones: str) -> Dict:
    """
    Convierte las especificaciones de string a diccionario
    Ejemplo: "ancho=800,alto=600" -> {'ancho': 800, 'alto': 600}
    """
    parametros = {}
    if especificaciones and especificaciones.strip():
        partes = especificaciones.split(',')
        for parte in partes:
            if '=' in parte:
                key, value = parte.split('=', 1)
                key = key.strip()
                value = value.strip()
                
                # Convertir a número si es posible
                try:
                    if '.' in value:
                        value = float(value)
                    else:
                        value = int(value)
                except ValueError:
                    # Mantener como string si no se puede convertir
                    pass
                
                parametros[key] = value
    
    return parametros

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