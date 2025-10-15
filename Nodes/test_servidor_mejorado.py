import Pyro4
import base64
from PIL import Image
import io

# Conectar al servidor
uri = "PYRO:procesador.imagenes@localhost:9090"
procesador = Pyro4.Proxy(uri)

def test_procesamiento_con_guardado():
    """Probar el procesamiento con guardado automático"""
    
    # Crear una imagen de prueba simple
    imagen_prueba = Image.new('RGB', (100, 100), color='red')
    buffer = io.BytesIO()
    imagen_prueba.save(buffer, format='PNG')
    imagen_b64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
    
    # Preparar datos de prueba
    imagenes = [{
        'nombre': 'imagen_prueba.png',
        'imagen': imagen_b64,
        'transformaciones': [
            {
                'tipo': 'escala_grises',
                'parametros': {}
            },
            {
                'tipo': 'redimensionar',
                'parametros': {'ancho': 200, 'alto': 150}
            },
            {
                'tipo': 'marca_agua',
                'parametros': {'texto': 'PROCESADO', 'posicion': 'centro'}
            }
        ]
    }]
    
    print("Enviando imagen para procesamiento...")
    resultado = procesador.procesar_imagenes(imagenes)
    
    print(f"Resultado: {resultado['estado']}")
    print(f"Total procesadas: {resultado['total_procesadas']}")
    
    for nombre, img_result in resultado['resultados'].items():
        print(f"\nImagen: {nombre}")
        print(f"Estado: {img_result['estado']}")
        if img_result['estado'] == 'completado':
            print(f"Archivo guardado: {img_result['archivo_guardado']}")
            print(f"Ruta: {img_result['ruta_completa']}")
            print(f"Tamaño base64: {img_result['tamaño_bytes']} bytes")
            print(f"Transformaciones aplicadas: {len(img_result['transformaciones_aplicadas'])}")

def test_estado_servidor():
    """Probar el estado mejorado del servidor"""
    estado = procesador.obtener_estado_servidor()
    print("\n=== ESTADO DEL SERVIDOR ===")
    print(f"Estado: {estado['estado']}")
    print(f"Carpeta salida: {estado['carpeta_salida']}")
    print(f"Archivos procesados: {estado['archivos_procesados']}")
    print(f"Hilos activos: {estado['hilos']['tareas_activas']}")

def test_listar_archivos():
    """Probar listado de archivos procesados"""
    archivos = procesador.listar_archivos_procesados()
    print("\n=== ARCHIVOS PROCESADOS ===")
    for archivo in archivos:
        print(f"Nombre: {archivo['nombre']}")
        print(f"Tamaño: {archivo['tamaño_bytes']} bytes")
        print(f"Modificado: {archivo['fecha_modificacion']}")
        print("---")

if __name__ == "__main__":
    try:
        # Probar conexión
        saludo = procesador.saludar()
        print(f"✅ {saludo}\n")
        
        # Ejecutar tests
        test_procesamiento_con_guardado()
        test_estado_servidor()
        test_listar_archivos()
        
    except Exception as e:
        print(f"❌ Error: {e}")