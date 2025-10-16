# test_pyro_connection.py
import Pyro4
import sys
import os

# Añadir el directorio actual al path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

def test_pyro_connection():
    try:
        print("🔍 Probando conexión con servidor Pyro4...")
        
        # Intentar conectar
        uri = "PYRO:procesador.imagenes@localhost:9090"
        procesador = Pyro4.Proxy(uri)
        
        # Probar métodos básicos
        saludo = procesador.saludar()
        print(f"✅ Conexión exitosa: {saludo}")
        
        # Probar estado
        estado = procesador.obtener_estado_servidor()
        print(f"✅ Estado del servidor: {estado}")
        
        # Probar con una imagen simple
        from PIL import Image, ImageDraw
        import io
        import base64
        
        # Crear imagen de prueba
        img = Image.new('RGB', (100, 100), color='red')
        buffer = io.BytesIO()
        img.save(buffer, format='PNG')
        imagen_b64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
        
        # Preparar datos de prueba
        imagenes_prueba = [{
            'nombre': 'test_connection.png',
            'imagen': imagen_b64,
            'transformaciones': [
                {
                    'tipo': 'escala_grises',
                    'parametros': {}
                }
            ]
        }]
        
        print("🔄 Probando procesamiento de imagen...")
        resultado = procesador.procesar_imagenes(imagenes_prueba)
        print(f"✅ Procesamiento exitoso: {resultado}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error conectando a Pyro4: {e}")
        import traceback
        print("Detalles completos del error:")
        print(traceback.format_exc())
        return False

if __name__ == "__main__":
    test_pyro_connection()