import requests
import base64
from soap_utils import generate_soap_request, generate_login_soap

def imagen_a_base64_simple():
    """
    Crea una imagen simple de prueba en base64
    """
    from PIL import Image, ImageDraw
    import io
    
    # Crear una imagen simple de 100x100 píxeles
    img = Image.new('RGB', (100, 100), color='red')
    draw = ImageDraw.Draw(img)
    draw.text((10, 10), "TEST IMAGE", fill='white')
    
    # Convertir a base64
    buffer = io.BytesIO()
    img.save(buffer, format='PNG')
    return base64.b64encode(buffer.getvalue()).decode('utf-8')

def test_procesamiento_con_imagen_real():
    """Probar procesamiento con imagen real en base64"""
    
    # Primero hacer login
    soap_login = generate_login_soap("usuario_prueba", "password123")
    response_login = send_soap_request(soap_login, "login")
    print("✅ Login exitoso")
    
    # Crear imagen real en base64
    imagen_base64_real = imagen_a_base64_simple()
    print(f"📷 Imagen base64 creada ({len(imagen_base64_real)} caracteres)")
    
    imagenes_ejemplo = [
        {
            'nombre': 'imagen_real_test.png',
            'tipo': 'png',
            'contenido_base64': imagen_base64_real,
            'cambios': [
                {
                    'nombre': 'escala_grises',
                    'especificaciones': ''
                },
                {
                    'nombre': 'redimensionar',
                    'especificaciones': 'ancho=200,alto=200'
                }
            ]
        }
    ]
    
    soap_xml = generate_soap_request(1, imagenes_ejemplo)
    
    print("🖼️ Enviando imagen real para procesamiento...")
    response = send_soap_request(soap_xml, "procesar_imagen_cambios")
    
    print("📋 Respuesta SOAP:")
    print(response.text)

def send_soap_request(soap_xml, action):
    """Envía una petición SOAP"""
    url = "http://localhost:5000/client/soap"
    headers = {
        'Content-Type': 'text/xml; charset=utf-8',
        'SOAPAction': action,
    }
    
    return requests.post(url, data=soap_xml, headers=headers)

if __name__ == "__main__":
    print("🧪 Probando con imagen real...\n")
    test_procesamiento_con_imagen_real()