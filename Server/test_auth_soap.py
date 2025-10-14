import requests
from soap_utils import generate_login_soap, generate_register_soap, generate_soap_request

def test_registro():
    """Probar registro de usuario"""
    soap_xml = generate_register_soap("usuario_prueba", "password123")
    
    print("📝 Probando registro de usuario...")
    response = send_soap_request(soap_xml, "registrar_usuario")
    print("Respuesta:", response.text)

def test_login():
    """Probar inicio de sesión"""
    soap_xml = generate_login_soap("usuario_prueba", "password123")
    
    print("🔐 Probando login...")
    response = send_soap_request(soap_xml, "login")
    print("Respuesta:", response.text)
    
    # Extraer user_id de la respuesta para usar en otras pruebas
    return response.text

def test_procesamiento_con_auth():
    """Probar procesamiento después de autenticación"""
    # Primero hacer login para obtener user_id
    soap_login = generate_login_soap("usuario_prueba", "password123")
    response_login = send_soap_request(soap_login, "login")
    
    # Aquí necesitarías parsear la respuesta para obtener el user_id
    # Por ahora usamos un ID fijo para la prueba
    user_id = 1
    
    imagenes_ejemplo = [
        {
            'nombre': 'imagen_test.jpg',
            'tipo': 'jpg',
            'contenido_base64': 'VGVzdEJhc2U2NERhdGE=',
            'cambios': [
                {
                    'nombre': 'escala_grises',
                    'especificaciones': ''
                }
            ]
        }
    ]
    
    soap_xml = generate_soap_request(user_id, imagenes_ejemplo)
    
    print("🖼️ Probando procesamiento con autenticación...")
    response = send_soap_request(soap_xml, "procesar_imagen_cambios")
    print("Respuesta:", response.text)

def send_soap_request(soap_xml, action):
    """Envía una petición SOAP"""
    url = "http://localhost:5000/client/soap"
    headers = {
        'Content-Type': 'text/xml; charset=utf-8',
        'SOAPAction': action,
    }
    
    return requests.post(url, data=soap_xml, headers=headers)

if __name__ == "__main__":
    print("🧪 Probando sistema de autenticación SOAP...\n")
    
    test_registro()
    print("\n" + "="*50 + "\n")
    
    test_login()
    print("\n" + "="*50 + "\n")
    
    test_procesamiento_con_auth()