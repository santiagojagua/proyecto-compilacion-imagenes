import requests
import base64
from soap_utils import generate_soap_request

def test_soap_service():
    """Probar el servicio SOAP con datos de ejemplo"""
    
    # Datos de prueba
    imagenes_ejemplo = [
        {
            'nombre': 'imagen1.jpg',
            'tipo': 'jpg',
            'contenido_base64': 'VGVzdEJhc2U2NERhdGE=',  # "TestBase64Data" en base64
            'cambios': [
                {
                    'nombre': 'escala_grises',
                    'especificaciones': ''
                },
                {
                    'nombre': 'redimensionar', 
                    'especificaciones': 'ancho=800,alto=600'
                }
            ]
        },
        {
            'nombre': 'imagen2.png',
            'tipo': 'png', 
            'contenido_base64': 'T3RyYURhdG9zQmFzZTY0',
            'cambios': [
                {
                    'nombre': 'rotar',
                    'especificaciones': 'angulo=90'
                }
            ]
        }
    ]
    
    # Generar XML SOAP
    soap_xml = generate_soap_request(1, imagenes_ejemplo)
    
    print("XML SOAP generado:")
    print(soap_xml)
    print("\n" + "="*50 + "\n")
    
    # Enviar petición
    url = "http://localhost:5000/client/soap"
    headers = {
        'Content-Type': 'text/xml; charset=utf-8',
        'SOAPAction': 'procesar_imagen_cambios'
    }
    
    print("Enviando petición SOAP...")
    response = requests.post(url, data=soap_xml, headers=headers)
    
    print(f"Status Code: {response.status_code}")
    print("Respuesta:")
    print(response.text)

if __name__ == "__main__":
    test_soap_service()