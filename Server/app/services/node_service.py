import requests

def get_saludo_from_node():
    """Llama al nodo REST y devuelve el resultado como string"""
    try:
        resp = requests.get("http://localhost:8080/api/saludo")
        resp.raise_for_status()
        data = resp.json()

        return f"Mensaje: {data['mensaje']}, Servidor: {data['servidor']}, Timestamp: {data['timestamp']}"

    except Exception as e:
        return f"Error al comunicarse con el nodo: {str(e)}"
