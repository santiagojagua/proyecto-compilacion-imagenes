import requests
import json

def get_saludo_from_node():
    """Llama al nodo REST y devuelve el resultado como string"""
    try:
        resp = requests.get("http://localhost:8080/api/saludo")
        resp.raise_for_status()
        data = resp.json()

        return f"Mensaje: {data['mensaje']}, Servidor: {data['servidor']}, Timestamp: {data['timestamp']}"

    except Exception as e:
        return f"Error al comunicarse con el nodo: {str(e)}"
    
def enviar_json_imagenes(json_rpc: dict):

    """
    Envía el JSON RPC construido al nodo.
    """
    url = "http://localhost:8080/api/rpc"

    try:
        # Convertir dict a JSON string
        payload = json.dumps(json_rpc)

        headers = {
            "Content-Type": "application/json"
        }

        # POST con el JSON RPC
        resp = requests.post(url, data=payload, headers=headers)
        resp.raise_for_status()  # Lanza excepción si no es 2xx

        # Obtener respuesta como dict
        data = resp.json()

        # Dependiendo de la respuesta del nodo
        mensaje = data.get("mensaje") or data.get("result") or str(data)
        servidor = data.get("servidor", "desconocido")
        timestamp = data.get("timestamp", "desconocido")

        return f"Mensaje: {mensaje}, Servidor: {servidor}, Timestamp: {timestamp}"

    except Exception as e:
        print(e)
        return f"Error al comunicarse con el nodo: {str(e)}"
