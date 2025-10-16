class CambioInput:
    def __init__(self, nombre: str, especificaciones: str):
        self.nombre = nombre
        self.especificaciones = especificaciones


class ImagenInput:
    def __init__(self, nombre: str, tipo: str, contenido_base64: str, cambios: list):
        self.nombre = nombre
        self.tipo = tipo
        self.contenido_base64 = contenido_base64
        # Mapeamos los cambios a objetos CambioInput
        self.cambios = [CambioInput(**c) for c in cambios]


class ImagenCambiosViewModel:
    def __init__(self, data: dict):
        """
        JSON con la estructura:
        {
            "user_id": 1,
            "imagenes": [
                {
                    "nombre": "imagen1.png",
                    "tipo": "png",
                    "contenido_base64": "...",
                    "cambios": [
                        {"nombre": "Cambio1", "especificaciones": "Detalles..."}
                    ]
                }
            ]
        }
        """
        self.user_id = data.get("user_id")
        self.imagenes = [ImagenInput(**img) for img in data.get("imagenes", [])]