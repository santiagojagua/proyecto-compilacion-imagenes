from typing import Dict, List, Any, TypedDict

class ImagenProcesamiento(TypedDict):
    nombre: str
    imagen: str  # base64
    transformaciones: List[Dict]

class Transformacion(TypedDict):
    tipo: str
    parametros: Dict[str, Any]

class ResultadoProcesamiento(TypedDict):
    status: str
    nombre: str
    imagen_procesada: str
    dimensiones_originales: str
    dimensiones_procesadas: str
    transformaciones_aplicadas: List[str]