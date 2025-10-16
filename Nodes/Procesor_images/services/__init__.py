# __init__.py
"""
Módulo de procesamiento de imágenes dividido en clases especializadas
"""

# Importar y exportar todas las clases principales
from .procesador_base import ProcesadorBase
from .procesador_base64 import ProcesadorBase64
from .procesador_archivos import ProcesadorArchivos
from .procesador_transformaciones import TransformacionesCV2
from .transformaciones import TransformacionesImagen

# Si necesitas mantener compatibilidad con imports antiguos
from .gestor_hilos import GestorHilos

# Exportar todas las clases
__all__ = [
    'ProcesadorBase',
    'ProcesadorBase64', 
    'ProcesadorArchivos',
    'TransformacionesCV2',
    'GestorHilos',
    'TransformacionesImagen'
]

# Alias para compatibilidad (opcional)
ProcesadorImagenes = ProcesadorBase64  # o la clase que prefieras como principal