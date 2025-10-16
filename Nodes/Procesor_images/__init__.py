"""
Módulo principal para procesamiento de imágenes con Pyro4
"""
from .core.servidor_principal import ServidorPrincipal
from .services.gestor_hilos import GestorHilos
from .services.transformaciones import TransformacionesImagen

# Importar las nuevas clases especializadas
from .services.procesador_base64 import ProcesadorBase64
from .services.procesador_archivos import ProcesadorArchivos

# Alias para mantener compatibilidad
ProcesadorImagenes = ProcesadorBase64

__version__ = "1.0.0"
__all__ = [
    'ServidorPrincipal',
    'ProcesadorImagenes', 
    'TransformacionesImagen',
    'GestorHilos',
    'ProcesadorBase64',
    'ProcesadorArchivos'
]