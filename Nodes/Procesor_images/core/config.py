from pathlib import Path
import socket
import logging

class Config:
    """Configuración del servidor Pyro"""
    
    # Configuración de red
    HOST = '0.0.0.0'
    PORT = 9090
    MAX_HILOS = 3
    MAX_MEMORY_USAGE = 0.8
    
    # Timeouts
    PYRO_TIMEOUT = 60
    WORKER_TIMEOUT = 300  # 5 minutos
    
    # Configuración de procesamiento
    BATCH_SIZE = 10
    TARGET_SIZE = (1024, 1024)

    PROJECT_ROOT = Path(__file__).resolve().parents[2]  # /.../Nodes
    IMAGES_DIR = PROJECT_ROOT / "images"
    SAVE_IMAGES = True
    
    @classmethod
    def get_available_port(cls, start_port=9090):
        """Encuentra un puerto disponible"""
        port = start_port
        while port < 65535:
            try:
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                    s.bind((cls.HOST, port))
                    return port
            except OSError:
                port += 1
        raise Exception("No se pudo encontrar un puerto disponible")