import Pyro4
import threading
import logging
from ..services import ProcesadorImagenes, TransformacionesImagen, GestorHilos
from .config import Config

logger = logging.getLogger(__name__)

class ServidorPrincipal:
    def __init__(self, host=Config.HOST, port=Config.PORT):
        self.host = host
        self.port = port
        self.procesador_imagenes = ProcesadorImagenes()
        self.transformaciones = TransformacionesImagen()
        self.gestor_hilos = GestorHilos(max_hilos=Config.MAX_HILOS)
        
        # Integrar gestor de hilos con procesador
        self.procesador_imagenes.set_gestor_hilos(self.gestor_hilos)
        
    def iniciar_servicios(self):
        """Inicia todos los servicios Pyro y el gestor de hilos"""
        # Iniciar gestor de hilos
        self.gestor_hilos.iniciar_workers()
        
        Pyro4.config.DETAILED_TRACEBACK = True

        # Crear daemon con puerto fijo
        daemon = Pyro4.Daemon(host=self.host, port=self.port)
        
        # Registrar servicios
        uri_imagenes = daemon.register(self.procesador_imagenes, "procesador.imagenes")
        uri_transformaciones = daemon.register(self.transformaciones, "transformaciones.imagen")
        uri_gestor_hilos = daemon.register(self.gestor_hilos, "gestor.hilos")
        
        self._mostrar_info_servicios(daemon, uri_imagenes, uri_transformaciones, uri_gestor_hilos)
        
        # Iniciar el loop del daemon en un hilo separado
        thread = threading.Thread(target=daemon.requestLoop, name="Pyro4-Daemon")
        thread.daemon = True
        thread.start()
        
        return daemon
    
    def _mostrar_info_servicios(self, daemon, uri_imagenes, uri_transformaciones, uri_gestor_hilos):
        # 1) Preferir locationStr
        location = getattr(daemon, "locationStr", None)

        # 2) Si no existe, intentar con el URI devuelto por register
        if not location:
            try:
                host = getattr(uri_imagenes, "host", self.host)
                port = getattr(uri_imagenes, "port", self.port)
                location = f"{host}:{port}"
            except Exception:
                location = f"{self.host}:{self.port}"  # último recurso

        print("=== Servicios Pyro Iniciados ===")
        print(f"Servidor: {location}")
        print(f"Procesador Imágenes: {uri_imagenes}")
        print(f"Transformaciones: {uri_transformaciones}")
        print(f"Gestor Hilos: {uri_gestor_hilos}")
        print("=================================")

if __name__ == "__main__":
    servidor = ServidorPrincipal()
    daemon = servidor.iniciar_servicios()
    
    try:
        print("🟢 Servidor Pyro ejecutándose. Presiona Ctrl+C para detener.")
        while True:
            threading.Event().wait(1)
    except KeyboardInterrupt:
        print("\n🛑 Cerrando servidor...")
        servidor.gestor_hilos.shutdown()