#!/usr/bin/env python3
"""
Script principal para iniciar el servidor Pyro de procesamiento de imágenes
"""
import sys
import os

# Agregar el directorio Procesor_images al path
sys.path.append(os.path.join(os.path.dirname(__file__), 'Procesor_images'))

from Procesor_images.core.servidor_principal import ServidorPrincipal

def main():
    """Función principal"""
    try:
        print("🚀 Iniciando Servidor Pyro de Procesamiento de Imágenes...")
        servidor = ServidorPrincipal()
        daemon = servidor.iniciar_servicios()
        
        print("🟢 Servidor Pyro ejecutándose. Presiona Ctrl+C para detener.")
        
        try:
            while True:
                import threading
                threading.Event().wait(1)
        except KeyboardInterrupt:
            print("\n🛑 Cerrando servidor...")
            servidor.gestor_hilos.shutdown()
            
    except Exception as e:
        print(f"💥 Error fatal: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()