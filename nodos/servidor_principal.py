import Pyro4
import sys
import os

# Añadir el directorio actual al path para imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from procesador_imagenes import ProcesadorImagenes

def main():
    print("=== Iniciando Servidor de Procesamiento de Imágenes ===")
    
    # Configuración del servidor
    HOST = "localhost"
    PORT = 9090
    MAX_WORKERS = 5
    
    try:
        # Crear el daemon de Pyro4
        daemon = Pyro4.Daemon(host=HOST, port=PORT)
        
        # Crear instancia del procesador
        procesador = ProcesadorImagenes(max_workers=MAX_WORKERS)
        
        # Registrar el objeto en el daemon
        uri = daemon.register(procesador, "procesador.imagenes")
        
        # Mostrar información del servidor
        print(f"✓ Servidor iniciado en {HOST}:{PORT}")
        print(f"✓ URI del servicio: {uri}")
        print(f"✓ Hilos máximos de trabajo: {MAX_WORKERS}")
        print("✓ Transformaciones disponibles:")
        
        transformaciones = procesador.transformaciones.obtener_transformaciones_disponibles()
        for key, desc in transformaciones.items():
            print(f"  - {key}: {desc}")
        
        print("\n=== Servidor listo para recibir conexiones ===")
        print("Presiona Ctrl+C para detener el servidor\n")
        
        # Iniciar el loop principal del servidor
        daemon.requestLoop()
        
    except KeyboardInterrupt:
        print("\n\n=== Deteniendo servidor ===")
        print("Apagando gestor de hilos...")
        procesador.gestor_hilos.shutdown()
        print("✓ Servidor detenido correctamente")
    
    except Exception as e:
        print(f"❌ Error iniciando el servidor: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()