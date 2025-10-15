import Pyro4
import sys
import os

# Añadir el directorio actual al path para imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from Procesor_images.procesador_imagenes import ProcesadorImagenes

def crear_carpeta_imagenes():
    """Crea la carpeta images si no existe"""
    carpeta_images = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'images')
    if not os.path.exists(carpeta_images):
        os.makedirs(carpeta_images)
        print(f"✓ Carpeta 'images' creada en: {carpeta_images}")
    return carpeta_images

def main():
    print("=== Iniciando Servidor de Procesamiento de Imágenes ===")
    
    # Configuración del servidor
    HOST = "localhost"
    PORT = 9090
    MAX_WORKERS = 5
    
    # Crear carpeta images
    carpeta_images = crear_carpeta_imagenes()
    
    try:
        # Crear el daemon de Pyro4
        print("Iniciando daemon Pyro4...")
        daemon = Pyro4.Daemon(host=HOST, port=PORT)
        
        # Crear instancia del procesador
        print("Creando procesador de imágenes...")
        procesador = ProcesadorImagenes(max_workers=MAX_WORKERS, carpeta_salida=carpeta_images)
        
        # Registrar el objeto en el daemon
        print("Registrando servicio...")
        uri = daemon.register(procesador, "procesador.imagenes")
        
        # Intentar registrar en el nameserver (con timeout)
        print("Buscando nameserver...")
        try:
            ns = Pyro4.locateNS(host=HOST, broadcast=False, timeout=2)
            ns.register("procesador.imagenes", uri)
            print("✓ Servidor registrado en el nameserver de Pyro4")
        except Exception as e:
            print(f"⚠ Nameserver no encontrado, ejecutando sin registro: {e}")
            print("✓ Los clientes deberán usar la URI directamente")
        
        # Mostrar información del servidor
        print(f"\n✓ Servidor iniciado en {HOST}:{PORT}")
        print(f"✓ URI del servicio: {uri}")
        print(f"✓ Hilos máximos de trabajo: {MAX_WORKERS}")
        print(f"✓ Carpeta de salida: {carpeta_images}")
        
        transformaciones = procesador.transformaciones.obtener_transformaciones_disponibles()
        print(f"✓ Transformaciones disponibles: {len(transformaciones)}")
        
        print("\n" + "="*50)
        print("SERVIDOR LISTO - Esperando conexiones...")
        print("="*50)
        print("Para conectar un cliente use:")
        print(f"URI: {uri}")
        print("Presiona Ctrl+C para detener el servidor")
        
        # Iniciar el loop principal del servidor
        daemon.requestLoop()
        
    except KeyboardInterrupt:
        print("\n\n=== Deteniendo servidor ===")
        print("Apagando gestor de hilos...")
        if 'procesador' in locals():
            procesador.gestor_hilos.shutdown()
        print("✓ Servidor detenido correctamente")
    
    except Exception as e:
        print(f"❌ Error crítico en el servidor: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()