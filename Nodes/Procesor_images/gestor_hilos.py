import threading
from concurrent.futures import ThreadPoolExecutor
from typing import List, Dict, Any
import time

class GestorHilos:
    def __init__(self, max_workers: int = 5):
        self.max_workers = max_workers
        self.executor = ThreadPoolExecutor(max_workers=max_workers)
        self.lock = threading.Lock()
        self.tareas_activas = 0
        
    def procesar_paralelo(self, tareas: List[Dict]) -> Dict[str, Any]:
        """
        Procesa una lista de tareas en paralelo usando hilos
        """
        resultados = {}
        futures = {}
        
        with self.lock:
            self.tareas_activas += len(tareas)
        
        print(f"Procesando {len(tareas)} tareas en paralelo...")
        
        # Enviar cada tarea a un hilo
        for tarea in tareas:
            future = self.executor.submit(self._ejecutar_tarea, tarea)
            futures[future] = tarea.get('nombre', 'sin_nombre')
        
        # Recoger resultados
        for future in futures:
            nombre = futures[future]
            try:
                resultado = future.result(timeout=300)  # 5 minutos timeout
                resultados[nombre] = resultado
            except Exception as e:
                resultados[nombre] = {'error': str(e)}
        
        with self.lock:
            self.tareas_activas -= len(tareas)
        
        return resultados
    
    def _ejecutar_tarea(self, tarea: Dict) -> Any:
        """
        Ejecuta una tarea individual en un hilo
        """
        funcion = tarea['funcion']
        args = tarea.get('args', ())
        kwargs = tarea.get('kwargs', {})
        
        try:
            return funcion(*args, **kwargs)
        except Exception as e:
            return {'error': f"Error en tarea: {str(e)}"}
    
    def obtener_estado(self) -> Dict[str, Any]:
        """
        Retorna el estado actual del gestor de hilos
        """
        return {
            'max_workers': self.max_workers,
            'tareas_activas': self.tareas_activas,
            'hilos_disponibles': self.max_workers - self.tareas_activas,
            'timestamp': time.time()
        }
    
    def shutdown(self):
        """
        Apaga el ejecutor de hilos
        """
        self.executor.shutdown(wait=True)