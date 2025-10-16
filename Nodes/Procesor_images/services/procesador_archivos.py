import cv2
import numpy as np
import time
import Pyro4
import os

from .procesador_base import ProcesadorBase
from ..core.config import Config

@Pyro4.expose
class ProcesadorArchivos(ProcesadorBase):
    """Clase especializada en procesamiento de archivos de imagen"""
    
    def __init__(self, max_memory_usage=Config.MAX_MEMORY_USAGE):
        super().__init__(max_memory_usage)
    
    # ========== MÉTODOS PARA PROCESAMIENTO DE ARCHIVOS ==========
    
    @Pyro4.expose
    def procesar_imagen(self, image_path, output_dir, target_size=Config.TARGET_SIZE):
        """Procesa una sola imagen de alta resolución"""
        return self.process_single_image(image_path, output_dir, target_size)
    
    def process_single_image(self, image_path, output_dir, target_size=Config.TARGET_SIZE):
        """Procesa una sola imagen de alta resolución con manejo de memoria"""
        try:
            # Verificar memoria antes de procesar
            if self.check_memory_usage() > self.max_memory_usage * 100:
                self.logger.warning("Alto uso de memoria, esperando...")
                time.sleep(5)
                return {"status": "error", "message": "Alto uso de memoria"}
            
            self.logger.info(f"Procesando: {os.path.basename(image_path)}")
            
            # Leer imagen en modo de baja memoria si es posible
            img = cv2.imread(image_path, cv2.IMREAD_REDUCED_COLOR_2)
            if img is None:
                self.logger.error(f"No se pudo leer: {image_path}")
                return {"status": "error", "message": "No se pudo leer la imagen"}
            
            # Procesamiento optimizado para alta resolución
            processed_img = self.optimized_processing(img, target_size)
            
            # Guardar resultado
            output_filename = f"processed_{os.path.basename(image_path)}"
            output_path = os.path.join(output_dir, output_filename)
            
            success = cv2.imwrite(output_path, processed_img, 
                                [cv2.IMWRITE_JPEG_QUALITY, 95])
            
            if success:
                self.logger.info(f"Guardado: {output_path}")
                return {
                    "status": "success",
                    "original": image_path,
                    "processed": output_path,
                    "filename": output_filename
                }
            else:
                self.logger.error(f"Error guardando: {output_path}")
                return {"status": "error", "message": "Error guardando imagen"}
                
        except Exception as e:
            self.logger.error(f"Error procesando {image_path}: {e}")
            return {"status": "error", "message": str(e)}
    
    @Pyro4.expose
    def procesar_lote(self, input_dir, output_dir, batch_size=Config.BATCH_SIZE, supported_formats=None):
        """Procesa un lote de imágenes de alta resolución"""
        return self.process_batch(input_dir, output_dir, batch_size, supported_formats)
    
    def process_batch(self, input_dir, output_dir, batch_size=Config.BATCH_SIZE, supported_formats=None):
        """Procesa un lote de imágenes de alta resolución"""
        if supported_formats is None:
            supported_formats = ['.jpg', '.jpeg', '.png', '.tiff', '.tif', '.bmp']
        
        # Crear directorio de salida si no existe
        os.makedirs(output_dir, exist_ok=True)
        
        # Obtener lista de imágenes
        image_files = self._obtener_archivos_imagen(input_dir, supported_formats)
        total_images = len(image_files)
        
        if total_images == 0:
            return {
                "status": "error",
                "message": f"No se encontraron imágenes en {input_dir} con formatos: {supported_formats}"
            }
        
        self.logger.info(f"Iniciando procesamiento de {total_images} imágenes")
        
        # Procesar en lotes para control de memoria
        processed_count = 0
        failed_count = 0
        processed_files = []
        failed_files = []
        
        self.is_processing = True
        
        try:
            for i in range(0, total_images, batch_size):
                batch = image_files[i:i + batch_size]
                batch_number = i//batch_size + 1
                total_batches = (total_images + batch_size - 1)//batch_size
                
                self.logger.info(f"Procesando lote {batch_number}/{total_batches}")
                
                for image_path in batch:
                    result = self.process_single_image(image_path, output_dir)
                    if result["status"] == "success":
                        processed_count += 1
                        processed_files.append(result)
                    else:
                        failed_count += 1
                        failed_files.append({
                            "file": image_path,
                            "error": result["message"]
                        })
                    
                    # Pequeña pausa para evitar sobrecarga
                    time.sleep(0.1)
                
                # Log de progreso
                progress = (i + len(batch)) / total_images * 100
                self.logger.info(f"Progreso: {progress:.1f}% - Procesadas: {processed_count}, Fallidas: {failed_count}")
                
                # Limpiar memoria periódicamente
                if i % (batch_size * 3) == 0:
                    self.cleanup_memory()
            
            self.logger.info(f"Procesamiento completado: {processed_count} exitosas, {failed_count} fallidas de {total_images} totales")
            
            return {
                "status": "completed",
                "total_images": total_images,
                "processed": processed_count,
                "failed": failed_count,
                "processed_files": processed_files,
                "failed_files": failed_files,
                "output_directory": output_dir
            }
            
        finally:
            self.is_processing = False

    @Pyro4.expose
    def redimensionar_imagen(self, image_path, output_path, nuevo_tamaño):
        """Método de compatibilidad para redimensionamiento individual"""
        result = self.process_single_image(image_path, os.path.dirname(output_path), nuevo_tamaño)
        if result["status"] == "success":
            return True
        else:
            raise Exception(result["message"])
    
    # ========== MÉTODOS AUXILIARES ARCHIVOS ==========

    def _obtener_archivos_imagen(self, directorio, formatos_soportados):
        """Obtiene la lista de archivos de imagen en un directorio"""
        archivos = []
        for file in os.listdir(directorio):
            if any(file.lower().endswith(fmt) for fmt in formatos_soportados):
                archivos.append(os.path.join(directorio, file))
        return archivos

    def optimized_processing(self, image, target_size):
        """Procesamiento optimizado para imágenes de alta resolución"""
        # Redimensionamiento inteligente manteniendo aspect ratio
        h, w = image.shape[:2]
        target_w, target_h = target_size
        
        # Calcular nuevo tamaño manteniendo proporciones
        scale = min(target_w / w, target_h / h)
        new_w, new_h = int(w * scale), int(h * scale)
        
        # Redimensionar usando interpolación de alta calidad
        resized = cv2.resize(image, (new_w, new_h), 
                           interpolation=cv2.INTER_LANCZOS4)
        
        # Mejora de calidad básica
        # Convertir a float32 para operaciones
        img_float = resized.astype(np.float32) / 255.0
        
        # Ajuste de contraste leve
        img_contrast = cv2.convertScaleAbs(img_float * 255, alpha=1.1, beta=0)
        
        return img_contrast

    def estimate_image_memory(self, image_path):
        """Estima la memoria requerida para una imagen"""
        try:
            # Leer solo los metadatos para estimar tamaño
            img = cv2.imread(image_path, cv2.IMREAD_UNCHANGED)
            if img is None:
                return 0
            
            # Calcular memoria aproximada en MB
            memory_mb = (img.shape[0] * img.shape[1] * img.shape[2]) / (1024 * 1024)
            return memory_mb
        except Exception as e:
            self.logger.warning(f"Error estimando memoria para {image_path}: {e}")
            return 0

    def cleanup_memory(self):
        """Limpia la memoria periódicamente"""
        try:
            import gc
            gc.collect()
            self.logger.debug("Memoria limpiada")
        except Exception as e:
            self.logger.warning(f"Error limpiando memoria: {e}")

    @Pyro4.expose
    def check_memory_usage(self):
        """Verifica el uso actual de memoria"""
        import psutil
        memory_info = psutil.virtual_memory()
        return memory_info.percent