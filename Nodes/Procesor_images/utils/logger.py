import logging
import sys
import io

def setup_logging(name, level=logging.INFO):
    logger = logging.getLogger(name)

    if not logger.handlers:
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

        # Archivo en utf-8
        file_handler = logging.FileHandler('highres_batch_processing.log', encoding='utf-8')
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)

        # Consola en utf-8 (con fallback)
        stream = sys.stdout
        try:
            if hasattr(stream, "reconfigure"):
                stream.reconfigure(encoding='utf-8', errors='replace')
            else:
                stream = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
        except Exception:
            pass

        console_handler = logging.StreamHandler(stream)
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)

        logger.setLevel(level)

    return logger