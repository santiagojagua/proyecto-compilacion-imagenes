from flask import Blueprint, jsonify
import traceback

debug_bp = Blueprint('debug', __name__)

@debug_bp.route('/health')
def health_check():
    """Health check simple"""
    return jsonify({
        'status': 'healthy',
        'service': 'Flask Server',
        'timestamp': '2025-10-16T03:15:00Z'
    })

@debug_bp.route('/pyro-connection')
def debug_pyro_connection():
    """Endpoint para debug de conexión Pyro"""
    try:
        from app.services.node_service import probar_conexion_pyro
        resultado = probar_conexion_pyro()
        return jsonify(resultado)
    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e),
            'traceback': traceback.format_exc()
        }), 500

@debug_bp.route('/test-minimal')
def debug_test_minimal():
    """Endpoint para prueba mínima de procesamiento"""
    try:
        from app.services.node_service import conectar_servidor_imagenes
        import base64
        
        # Crear una imagen mínima de prueba
        from PIL import Image, ImageDraw
        img = Image.new('RGB', (10, 10), color='red')
        draw = ImageDraw.Draw(img)
        draw.text((2, 2), "TEST", fill='white')
        
        # Convertir a base64
        import io
        buffer = io.BytesIO()
        img.save(buffer, format='JPEG')
        img_base64 = base64.b64encode(buffer.getvalue()).decode('utf-8')
        
        # Probar procesamiento directo
        procesador = conectar_servidor_imagenes()
        resultado = procesador.procesar_imagen_directo(img_base64, [
            {'tipo': 'redimensionar', 'parametros': {'ancho': 5, 'alto': 5}}
        ])
        
        return jsonify({
            'success': True,
            'resultado': resultado,
            'message': 'Prueba mínima exitosa'
        })
        
    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e),
            'traceback': traceback.format_exc(),
            'message': 'Error en prueba mínima'
        }), 500