from spyne import Application, rpc, ServiceBase, Unicode, ComplexModel, Array, Integer
from spyne.protocol.soap import Soap11
from spyne.server.wsgi import WsgiApplication
from spyne.model.complex import Iterable
from spyne.model.primitive import String
from app.services.db_service import procesar_imagenes
import json

# ===== MODELOS SOAP =====

class CambioType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    especificaciones = Unicode

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ImagenType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    tipo = Unicode
    contenido_base64 = Unicode
    cambios = Array(CambioType)

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ImagenCambiosType(ComplexModel):
    __namespace__ = "server.soap.service"
    user_id = Integer
    imagenes = Array(ImagenType)

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ResultadoImagenType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    estado = Unicode
    imagen_procesada = Unicode
    formato_salida = Unicode
    dimensiones_finales = Unicode

class ProcesamientoResultType(ComplexModel):
    __namespace__ = "server.soap.service"
    success = Unicode
    total_procesadas = Integer
    mensaje = Unicode
    resultados = Array(ResultadoImagenType)

class ImageProcessingService(ServiceBase):
    __namespace__ = "server.soap.service"
    
    @rpc(ImagenCambiosType, _returns=ProcesamientoResultType)
    def procesar_imagen_cambios(ctx, request):
        """
        Recibe el XML SOAP con imágenes y transformaciones,
        las envía al servidor Pyro4 para procesamiento
        """
        try:
            print("SOAP Request recibido:")
            print(f"User ID: {request.user_id}")
            print(f"Número de imágenes: {len(request.imagenes)}")
            
            # Convertir objeto Spyne a dict
            data_dict = {
                'user_id': request.user_id,
                'imagenes': []
            }
            
            for img in request.imagenes:
                imagen_data = {
                    'nombre': img.nombre,
                    'tipo': img.tipo,
                    'contenido_base64': img.contenido_base64,
                    'cambios': []
                }
                
                for cambio in img.cambios:
                    cambio_data = {
                        'nombre': cambio.nombre,
                        'especificaciones': cambio.especificaciones
                    }
                    imagen_data['cambios'].append(cambio_data)
                
                data_dict['imagenes'].append(imagen_data)
            
            # Procesar a través del servicio de base de datos (que ahora usa Pyro4)
            result = procesar_imagenes(data_dict)
            
            # Construir respuesta SOAP estructurada
            response = ProcesamientoResultType()
            response.success = "true" if result.get('success') else "false"
            response.mensaje = result.get('mensaje', '')
            
            if result.get('success') and 'resultado' in result:
                pyro_result = result['resultado']
                response.total_procesadas = pyro_result.get('total_procesadas', 0)
                response.resultados = []
                
                if 'resultados' in pyro_result:
                    for nombre, img_result in pyro_result['resultados'].items():
                        resultado_img = ResultadoImagenType()
                        resultado_img.nombre = nombre
                        resultado_img.estado = img_result.get('estado', 'desconocido')
                        resultado_img.formato_salida = img_result.get('formato_salida', '')
                        resultado_img.dimensiones_finales = str(img_result.get('dimensiones_finales', ''))
                        
                        # Incluir imagen procesada (puede ser muy grande, considerar opcional)
                        if 'imagen_procesada' in img_result:
                            resultado_img.imagen_procesada = img_result['imagen_procesada'][:100] + "..." if len(img_result['imagen_procesada']) > 100 else img_result['imagen_procesada']
                        else:
                            resultado_img.imagen_procesada = ""
                        
                        response.resultados.append(resultado_img)
            
            return response
                
        except Exception as e:
            import traceback
            error_details = traceback.format_exc()
            print(f"Error en SOAP service: {error_details}")
            
            # Devolver respuesta de error
            response = ProcesamientoResultType()
            response.success = "false"
            response.mensaje = f"Error en el servicio: {str(e)}"
            response.total_procesadas = 0
            response.resultados = []
            
            return response
    
    @rpc(_returns=Unicode)
    def obtener_estado_servidor(ctx):
        """Verifica el estado del servidor Pyro4"""
        from app.services.node_service import obtener_estado_servidor
        
        estado = obtener_estado_servidor()
        if estado['success']:
            return f"✅ Servidor Pyro4 conectado - {estado['detalles']['estado']}"
        else:
            return f"❌ Servidor Pyro4 desconectado: {estado['error']}"

# Configuración de la aplicación SOAP con namespaces correctos
soap_app = Application(
    [ImageProcessingService],
    tns='server.soap.service',
    in_protocol=Soap11(validator='lxml'),
    out_protocol=Soap11(),
    name='ImageProcessingService'
)

wsgi_app = WsgiApplication(soap_app)