from spyne import Application, rpc, ServiceBase, Unicode, ComplexModel, Array, Integer
from spyne.protocol.soap import Soap11
from spyne.server.wsgi import WsgiApplication
from app.services.node_service import get_saludo_from_node
from app.services.db_service import *

# ===== MODELOS SOAP =====

class CambioType(ComplexModel):
    nombre = Unicode
    especificaciones = Unicode


class ImagenType(ComplexModel):
    nombre = Unicode
    tipo = Unicode
    contenido_base64 = Unicode
    cambios = Array(CambioType)


class ImagenCambiosType(ComplexModel):
    user_id = Integer
    imagenes = Array(ImagenType)


class HelloWorldService(ServiceBase):
    @rpc(Unicode, _returns=Unicode)
    def say_hello(ctx, message):
        if message.strip().lower() == "hola":
            return "Hola mundo"
        return f"Mensaje no reconocido: {message}"
    
    @rpc(_returns=Unicode)
    def get_saludo(ctx):
        """Llama al nodo a través del node_service"""
        return get_saludo_from_node()
    
    @rpc(ImagenCambiosType, _returns=Unicode)
    def procesar_imagen_cambios(ctx, request):
        
        """Recibe el XML del front, lo convierte y lo pasa a otra función"""
        user_id = request.user_id

        data_dict = spyne_to_dict(request)
        result = procesar_imagenes(data_dict)

        return f"Procesado correctamente: {result}"
    
def spyne_to_dict(obj):
    """
    Convierte un objeto Spyne (data) a dict recursivamente.
    """
    if isinstance(obj, list):
        return [spyne_to_dict(item) for item in obj]
    elif hasattr(obj, '__dict__'):
        return {k: spyne_to_dict(v) for k, v in obj.__dict__.items() if not k.startswith('_')}
    else:
        return obj

soap_app = Application(
    [HelloWorldService],
    tns="server.soap.service",
    in_protocol=Soap11(validator="lxml"),
    out_protocol=Soap11(),
    name='HelloWorldService'
)

wsgi_app = WsgiApplication(soap_app)
