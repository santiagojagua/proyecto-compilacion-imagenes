from spyne import Application, rpc, ServiceBase, Unicode
from spyne.protocol.soap import Soap11
from spyne.server.wsgi import WsgiApplication
from app.services.node_service import get_saludo_from_node

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

soap_app = Application(
    [HelloWorldService],
    tns="server.soap.service",
    in_protocol=Soap11(validator="lxml"),
    out_protocol=Soap11()
)

wsgi_app = WsgiApplication(soap_app)
