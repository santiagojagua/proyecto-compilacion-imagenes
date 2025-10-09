from flask import Blueprint, request, Response
from app.services.soap_service import wsgi_app, soap_app
from spyne.interface.wsdl import Wsdl11
import io

soap_bp = Blueprint("soap", __name__)

@soap_bp.route("/soap", methods=["GET", "POST"])
def soap_service():
    if request.method == "GET":
        # Generar WSDL automáticamente
        wsdl = Wsdl11(soap_app.interface)
        wsdl_string = wsdl.get_interface_document()
        return Response(wsdl_string, mimetype='text/xml')
    
    elif request.method == "POST":
        # Procesar petición SOAP
        def start_response(status, response_headers, exc_info=None):
            nonlocal response_status, response_headers_out
            response_status = status
            response_headers_out = response_headers
            return lambda x: None

        response_status = None
        response_headers_out = None

        response = wsgi_app(request.environ, start_response)
        response_data = b"".join(response)

        return Response(response_data, 
                       status=int(response_status.split()[0]),
                       headers=dict(response_headers_out))

@soap_bp.route("/wsdl", methods=["GET"])
def get_wsdl():
    """Endpoint específico para obtener el WSDL"""
    wsdl = Wsdl11(soap_app.interface)
    wsdl_string = wsdl.get_interface_document()
    return Response(wsdl_string, mimetype='text/xml')
