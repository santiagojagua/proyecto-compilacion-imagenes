from flask import Blueprint, request, Response, jsonify
from app.services.soap_service import wsgi_app, soap_app
from app.services.node_service import probar_conexion_pyro, obtener_estado_servidor
from spyne.interface.wsdl import Wsdl11
import io

soap_bp = Blueprint("soap", __name__)

@soap_bp.route("/soap", methods=["GET", "POST"])
def soap_service():
    if request.method == "GET":
        wsdl = Wsdl11(soap_app.interface)
        wsdl_string = wsdl.get_interface_document()
        return Response(wsdl_string, mimetype='text/xml')

    elif request.method == "POST":
        # Ejecutar Spyne y capturar status/headers/respuesta
        response_status = None
        response_headers_out = None

        def start_response(status, response_headers, exc_info=None):
            nonlocal response_status, response_headers_out
            response_status = status
            response_headers_out = response_headers
            return lambda x: None

        try:
            # No leemos el body ni lo reinyectamos: Spyne lo consume del environ original
            response_iter = wsgi_app(request.environ, start_response)
            response_data = b"".join(response_iter)
        except Exception:
            import traceback
            print("❌ EXCEPCIÓN en wsgi_app:\n", traceback.format_exc())
            return Response("Internal Server Error", status=500)

        # Si también quieres silenciar la respuesta, elimina este bloque o ponlo detrás de un flag
        # print("==== SOAP RESPONSE ====")
        # print("Status:", response_status)
        # print("Headers:", dict(response_headers_out or {}))
        # print("Body (primeros 1200 chars):")
        # print(response_data[:1200].decode("utf-8", errors="replace"))
        # print("=======================")

        return Response(
            response_data,
            status=int(response_status.split()[0]) if response_status else 500,
            headers=dict(response_headers_out or {})
        )

@soap_bp.route("/wsdl", methods=["GET"])
def get_wsdl():
    """Endpoint específico para obtener el WSDL"""
    wsdl = Wsdl11(soap_app.interface)
    wsdl_string = wsdl.get_interface_document()
    return Response(wsdl_string, mimetype='text/xml')

# ✅ NUEVO: Endpoints de diagnóstico
@soap_bp.route("/debug/pyro-connection", methods=["GET"])
def debug_pyro_connection():
    """Endpoint para debug de conexión Pyro"""
    resultado = probar_conexion_pyro()
    return jsonify(resultado)

@soap_bp.route("/debug/pyro-status", methods=["GET"])
def debug_pyro_status():
    """Endpoint para obtener estado del servidor Pyro"""
    resultado = obtener_estado_servidor()
    return jsonify(resultado)