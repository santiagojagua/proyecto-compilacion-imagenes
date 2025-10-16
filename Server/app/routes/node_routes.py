from flask import Blueprint, jsonify
from app.services.node_service import obtener_estado_servidor, probar_conexion_pyro

node_bp = Blueprint("node", __name__)

@node_bp.route("/status", methods=["GET"])
def get_node_status():
    """Obtiene el estado del servidor Pyro4"""
    resultado = obtener_estado_servidor()
    return jsonify(resultado)

@node_bp.route("/test-connection", methods=["GET"])
def test_node_connection():
    """Prueba la conexión con el servidor Pyro4"""
    resultado = probar_conexion_pyro()
    return jsonify(resultado)

# Mantener la ruta original para compatibilidad
@node_bp.route("/<int:node_id>", methods=["GET"])
def get_node(node_id):
    """Endpoint de compatibilidad"""
    from app.services.node_service import call_node
    response = call_node(node_id, {"action": "status"})
    return jsonify(response)