from flask import Blueprint, jsonify
from app.services.node_service import call_node

node_bp = Blueprint("node", __name__)

@node_bp.route("/<int:node_id>", methods=["GET"])
def get_node(node_id):
    response = call_node(node_id, {"action": "status"})
    return jsonify(response)
