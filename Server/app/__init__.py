from flask import Flask
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

def create_app():
    app = Flask(__name__)

    #Configuración
    app.config.from_object("app.config.Config")

    #Inicializar BD
    db.init_app(app)

    #Registrar rutas
    from app.routes.client_routes import soap_bp
    #from app.routes.node_routes import node_bp

    app.register_blueprint(soap_bp, url_prefix="/client")
    #app.register_blueprint(node_bp, url_prefix="/node")

    return app
