from flask import Flask
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

def create_app():
    app = Flask(__name__)
    app.config.from_object('app.config.Config')
    
    # Inicializar la base de datos
    db.init_app(app)
    
    # Importar blueprints DENTRO de la función para evitar imports circulares
    with app.app_context():
        from app.routes.client_routes import soap_bp
        from app.routes.node_routes import node_bp
        from app.routes.debug_routes import debug_bp
        
        # Registrar blueprints con prefijos
        app.register_blueprint(soap_bp, url_prefix='/client')
        app.register_blueprint(node_bp, url_prefix='/node')
        app.register_blueprint(debug_bp, url_prefix='/debug')
    
    return app