from app import db
from datetime import datetime

class User(db.Model):
    __tablename__ = "users"

    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(50), unique=True, nullable=False)
    password = db.Column(db.String(200), nullable=False)  # Encriptada

class Imagen(db.Model):
    __tablename__ = "imagen"

    id = db.Column(db.Integer, primary_key=True)
    nombre = db.Column(db.String(250), nullable=False)
    tipo = db.Column(db.String(50), nullable=False)

class Cambio(db.Model):
    __tablename__ = "cambio"

    id = db.Column(db.Integer, primary_key=True)
    nombre = db.Column(db.String(250), nullable=False)
    especificaciones = db.Column(db.String(500), nullable=False)

class UsuariosImagen(db.Model):
    __tablename__ = "usuariosImagen"

    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey("users.id"), nullable=False)
    user = db.relationship("User", backref=db.backref("imagenes", lazy=True))

    imagen_id = db.Column(db.Integer, db.ForeignKey("imagen.id"), nullable=False)
    imagen = db.relationship("Imagen", backref=db.backref("usuarios", lazy=True))

    fecha_crea = db.Column(db.DateTime, default=datetime.now, nullable=False)

class ImagenCambio(db.Model):
    __tablename__ = "imagenCambio"

    id = db.Column(db.Integer, primary_key=True)
    cambio_id = db.Column(db.Integer, db.ForeignKey("cambio.id"), nullable=False)
    cambio = db.relationship("Cambio", backref=db.backref("imagenes", lazy=True))

    imagen_id = db.Column(db.Integer, db.ForeignKey("imagen.id"), nullable=False)
    imagen = db.relationship("Imagen", backref=db.backref("cambios", lazy=True))

    fecha_crea = db.Column(db.DateTime, default=datetime.now, nullable=False)