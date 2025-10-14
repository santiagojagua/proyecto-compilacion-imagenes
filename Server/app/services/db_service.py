from app import db
from app.models.entitys import User, Imagen, Cambio, UsuariosImagen, ImagenCambio
from app.models.viewModel import ImagenCambiosViewModel
from .node_service import procesar_imagenes_pyro
from datetime import datetime
import hashlib

def hash_password(password):
    """Encripta la contraseña usando SHA-256"""
    return hashlib.sha256(password.encode('utf-8')).hexdigest()

def registrar_usuario(username, password):
    """
    Registra un nuevo usuario en la base de datos
    """
    try:
        # Verificar si el usuario ya existe
        usuario_existente = User.query.filter_by(username=username).first()
        if usuario_existente:
            return {
                'success': False,
                'message': 'El nombre de usuario ya existe'
            }
        
        # Crear nuevo usuario
        nuevo_usuario = User(
            username=username,
            password=hash_password(password)
        )
        
        db.session.add(nuevo_usuario)
        db.session.commit()
        
        return {
            'success': True,
            'message': 'Usuario registrado exitosamente',
            'user_id': nuevo_usuario.id
        }
        
    except Exception as e:
        db.session.rollback()
        return {
            'success': False,
            'message': f'Error registrando usuario: {str(e)}'
        }

def login_usuario(username, password):
    """
    Autentica un usuario
    """
    try:
        # Buscar usuario
        usuario = User.query.filter_by(username=username).first()
        
        if not usuario:
            return {
                'success': False,
                'message': 'Usuario no encontrado'
            }
        
        # Verificar contraseña
        if usuario.password != hash_password(password):
            return {
                'success': False,
                'message': 'Contraseña incorrecta'
            }
        
        return {
            'success': True,
            'message': 'Login exitoso',
            'user_id': usuario.id
        }
        
    except Exception as e:
        return {
            'success': False,
            'message': f'Error en login: {str(e)}'
        }

def registrar_imagenes_en_db(data: dict):
    """
    Registra las imágenes y cambios en la base de datos antes del procesamiento
    """
    try:
        vm = ImagenCambiosViewModel(data)
        user_id = vm.user_id
        
        # Verificar que el usuario existe
        usuario = User.query.get(user_id)
        if not usuario:
            return {
                'success': False,
                'message': f'Usuario con ID {user_id} no encontrado'
            }
        
        ids_imagenes = []
        
        for img in vm.imagenes:
            # 1. Crear registro de Imagen
            nueva_imagen = Imagen(
                nombre=img.nombre,
                tipo=img.tipo
            )
            db.session.add(nueva_imagen)
            db.session.flush()  # Para obtener el ID
            
            # 2. Crear relación UsuariosImagen
            usuario_imagen = UsuariosImagen(
                user_id=user_id,
                imagen_id=nueva_imagen.id,
                fecha_crea=datetime.now()
            )
            db.session.add(usuario_imagen)
            
            # 3. Crear registros de Cambios
            for cambio in img.cambios:
                nuevo_cambio = Cambio(
                    nombre=cambio.nombre,
                    especificaciones=cambio.especificaciones
                )
                db.session.add(nuevo_cambio)
                db.session.flush()
                
                # 4. Crear relación ImagenCambio
                imagen_cambio = ImagenCambio(
                    cambio_id=nuevo_cambio.id,
                    imagen_id=nueva_imagen.id,
                    fecha_crea=datetime.now()
                )
                db.session.add(imagen_cambio)
            
            ids_imagenes.append(nueva_imagen.id)
        
        # Confirmar todos los cambios en la BD
        db.session.commit()
        
        return {
            'success': True,
            'message': f'Registradas {len(ids_imagenes)} imágenes en la base de datos',
            'ids_imagenes': ids_imagenes
        }
        
    except Exception as e:
        db.session.rollback()
        return {
            'success': False,
            'message': f'Error registrando en BD: {str(e)}'
        }

def procesar_imagenes(data: dict):
    """
    Procesa las imágenes después de registrarlas en la BD
    """
    try:
        # Convertir a ViewModel para validación
        vm = ImagenCambiosViewModel(data)
        
        # Preparar datos para Pyro4
        lista_imagenes = []
        for img in vm.imagenes:
            imagen_data = {
                'nombre': img.nombre,
                'contenido_base64': img.contenido_base64,
                'cambios': [
                    {
                        'nombre': cambio.nombre,
                        'especificaciones': cambio.especificaciones
                    }
                    for cambio in img.cambios
                ]
            }
            lista_imagenes.append(imagen_data)
        
        # Enviar al servidor Pyro4
        resultado = procesar_imagenes_pyro(lista_imagenes)
        
        return resultado
    
    except Exception as e:
        return {
            'success': False, 
            'message': f'Error en procesamiento: {str(e)}'
        }

def obtener_historial_usuario(user_id: int):
    """
    Obtiene el historial de procesamiento de un usuario
    """
    try:
        historial = db.session.query(
            UsuariosImagen.fecha_crea,
            Imagen.nombre,
            Imagen.tipo,
            Cambio.nombre.label('cambio_nombre'),
            Cambio.especificaciones
        ).join(Imagen, UsuariosImagen.imagen_id == Imagen.id)\
         .join(ImagenCambio, Imagen.id == ImagenCambio.imagen_id)\
         .join(Cambio, ImagenCambio.cambio_id == Cambio.id)\
         .filter(UsuariosImagen.user_id == user_id)\
         .order_by(UsuariosImagen.fecha_crea.desc())\
         .all()
        
        resultados = []
        for item in historial:
            resultados.append({
                'fecha': item.fecha_crea.isoformat(),
                'imagen_nombre': item.nombre,
                'imagen_tipo': item.tipo,
                'cambio_aplicado': item.cambio_nombre,
                'especificaciones': item.especificaciones
            })
        
        return {
            'success': True,
            'historial': resultados,
            'total_registros': len(resultados)
        }
        
    except Exception as e:
        return {
            'success': False,
            'message': f'Error obteniendo historial: {str(e)}'
        }