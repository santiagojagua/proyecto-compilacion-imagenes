from app import db
from app.models.entitys import User
from app.models.viewModel import ImagenCambiosViewModel
from .node_service import procesar_imagenes_pyro

def save_request(data):
    req = User(content=data)
    db.session.add(req)
    db.session.commit()
    
def procesar_imagenes(data: dict):
    """
    Recibe un diccionario (desde el SOAP), lo transforma al ViewModel
    y envía al servidor Pyro4 para procesamiento
    """
    try:
        # Convertir a ViewModel
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
        
        # Guardar en base de datos si es necesario
        # save_request(data)  # Descomentar si quieres guardar las peticiones
        
        return resultado
    
    except Exception as e:
        db.session.rollback()
        return {
            'success': False, 
            'message': f'Error en procesamiento: {str(e)}'
        }

# Eliminar la función construir_json_rpc ya que no se usa más