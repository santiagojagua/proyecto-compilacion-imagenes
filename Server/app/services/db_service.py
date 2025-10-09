from app import db
from app.models.entitys import User
from app.models.viewModel import ImagenCambiosViewModel

def save_request(data):
    req = User(content=data)
    db.session.add(req)
    db.session.commit()
    
def procesar_imagenes(data: dict):
    
    """
    Recibe un diccionario (desde el SOAP/JSON/XML),
    lo transforma al ViewModel y guarda todo en la BD.
    """
    
    try:
        
        vm = ImagenCambiosViewModel(data)
        print(f"🧩 Usuario: {vm.user_id}")
        for img in vm.imagenes:
            print(f"📸 Imagen: {img.nombre} ({img.tipo})")
            for cambio in img.cambios:
                print(f"   🔁 Cambio: {cambio.nombre} - {cambio.especificaciones}")
        return {"success": True, "message": "Datos procesados correctamente"}
    
    except Exception as e:
        db.session.rollback()
        return {"success": False, "message": str(e)}
