from app import db
from app.models.entitys import User
from app.models.viewModel import ImagenCambiosViewModel
from .node_service import *

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
        json_rpc = construir_json_rpc(vm)

        resultado = enviar_json_imagenes(json_rpc)
        print(f"Respuesta del nodo: {resultado}")

        return {"success": True, "message": "Datos procesados correctamente"}
    
    except Exception as e:
        db.session.rollback()
        return {"success": False, "message": str(e)}
    
def construir_json_rpc(vm: ImagenCambiosViewModel) -> dict:
    tareas = []
    for idx, img in enumerate(vm.imagenes, start=1):

        ops = {c.nombre: c.especificaciones for c in img.cambios}
        tarea = {
            "id": idx,
            "b64": img.contenido_base64,
            "ops": ops
        }
        tareas.append(tarea)
    
    json_rpc = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "imgxProcesarLote",
        "params": {
            "tareas": tareas,
            "default-opts": {"as-data-uri": True},
            "max-threads": 4
        }
    }
    
    return json_rpc
