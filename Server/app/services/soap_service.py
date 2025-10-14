from spyne import Application, rpc, ServiceBase, Unicode, ComplexModel, Array, Integer
from spyne.protocol.soap import Soap11
from spyne.server.wsgi import WsgiApplication
from spyne.model.complex import Iterable
from spyne.model.primitive import String
from app.services.db_service import procesar_imagenes, registrar_imagenes_en_db, registrar_usuario, login_usuario
from app import db
import hashlib

# ===== MODELOS SOAP =====

class CambioType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    especificaciones = Unicode

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ImagenType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    tipo = Unicode
    contenido_base64 = Unicode
    cambios = Array(CambioType)

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ImagenCambiosType(ComplexModel):
    __namespace__ = "server.soap.service"
    user_id = Integer
    imagenes = Array(ImagenType)

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

class ResultadoTransformacionType(ComplexModel):
    __namespace__ = "server.soap.service"
    transformacion = Unicode
    numero = Integer
    estado = Unicode
    error = Unicode(optional=True)

class ResultadoImagenType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre_original = Unicode
    estado = Unicode
    imagen_procesada_base64 = Unicode
    formato_salida = Unicode
    dimensiones_finales = Unicode
    archivo_guardado = Unicode
    transformaciones_aplicadas = Array(ResultadoTransformacionType)

class ProcesamientoResultType(ComplexModel):
    __namespace__ = "server.soap.service"
    success = Unicode
    total_procesadas = Integer
    mensaje = Unicode
    user_id = Integer
    resultados = Array(ResultadoImagenType)

class LoginResultType(ComplexModel):
    __namespace__ = "server.soap.service"
    success = Unicode
    user_id = Integer
    mensaje = Unicode
    username = Unicode

class RegisterResultType(ComplexModel):
    __namespace__ = "server.soap.service"
    success = Unicode
    user_id = Integer
    mensaje = Unicode
    username = Unicode

class ImageProcessingService(ServiceBase):
    __namespace__ = "server.soap.service"
    
    @rpc(ImagenCambiosType, _returns=ProcesamientoResultType)
    def procesar_imagen_cambios(ctx, request):
        """
        Recibe el XML SOAP con imágenes y transformaciones,
        registra en BD, envía al servidor Pyro4 y devuelve resultados
        """
        try:
            print("SOAP Request recibido:")
            print(f"User ID: {request.user_id}")
            print(f"Número de imágenes: {len(request.imagenes)}")
            
            # Convertir objeto Spyne a dict
            data_dict = {
                'user_id': request.user_id,
                'imagenes': []
            }
            
            for img in request.imagenes:
                imagen_data = {
                    'nombre': img.nombre,
                    'tipo': img.tipo,
                    'contenido_base64': img.contenido_base64,
                    'cambios': []
                }
                
                for cambio in img.cambios:
                    cambio_data = {
                        'nombre': cambio.nombre,
                        'especificaciones': cambio.especificaciones
                    }
                    imagen_data['cambios'].append(cambio_data)
                
                data_dict['imagenes'].append(imagen_data)
            
            # 1. PRIMERO: Registrar en base de datos
            print("📝 Registrando en base de datos...")
            registro_db = registrar_imagenes_en_db(data_dict)
            
            if not registro_db.get('success'):
                raise Exception(f"Error registrando en BD: {registro_db.get('message')}")
            
            print(f"✅ Registro en BD exitoso. IDs: {registro_db.get('ids_imagenes', [])}")
            
            # 2. SEGUNDO: Procesar imágenes con Pyro4
            print("🔄 Enviando imágenes a servidor Pyro4...")
            result_pyro = procesar_imagenes(data_dict)
            
            # 3. Construir respuesta SOAP estructurada
            response = ProcesamientoResultType()
            response.success = "true" if result_pyro.get('success') else "false"
            response.mensaje = result_pyro.get('mensaje', '')
            response.user_id = request.user_id
            
            if result_pyro.get('success') and 'resultado' in result_pyro:
                pyro_result = result_pyro['resultado']
                response.total_procesadas = pyro_result.get('total_procesadas', 0)
                response.resultados = []
                
                if 'resultados' in pyro_result:
                    for nombre, img_result in pyro_result['resultados'].items():
                        resultado_img = ResultadoImagenType()
                        resultado_img.nombre_original = nombre
                        resultado_img.estado = img_result.get('estado', 'desconocido')
                        resultado_img.formato_salida = img_result.get('formato_salida', '')
                        resultado_img.dimensiones_finales = str(img_result.get('dimensiones_finales', ''))
                        resultado_img.archivo_guardado = img_result.get('archivo_guardado', '')
                        
                        # Incluir imagen procesada en base64 (completa)
                        if 'imagen_procesada' in img_result:
                            resultado_img.imagen_procesada_base64 = img_result['imagen_procesada']
                        else:
                            resultado_img.imagen_procesada_base64 = ""
                        
                        # Transformaciones aplicadas
                        resultado_img.transformaciones_aplicadas = []
                        if 'transformaciones_aplicadas' in img_result:
                            for trans in img_result['transformaciones_aplicadas']:
                                trans_result = ResultadoTransformacionType()
                                trans_result.transformacion = trans.get('transformacion', '')
                                trans_result.numero = trans.get('numero', 0)
                                trans_result.estado = trans.get('estado', '')
                                if 'error' in trans:
                                    trans_result.error = trans['error']
                                resultado_img.transformaciones_aplicadas.append(trans_result)
                        
                        response.resultados.append(resultado_img)
            
            print(f"✅ Procesamiento completado. Imágenes: {response.total_procesadas}")
            return response
                
        except Exception as e:
            import traceback
            error_details = traceback.format_exc()
            print(f"❌ Error en SOAP service: {error_details}")
            
            # Devolver respuesta de error
            response = ProcesamientoResultType()
            response.success = "false"
            response.mensaje = f"Error en el servicio: {str(e)}"
            response.total_procesadas = 0
            response.user_id = request.user_id if 'request' in locals() else 0
            response.resultados = []
            
            return response
    
    @rpc(Unicode, Unicode, _returns=LoginResultType)
    def login(ctx, username, password):
        """
        Inicio de sesión de usuario
        """
        try:
            print(f"🔐 Intento de login: {username}")
            
            resultado = login_usuario(username, password)
            
            response = LoginResultType()
            response.success = "true" if resultado.get('success') else "false"
            response.mensaje = resultado.get('message', '')
            response.user_id = resultado.get('user_id', 0)
            response.username = username
            
            if resultado.get('success'):
                print(f"✅ Login exitoso para usuario: {username} (ID: {resultado.get('user_id')})")
            else:
                print(f"❌ Login fallido para usuario: {username}")
            
            return response
            
        except Exception as e:
            print(f"❌ Error en login: {e}")
            
            response = LoginResultType()
            response.success = "false"
            response.mensaje = f"Error en el servicio: {str(e)}"
            response.user_id = 0
            response.username = username
            
            return response
    
    @rpc(Unicode, Unicode, _returns=RegisterResultType)
    def registrar_usuario(ctx, username, password):
        """
        Registro de nuevo usuario
        """
        try:
            print(f"👤 Registro de usuario: {username}")
            
            resultado = registrar_usuario(username, password)
            
            response = RegisterResultType()
            response.success = "true" if resultado.get('success') else "false"
            response.mensaje = resultado.get('message', '')
            response.user_id = resultado.get('user_id', 0)
            response.username = username
            
            if resultado.get('success'):
                print(f"✅ Registro exitoso para usuario: {username} (ID: {resultado.get('user_id')})")
            else:
                print(f"❌ Registro fallido para usuario: {username}")
            
            return response
            
        except Exception as e:
            print(f"❌ Error en registro: {e}")
            
            response = RegisterResultType()
            response.success = "false"
            response.mensaje = f"Error en el servicio: {str(e)}"
            response.user_id = 0
            response.username = username
            
            return response
    
    @rpc(_returns=Unicode)
    def obtener_estado_servidor(ctx):
        """Verifica el estado del servidor Pyro4"""
        from app.services.node_service import obtener_estado_servidor
        
        estado = obtener_estado_servidor()
        if estado['success']:
            return f"✅ Servidor Pyro4 conectado - {estado['detalles']['estado']}"
        else:
            return f"❌ Servidor Pyro4 desconectado: {estado['error']}"

# Configuración de la aplicación SOAP con namespaces correctos
soap_app = Application(
    [ImageProcessingService],
    tns='server.soap.service',
    in_protocol=Soap11(validator='lxml'),
    out_protocol=Soap11(),
    name='ImageProcessingService'
)

wsgi_app = WsgiApplication(soap_app)