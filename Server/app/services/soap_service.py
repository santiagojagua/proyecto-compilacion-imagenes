from app.models.entitys import User
from spyne import Application, rpc, ServiceBase, Unicode, ComplexModel, Array, Integer
from spyne.protocol.soap import Soap11
from spyne.server.wsgi import WsgiApplication
from app.services.db_service import procesar_imagenes, registrar_imagenes_en_db, registrar_usuario, login_usuario
from app import db
from spyne.model.primitive import AnyXml
from lxml import etree

# ===== MODELOS SOAP =====

class CambioType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    especificaciones = Unicode

# Wrapper para <ser:cambios> que contiene <ser:CambioType>*
class CambiosList(ComplexModel):
    __namespace__ = "server.soap.service"
    CambioType = Array(CambioType, sub_name='CambioType')

class ImagenType(ComplexModel):
    __namespace__ = "server.soap.service"
    nombre = Unicode
    tipo = Unicode
    contenido_base64 = Unicode
    # Ahora el campo 'cambios' es un wrapper complejo
    cambios = CambiosList

# Wrapper para <ser:imagenes> que contiene <ser:ImagenType>*
class ImagenesList(ComplexModel):
    __namespace__ = "server.soap.service"
    ImagenType = Array(ImagenType, sub_name='ImagenType')

class ImagenCambiosType(ComplexModel):
    __namespace__ = "server.soap.service"
    user_id = Integer
    # Ahora 'imagenes' es un wrapper complejo
    imagenes = ImagenesList

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

    @rpc(_returns=ProcesamientoResultType)
    def procesar_imagen_cambios(ctx):
        """
        Lee el sobre SOAP completo desde ctx.in_document y parsea:
        <ser:procesar_imagen_cambios>/<ser:request>/<ser:user_id> y <ser:imagenes>/<ser:ImagenType>*
        """
        try:
            print("="*50)
            print("📦 SOAP Request recibido (ctx.in_document)")

            ns = {
                'soap': 'http://schemas.xmlsoap.org/soap/envelope/',
                'ser': 'server.soap.service',
            }

            # Documento completo
            doc = ctx.in_document
            # 1) Ubicar el nodo <ser:request>
            req = doc.find('.//ser:procesar_imagen_cambios/ser:request', namespaces=ns)
            if req is None:
                # Fallback, por si cambia el layout
                req = doc.find('.//ser:request', namespaces=ns)
            if req is None:
                raise ValueError("No se encontró el elemento <ser:request> en el SOAP")

            # 2) user_id
            user_id_text = req.findtext('ser:user_id', namespaces=ns)
            if not user_id_text:
                user_id_text = req.findtext('.//ser:user_id', namespaces=ns)
            user_id = int(user_id_text) if user_id_text else 0

            # 3) imágenes: con wrapper <ser:imagenes> o items directos
            imagenes_el = req.find('ser:imagenes', namespaces=ns)
            if imagenes_el is not None:
                img_nodes = imagenes_el.findall('ser:ImagenType', namespaces=ns)
            else:
                img_nodes = req.findall('ser:ImagenType', namespaces=ns)

            print(f"User ID: {user_id} - Número de imágenes: {len(img_nodes)}")

            if user_id <= 0:
                raise ValueError(f"Usuario con ID {user_id} no válido en la petición SOAP")
            if not img_nodes:
                raise ValueError("No se encontraron imágenes en la petición SOAP")

            # 4) Construir data_dict para BD + Pyro
            data_dict = {'user_id': user_id, 'imagenes': []}

            for img_el in img_nodes:
                nombre = img_el.findtext('ser:nombre', namespaces=ns) or ''
                tipo = img_el.findtext('ser:tipo', namespaces=ns) or ''
                b64   = img_el.findtext('ser:contenido_base64', namespaces=ns) or ''

                cambios = []
                cambios_el = img_el.find('ser:cambios', namespaces=ns)
                if cambios_el is not None:
                    for c_el in cambios_el.findall('ser:CambioType', namespaces=ns):
                        cambios.append({
                            'nombre': c_el.findtext('ser:nombre', namespaces=ns) or '',
                            'especificaciones': c_el.findtext('ser:especificaciones', namespaces=ns) or ''
                        })
                else:
                    # Fallback por si vinieran <ser:CambioType> directos
                    for c_el in img_el.findall('ser:CambioType', namespaces=ns):
                        cambios.append({
                            'nombre': c_el.findtext('ser:nombre', namespaces=ns) or '',
                            'especificaciones': c_el.findtext('ser:especificaciones', namespaces=ns) or ''
                        })

                data_dict['imagenes'].append({
                    'nombre': nombre,
                    'tipo': tipo,
                    'contenido_base64': b64,
                    'cambios': cambios
                })

                print(f"   📷 {nombre} ({tipo}) - b64: {len(b64)} chars - cambios: {[c['nombre'] for c in cambios]}")

            # 5) Registrar en BD
            print("📝 Registrando en base de datos...")
            registro_db = registrar_imagenes_en_db(data_dict)
            if not registro_db.get('success'):
                raise Exception(f"Error registrando en BD: {registro_db.get('message')}")
            print(f"✅ Registro en BD exitoso. IDs: {registro_db.get('ids_imagenes', [])}")

            # 6) Enviar a Pyro
            from app.models.entitys import User
            usuario = User.query.get(user_id)
            usuario_nombre = usuario.username if usuario else "usuario_desconocido"
            print("🔄 Enviando imágenes a servidor Pyro4...")

            result_pyro = procesar_imagenes(data_dict, user_id, usuario_nombre)
            print(f"📡 Respuesta de Pyro4: {result_pyro}")

            # 7) Construir respuesta SOAP
            response = ProcesamientoResultType()
            response.success = "true" if result_pyro.get('success') else "false"
            response.mensaje = result_pyro.get('mensaje', '')
            response.user_id = user_id
            response.total_procesadas = 0
            response.resultados = []

            def _map_estado_imagen(status: str) -> str:
                s = (status or '').lower()
                return 'completado' if s in ('success', 'ok', 'completed', 'completado') else 'error'

            if result_pyro.get('success') and 'resultado' in result_pyro:
                pyro_result = result_pyro['resultado']
                response.total_procesadas = pyro_result.get('total_procesadas', 0)

                # Opcional: indexar la solicitud original por nombre para inferir formato (si quieres)
                solicitudes_por_nombre = {img['nombre']: img for img in data_dict.get('imagenes', [])}

                for img_result in pyro_result.get('resultados', []):
                    ri = ResultadoImagenType()
                    nombre = img_result.get('nombre', '')
                    ri.nombre_original = nombre

                    # Estado a nivel imagen: success -> completado
                    ri.estado = _map_estado_imagen(img_result.get('status'))

                    # Dimensiones, base64 y archivo (si aplica)
                    ri.dimensiones_finales = img_result.get('dimensiones_procesadas', '')
                    ri.imagen_procesada_base64 = img_result.get('imagen_procesada', '') or ''
                    ri.archivo_guardado = img_result.get('processed', '') or ''

                    # Formato de salida: por defecto JPEG (cv2.imencode('.jpg', ...))
                    formato = 'JPEG'

                    # (Opcional) Intentar leer "convertir_formato" de la solicitud original
                    sol = solicitudes_por_nombre.get(nombre)
                    if sol:
                        for c in sol.get('cambios', []):
                            if c.get('nombre') == 'convertir_formato':
                                espec = c.get('especificaciones', '')
                                # parseo simple k=v,k=v
                                for token in espec.split(','):
                                    k, _, v = token.partition('=')
                                    if k.strip().lower() == 'formato' and v.strip():
                                        formato = v.strip().upper()
                                        break

                    ri.formato_salida = formato

                    # Transformaciones aplicadas: marcar como 'completada'
                    ri.transformaciones_aplicadas = []
                    aplicadas = img_result.get('transformaciones_aplicadas', []) or []
                    for idx, tname in enumerate(aplicadas, start=1):
                        tr = ResultadoTransformacionType()
                        tr.transformacion = tname
                        tr.numero = idx
                        tr.estado = 'completada'  # <- clave para que el cliente muestre ✅
                        ri.transformaciones_aplicadas.append(tr)

                    response.resultados.append(ri)

            print(f"✅ Procesamiento completado. Imágenes: {response.total_procesadas}")
            print("="*50)
            return response

        except Exception as e:
            import traceback
            print("❌ ERROR en SOAP SERVICE (ctx.in_document):")
            print(traceback.format_exc())
            print("="*50)
            resp = ProcesamientoResultType()
            resp.success = "false"
            resp.mensaje = f"Error en el servicio: {str(e)}"
            resp.total_procesadas = 0
            resp.user_id = 0
            resp.resultados = []
            return resp
    
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
    in_protocol=Soap11(validator=None),   # <- sin validación de esquema en entrada
    out_protocol=Soap11(),
    name='ImageProcessingService'
)

wsgi_app = WsgiApplication(soap_app)
wsgi_app.max_content_length = 50 * 1024 * 1024 