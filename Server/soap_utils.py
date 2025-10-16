def generate_soap_request(user_id, imagenes):
    """
    Genera un XML SOAP correctamente formateado para procesar imágenes
    """
    imagenes_xml = ""
    
    for img in imagenes:
        cambios_xml = ""
        for cambio in img.get('cambios', []):
            cambios_xml += f'''<ser:CambioType>
    <ser:nombre>{cambio['nombre']}</ser:nombre>
    <ser:especificaciones>{cambio.get('especificaciones', '')}</ser:especificaciones>
</ser:CambioType>'''
        
        # Log informativo
        base64_preview = img['contenido_base64'][:30] + "..." if len(img['contenido_base64']) > 30 else img['contenido_base64']
        print(f"📷 Imagen: {img['nombre']}, Base64: {base64_preview}")
        print(f"   Transformaciones: {[c['nombre'] for c in img.get('cambios', [])]}")
        
        imagenes_xml += f'''<ser:ImagenType>
    <ser:nombre>{img['nombre']}</ser:nombre>
    <ser:tipo>{img['tipo']}</ser:tipo>
    <ser:contenido_base64>{img['contenido_base64']}</ser:contenido_base64>
    <ser:cambios>
        {cambios_xml}
    </ser:cambios>
</ser:ImagenType>'''
    
    soap_xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:procesar_imagen_cambios>
         <ser:request>
            <ser:user_id>{user_id}</ser:user_id>
            <ser:imagenes>
                {imagenes_xml}
            </ser:imagenes>
         </ser:request>
      </ser:procesar_imagen_cambios>
   </soapenv:Body>
</soapenv:Envelope>'''
    
    return soap_xml

def generate_login_soap(username, password):
    """
    Genera XML SOAP para inicio de sesión
    """
    soap_xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:login>
         <ser:username>{username}</ser:username>
         <ser:password>{password}</ser:password>
      </ser:login>
   </soapenv:Body>
</soapenv:Envelope>'''
    
    return soap_xml

def generate_register_soap(username, password):
    """
    Genera XML SOAP para registro de usuario
    """
    soap_xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:registrar_usuario>
         <ser:username>{username}</ser:username>
         <ser:password>{password}</ser:password>
      </ser:registrar_usuario>
   </soapenv:Body>
</soapenv:Envelope>'''
    
    return soap_xml