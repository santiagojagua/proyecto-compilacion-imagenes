def generate_soap_request(user_id, imagenes):
    """
    Genera un XML SOAP correctamente formateado
    """
    imagenes_xml = ""
    
    for img in imagenes:
        cambios_xml = ""
        for cambio in img.get('cambios', []):
            cambios_xml += f'''<ser:CambioType>
    <ser:nombre>{cambio['nombre']}</ser:nombre>
    <ser:especificaciones>{cambio.get('especificaciones', '')}</ser:especificaciones>
</ser:CambioType>'''
        
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