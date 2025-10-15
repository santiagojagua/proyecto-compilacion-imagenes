const { SOAP_ACTIONS } = require('../config/constants');

function generateSoapXml(soapAction, data) {
    switch (soapAction) {
        case SOAP_ACTIONS.REGISTER:
            return generateRegisterXml(data);
        case SOAP_ACTIONS.LOGIN:
            return generateLoginXml(data);
        case SOAP_ACTIONS.PROCESS_IMAGES:
            return generateProcessImagesXml(data);
        default:
            throw new Error(`Acción SOAP no soportada: ${soapAction}`);
    }
}

function generateRegisterXml(data) {
    return `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:registrar_usuario>
         <ser:username>${data.username}</ser:username>
         <ser:password>${data.password}</ser:password>
      </ser:registrar_usuario>
   </soapenv:Body>
</soapenv:Envelope>`;
}

function generateLoginXml(data) {
    return `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:login>
         <ser:username>${data.username}</ser:username>
         <ser:password>${data.password}</ser:password>
      </ser:login>
   </soapenv:Body>
</soapenv:Envelope>`;
}

function generateProcessImagesXml(data) {
    let imagenesXml = "";
    data.imagenes.forEach((img) => {
        let cambiosXml = "";
        img.transformaciones.forEach((cambio) => {
            cambiosXml += `<ser:CambioType>
        <ser:nombre>${cambio.nombre}</ser:nombre>
        <ser:especificaciones>${cambio.especificaciones || ""}</ser:especificaciones>
    </ser:CambioType>`;
        });

        imagenesXml += `<ser:ImagenType>
    <ser:nombre>${img.nombre}</ser:nombre>
    <ser:tipo>${img.tipo}</ser:tipo>
    <ser:contenido_base64>${img.contenido_base64}</ser:contenido_base64>
    <ser:cambios>
        ${cambiosXml}
    </ser:cambios>
</ser:ImagenType>`;
    });

    return `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:procesar_imagen_cambios>
         <ser:request>
            <ser:user_id>${data.user_id}</ser:user_id>
            <ser:imagenes>
                ${imagenesXml}
            </ser:imagenes>
         </ser:request>
      </ser:procesar_imagen_cambios>
   </soapenv:Body>
</soapenv:Envelope>`;
}

module.exports = {
    generateSoapXml,
    generateRegisterXml,
    generateLoginXml,
    generateProcessImagesXml
};