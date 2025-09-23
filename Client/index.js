const axios = require("axios");
const { parseString } = require("xml2js");
const { stripPrefix } = require("xml2js/lib/processors");

const soapRequest = `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                xmlns:ser="server.soap.service">
    <soapenv:Header/>
    <soapenv:Body>
        <ser:say_hello>
            <ser:message>Hola</ser:message>
        </ser:say_hello>
    </soapenv:Body>
</soapenv:Envelope>`;

const getSaludoRequest = `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                xmlns:ser="server.soap.service">
    <soapenv:Header/>
    <soapenv:Body>
        <ser:get_saludo/>
    </soapenv:Body>
</soapenv:Envelope>`;

const SOAP_URL = "http://127.0.0.1:5000/client/soap";

async function callSoapService() {
try {
    const { data } = await axios.post(SOAP_URL, soapRequest, {
    headers: {
        "Content-Type": "text/xml;charset=UTF-8",
        SOAPAction: "",
    },
    });

    console.log("Respuesta SOAP RAW:");
    console.log(data);

    parseString(
    data,
    { explicitArray: false, tagNameProcessors: [stripPrefix] },
    (err, result) => {
        if (err) {
            console.error("Error al parsear XML:", err);
            return;
        }

        const response =
            result.Envelope.Body.say_helloResponse.say_helloResult;

        console.log("✅ Respuesta parseada:", response);
        }
    );
} catch (error) {
    console.error("Error en la petición SOAP:", error.message);
}
}

async function callGetSaludo() {
try {
    const { data } = await axios.post(SOAP_URL, getSaludoRequest, {
        headers: {
            "Content-Type": "text/xml;charset=UTF-8",
            SOAPAction: "",
        },
    });

    console.log("Respuesta SOAP RAW:");
    console.log(data);

    parseString(
        data,
        { explicitArray: false, tagNameProcessors: [stripPrefix] },
        (err, result) => {
        if (err) {
            console.error("Error al parsear XML:", err);
            return;
        }

        const response =
            result.Envelope.Body.get_saludoResponse.get_saludoResult;

        console.log("Respuesta parseada:", response);
        }
    );
} catch (error) {
    console.error("Error en la petición SOAP:", error.message);
}
}

callGetSaludo();

//callSoapService();
