const axios = require('axios');
const { parseString } = require('xml2js');
const { stripPrefix } = require('xml2js/lib/processors');
const { SERVER, SOAP_ACTIONS } = require('../config/constants');
const { generateSoapXml } = require('../utils/soapUtils');

class SoapClient {
    constructor() {
        this.soapUrl = SERVER.SOAP_URL;
    }

    async sendRequest(soapAction, data) {
        try {
            const soapXml = generateSoapXml(soapAction, data);
            
            const response = await axios.post(this.soapUrl, soapXml, {
                headers: {
                    "Content-Type": "text/xml;charset=UTF-8",
                    "SOAPAction": soapAction,
                },
            });

            return await this.parseResponse(response.data);
        } catch (error) {
            console.error(`Error en SOAP request (${soapAction}):`, error.message);
            throw error;
        }
    }

    async parseResponse(xmlData) {
        return new Promise((resolve, reject) => {
            parseString(
                xmlData,
                { 
                    explicitArray: false, 
                    tagNameProcessors: [stripPrefix],
                    trim: true
                },
                (err, result) => {
                    if (err) {
                        reject(err);
                        return;
                    }

                    try {
                        const body = result.Envelope.Body;
                        let responseData = null;
                        
                        if (body.registrar_usuarioResponse) {
                            responseData = body.registrar_usuarioResponse.registrar_usuarioResult;
                        } else if (body.loginResponse) {
                            responseData = body.loginResponse.loginResult;
                        } else if (body.procesar_imagen_cambiosResponse) {
                            responseData = body.procesar_imagen_cambiosResponse.procesar_imagen_cambiosResult;
                        } else if (body.Fault) {
                            responseData = {
                                success: false,
                                error: true,
                                faultcode: body.Fault.faultcode,
                                faultstring: body.Fault.faultstring
                            };
                        } else {
                            responseData = body;
                        }
                        
                        resolve(responseData);
                    } catch (parseErr) {
                        reject(parseErr);
                    }
                }
            );
        });
    }

    async registerUser(username, password) {
        return await this.sendRequest(SOAP_ACTIONS.REGISTER, { username, password });
    }

    async loginUser(username, password) {
        return await this.sendRequest(SOAP_ACTIONS.LOGIN, { username, password });
    }

    async processImages(userId, images) {
        return await this.sendRequest(SOAP_ACTIONS.PROCESS_IMAGES, {
            user_id: userId,
            imagenes: images
        });
    }

    async getServerStatus() {
        const soapXml = `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:obtener_estado_servidor/>
   </soapenv:Body>
</soapenv:Envelope>`;

        try {
            const response = await axios.post(this.soapUrl, soapXml, {
                headers: {
                    "Content-Type": "text/xml;charset=UTF-8",
                    "SOAPAction": SOAP_ACTIONS.GET_STATUS,
                },
            });

            return await this.parseResponse(response.data);
        } catch (error) {
            throw error;
        }
    }
}

module.exports = new SoapClient();