const express = require("express");
const axios = require("axios");
const { parseString } = require("xml2js");
const { stripPrefix } = require("xml2js/lib/processors");
const multer = require("multer");
const fs = require("fs");
const path = require("path");

const app = express();
const PORT = 3000;

// Middleware
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true, limit: '50mb' }));

// Configuración de multer para subida de archivos
const storage = multer.memoryStorage();
const upload = multer({ 
  storage: storage,
  limits: {
    fileSize: 10 * 1024 * 1024, // 10MB límite
  }
});

// URL del servicio SOAP Flask
const SOAP_URL = "http://127.0.0.1:5000/client/soap";

// Función para convertir buffer a base64
function bufferToBase64(buffer) {
  return buffer.toString('base64');
}

// Función para generar XML SOAP para procesamiento de imágenes
function generateSoapRequest(userId, imagenes) {
  let imagenesXml = "";

  imagenes.forEach((img, index) => {
    let cambiosXml = "";
    
    img.transformaciones.forEach((cambio) => {
      cambiosXml += `
        <ser:CambioType>
          <ser:nombre>${cambio.nombre}</ser:nombre>
          <ser:especificaciones>${cambio.especificaciones || ""}</ser:especificaciones>
        </ser:CambioType>`;
    });

    imagenesXml += `
      <ser:ImagenType>
        <ser:nombre>${img.nombre}</ser:nombre>
        <ser:tipo>${img.tipo}</ser:tipo>
        <ser:contenido_base64>${img.contenido_base64}</ser:contenido_base64>
        <ser:cambios>${cambiosXml}
        </ser:cambios>
      </ser:ImagenType>`;
  });

  const soapXml = `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:procesar_imagen_cambios>
         <ser:request>
            <ser:user_id>${userId}</ser:user_id>
            <ser:imagenes>${imagenesXml}
            </ser:imagenes>
         </ser:request>
      </ser:procesar_imagen_cambios>
   </soapenv:Body>
</soapenv:Envelope>`;

  return soapXml;
}

// Función para parsear respuesta SOAP
function parseSoapResponse(xmlData) {
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
          // Extraer la respuesta del cuerpo SOAP
          const body = result.Envelope.Body;
          let response;
          
          if (body.procesar_imagen_cambiosResponse) {
            response = body.procesar_imagen_cambiosResponse.procesar_imagen_cambiosResult;
          } else if (body.Fault) {
            response = {
              error: true,
              faultcode: body.Fault.faultcode,
              faultstring: body.Fault.faultstring
            };
          } else {
            response = body;
          }
          
          resolve(response);
        } catch (parseErr) {
          reject(parseErr);
        }
      }
    );
  });
}

// ===== ENDPOINTS API REST =====

// Endpoint de salud
app.get("/api/health", (req, res) => {
  res.json({ 
    status: "OK", 
    message: "Servidor REST funcionando correctamente",
    timestamp: new Date().toISOString()
  });
});

// Endpoint para procesar imágenes desde JSON
app.post("/api/procesar-imagenes", async (req, res) => {
  try {
    const { user_id, imagenes } = req.body;

    // Validaciones
    if (!user_id) {
      return res.status(400).json({
        success: false,
        error: "El campo user_id es requerido"
      });
    }

    if (!imagenes || !Array.isArray(imagenes) || imagenes.length === 0) {
      return res.status(400).json({
        success: false,
        error: "El campo imagenes es requerido y debe ser un array no vacío"
      });
    }

    console.log(`Procesando ${imagenes.length} imágenes para usuario ${user_id}`);

    // Generar XML SOAP
    const soapXml = generateSoapRequest(user_id, imagenes);

    // Enviar petición SOAP
    const soapResponse = await axios.post(SOAP_URL, soapXml, {
      headers: {
        "Content-Type": "text/xml;charset=UTF-8",
        "SOAPAction": "procesar_imagen_cambios",
      },
    });

    // Parsear respuesta SOAP
    const parsedResponse = await parseSoapResponse(soapResponse.data);

    res.json({
      success: true,
      data: parsedResponse,
      metadata: {
        imagenes_procesadas: imagenes.length,
        user_id: user_id,
        timestamp: new Date().toISOString()
      }
    });

  } catch (error) {
    console.error("Error en /api/procesar-imagenes:", error.message);
    
    res.status(500).json({
      success: false,
      error: error.message,
      details: "Error al procesar las imágenes via SOAP"
    });
  }
});

// Endpoint para subir archivos y procesarlos
app.post("/api/subir-imagenes", upload.array("imagenes", 10), async (req, res) => {
  try {
    const { user_id, transformaciones } = req.body;
    const files = req.files;

    if (!user_id) {
      return res.status(400).json({
        success: false,
        error: "El campo user_id es requerido"
      });
    }

    if (!files || files.length === 0) {
      return res.status(400).json({
        success: false,
        error: "No se subieron archivos de imagen"
      });
    }

    // Parsear transformaciones si vienen como string JSON
    let transformacionesArray = [];
    try {
      transformacionesArray = transformaciones ? JSON.parse(transformaciones) : [];
    } catch (e) {
      transformacionesArray = [
        { nombre: "escala_grises", especificaciones: "" },
        { nombre: "redimensionar", especificaciones: "ancho=800,alto=600" }
      ];
    }

    // Preparar imágenes para SOAP
    const imagenesParaSoap = files.map(file => {
      const extension = path.extname(file.originalname).toLowerCase().substring(1);
      const nombreSinExtension = path.basename(file.originalname, path.extname(file.originalname));
      
      return {
        nombre: file.originalname,
        tipo: extension || 'jpg',
        contenido_base64: bufferToBase64(file.buffer),
        transformaciones: transformacionesArray
      };
    });

    console.log(`Procesando ${imagenesParaSoap.length} imágenes subidas para usuario ${user_id}`);

    // Generar XML SOAP
    const soapXml = generateSoapRequest(user_id, imagenesParaSoap);

    // Enviar petición SOAP
    const soapResponse = await axios.post(SOAP_URL, soapXml, {
      headers: {
        "Content-Type": "text/xml;charset=UTF-8",
        "SOAPAction": "procesar_imagen_cambios",
      },
    });

    // Parsear respuesta SOAP
    const parsedResponse = await parseSoapResponse(soapResponse.data);

    res.json({
      success: true,
      data: parsedResponse,
      metadata: {
        archivos_procesados: files.length,
        user_id: user_id,
        timestamp: new Date().toISOString()
      }
    });

  } catch (error) {
    console.error("Error en /api/subir-imagenes:", error.message);
    
    res.status(500).json({
      success: false,
      error: error.message,
      details: "Error al procesar las imágenes subidas"
    });
  }
});

// Endpoint para verificar estado del servidor SOAP
app.get("/api/estado-soap", async (req, res) => {
  try {
    const soapXml = `<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                  xmlns:ser="server.soap.service">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:obtener_estado_servidor/>
   </soapenv:Body>
</soapenv:Envelope>`;

    const soapResponse = await axios.post(SOAP_URL, soapXml, {
      headers: {
        "Content-Type": "text/xml;charset=UTF-8",
        "SOAPAction": "obtener_estado_servidor",
      },
    });

    const parsedResponse = await parseSoapResponse(soapResponse.data);

    res.json({
      success: true,
      estado: "conectado",
      respuesta: parsedResponse,
      timestamp: new Date().toISOString()
    });

  } catch (error) {
    console.error("Error verificando estado SOAP:", error.message);
    
    res.json({
      success: false,
      estado: "desconectado",
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

// Manejo de errores
app.use((error, req, res, next) => {
  console.error("Error no manejado:", error);
  res.status(500).json({
    success: false,
    error: "Error interno del servidor",
    details: error.message
  });
});

// Ruta no encontrada
app.use("*", (req, res) => {
  res.status(404).json({
    success: false,
    error: "Ruta no encontrada"
  });
});

// Iniciar servidor
app.listen(PORT, () => {
  console.log(`🚀 Servidor REST ejecutándose en http://localhost:${PORT}`);
  console.log(`📋 Endpoints disponibles:`);
  console.log(`   GET  /api/health`);
  console.log(`   GET  /api/estado-soap`);
  console.log(`   POST /api/procesar-imagenes`);
  console.log(`   POST /api/subir-imagenes`);
});