const express = require('express');
const router = express.Router();
const soapClient = require('../services/soapClient');
const { formatErrorResponse } = require('../utils/responseFormatter');

// Health check
router.get('/health', (req, res) => {
    res.json({ 
        status: "OK", 
        message: "Servidor REST funcionando correctamente",
        timestamp: new Date().toISOString(),
        endpoints: {
            auth: ["POST /api/auth/register", "POST /api/auth/login"],
            images: ["POST /api/images/process-images (requiere autenticación)"],
            system: ["GET /api/system/health", "GET /api/system/soap-status"]
        }
    });
});

// Estado del servidor SOAP
router.get('/soap-status', async (req, res) => {
    try {
        const result = await soapClient.getServerStatus();

        res.json({
            success: true,
            estado: "conectado",
            respuesta: result,
            timestamp: new Date().toISOString()
        });

    } catch (error) {
        res.json({
            success: false,
            estado: "desconectado",
            error: error.message,
            timestamp: new Date().toISOString()
        });
    }
});

module.exports = router;