const express = require('express');
const router = express.Router();
const soapClient = require('../services/soapClient');
const { authenticateToken } = require('../middleware/auth');
const { formatImageProcessingResponse, formatErrorResponse } = require('../utils/responseFormatter');
const { MESSAGES } = require('../config/constants');

// Procesar imágenes (requiere autenticación)
router.post('/process-images', authenticateToken, async (req, res) => {
    try {
        const { imagenes } = req.body;
        const user_id = req.user.user_id;

        if (!imagenes || !Array.isArray(imagenes) || imagenes.length === 0) {
            return res.status(400).json(
                formatErrorResponse(MESSAGES.VALIDATION.IMAGES_REQUIRED)
            );
        }

        console.log(`📤 Procesando ${imagenes.length} imágenes para usuario ${user_id} (${req.user.username})`);

        const result = await soapClient.processImages(user_id, imagenes);
        const formattedResponse = formatImageProcessingResponse(result, user_id);

        res.json(formattedResponse);

    } catch (error) {
        console.error("Error procesando imágenes:", error.message);
        res.status(500).json(
            formatErrorResponse(error.message, "Error en el procesamiento de imágenes")
        );
    }
});

module.exports = router;