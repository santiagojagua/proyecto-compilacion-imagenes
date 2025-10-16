const express = require('express');
const router = express.Router();
const soapClient = require('../services/soapClient');
const sessionManager = require('../services/sessionManager');
const { authenticateToken } = require('../middleware/auth');
const { formatAuthResponse, formatErrorResponse } = require('../utils/responseFormatter');
const { MESSAGES } = require('../config/constants');

// Registro de usuario
router.post('/register', async (req, res) => {
    try {
        const { username, password } = req.body;

        if (!username || !password) {
            return res.status(400).json(
                formatErrorResponse(MESSAGES.VALIDATION.USERNAME_PASSWORD_REQUIRED)
            );
        }

        console.log(`📝 Registrando usuario: ${username}`);

        const result = await soapClient.registerUser(username, password);
        
        res.json(formatAuthResponse(result, username));

    } catch (error) {
        console.error("Error en registro:", error.message);
        res.status(500).json(
            formatErrorResponse(error.message, "Error en el registro de usuario")
        );
    }
});

// Login de usuario
router.post('/login', async (req, res) => {
    try {
        const { username, password } = req.body;

        if (!username || !password) {
            return res.status(400).json(
                formatErrorResponse(MESSAGES.VALIDATION.USERNAME_PASSWORD_REQUIRED)
            );
        }

        console.log(`🔐 Login attempt: ${username}`);

        const result = await soapClient.loginUser(username, password);

        if (result.success === 'true' || result.success === true) {
            const token = sessionManager.createSession({
                user_id: result.user_id,
                username: username
            });

            res.json(formatAuthResponse(result, username, token));
        } else {
            res.status(401).json(formatAuthResponse(result, username));
        }

    } catch (error) {
        console.error("Error en login:", error.message);
        res.status(500).json(
            formatErrorResponse(error.message, "Error en el inicio de sesión")
        );
    }
});

// Logout
router.post('/logout', authenticateToken, (req, res) => {
    const token = req.headers['authorization'];
    sessionManager.deleteSession(token);
    
    res.json({
        success: true,
        message: MESSAGES.AUTH.LOGOUT_SUCCESS,
        timestamp: new Date().toISOString()
    });
});

// Información del usuario actual
router.get('/me', authenticateToken, (req, res) => {
    res.json({
        success: true,
        user: req.user,
        timestamp: new Date().toISOString()
    });
});

module.exports = router;