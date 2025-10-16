const express = require('express');
const { SERVER, MESSAGES } = require('./config/constants');

// Importar rutas
const authRoutes = require('./routes/auth');
const imageRoutes = require('./routes/images');
const systemRoutes = require('./routes/system');

const app = express();

// Middleware
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true, limit: '50mb' }));

// Configurar rutas
app.use('/api/auth', authRoutes);
app.use('/api/images', imageRoutes);
app.use('/api/system', systemRoutes);

// Manejo de errores
app.use((error, req, res, next) => {
    console.error("Error no manejado:", error);
    res.status(500).json({
        success: false,
        error: MESSAGES.ERRORS.INTERNAL_SERVER,
        details: error.message
    });
});

// Ruta no encontrada
app.use("*", (req, res) => {
    res.status(404).json({
        success: false,
        error: MESSAGES.ERRORS.ROUTE_NOT_FOUND
    });
});

// Iniciar servidor
app.listen(SERVER.PORT, () => {
    console.log(`🚀 Servidor REST ejecutándose en http://localhost:${SERVER.PORT}`);
    console.log(`📋 Endpoints disponibles:`);
    console.log(`   AUTH:`);
    console.log(`     POST /api/auth/register     - Registro de usuario`);
    console.log(`     POST /api/auth/login        - Inicio de sesión`);
    console.log(`     POST /api/auth/logout       - Cerrar sesión (requiere token)`);
    console.log(`     GET  /api/auth/me           - Info usuario (requiere token)`);
    console.log(`   IMAGES:`);
    console.log(`     POST /api/images/process-images - Procesar imágenes (requiere token)`);
    console.log(`   SYSTEM:`);
    console.log(`     GET  /api/system/health     - Estado del servidor`);
    console.log(`     GET  /api/system/soap-status - Estado servidor SOAP`);
    console.log('');
    console.log('🔐 Autenticación: Incluir header "Authorization: <token>" en requests protegidos');
});