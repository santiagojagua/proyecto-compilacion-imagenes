module.exports = {
    SERVER: {
        PORT: 3000,
        SOAP_URL: "http://127.0.0.1:5000/client/soap"
    },
    MESSAGES: {
        AUTH: {
            TOKEN_REQUIRED: 'Token de autorización requerido',
            INVALID_TOKEN: 'Token inválido o sesión expirada',
            LOGIN_SUCCESS: 'Login exitoso',
            LOGOUT_SUCCESS: 'Sesión cerrada correctamente',
            REGISTER_SUCCESS: 'Usuario registrado exitosamente'
        },
        VALIDATION: {
            USERNAME_PASSWORD_REQUIRED: 'Username y password son requeridos',
            IMAGES_REQUIRED: 'El campo imagenes es requerido y debe ser un array no vacío'
        },
        ERRORS: {
            INTERNAL_SERVER: 'Error interno del servidor',
            ROUTE_NOT_FOUND: 'Ruta no encontrada',
            SOAP_CONNECTION_FAILED: 'Error en la conexión con el servidor SOAP'
        }
    },
    SOAP_ACTIONS: {
        REGISTER: 'registrar_usuario',
        LOGIN: 'login',
        PROCESS_IMAGES: 'procesar_imagen_cambios',
        GET_STATUS: 'obtener_estado_servidor'
    }
};