function formatImageProcessingResponse(soapResult, userId) {
    const formattedResponse = {
        success: soapResult.success === 'true' || soapResult.success === true,
        total_processed: soapResult.total_procesadas || 0,
        message: soapResult.mensaje || '',
        user_id: userId,
        timestamp: new Date().toISOString(),
        results: []
    };

    if (soapResult.resultados && soapResult.resultados.ResultadoImagenType) {
        const resultadosArray = Array.isArray(soapResult.resultados.ResultadoImagenType) 
            ? soapResult.resultados.ResultadoImagenType 
            : [soapResult.resultados.ResultadoImagenType];

        formattedResponse.results = resultadosArray.map(imgResult => ({
            original_name: imgResult.nombre_original,
            status: imgResult.estado,
            processed_image_base64: imgResult.imagen_procesada_base64,
            output_format: imgResult.formato_salida,
            final_dimensions: imgResult.dimensiones_finales,
            saved_filename: imgResult.archivo_guardado,
            transformations_applied: processTransformations(imgResult.transformaciones_aplicadas)
        }));
    }

    return formattedResponse;
}

function processTransformations(transformations) {
    if (!transformations) return [];
    
    const transformationsArray = Array.isArray(transformations.ResultadoTransformacionType) 
        ? transformations.ResultadoTransformacionType 
        : [transformations.ResultadoTransformacionType];

    return transformationsArray.map(trans => ({
        transformation: trans.transformacion,
        order: trans.numero,
        status: trans.estado,
        error: trans.error || null
    }));
}

function formatAuthResponse(soapResult, username, token = null) {
    const response = {
        success: soapResult.success === 'true' || soapResult.success === true,
        message: soapResult.mensaje,
        user_id: soapResult.user_id,
        username: username,
        timestamp: new Date().toISOString()
    };

    if (token) {
        response.token = token;
    }

    return response;
}

function formatErrorResponse(message, details = null) {
    const response = {
        success: false,
        error: message,
        timestamp: new Date().toISOString()
    };

    if (details) {
        response.details = details;
    }

    return response;
}

module.exports = {
    formatImageProcessingResponse,
    formatAuthResponse,
    formatErrorResponse,
    processTransformations
};