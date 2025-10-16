const axios = require('axios');
const fs = require('fs');
const path = require('path');
const { SERVER } = require('../config/constants');

const API_BASE = `http://localhost:${SERVER.PORT}/api`;
const IMAGES_DIR = path.join(__dirname, '..', 'images');

class SimpleTestClient {
    constructor() {
        this.token = null;
    }

    async login() {
        const username = `testuser_${Date.now()}`;
        const password = 'testpass123';
        
        // Registrar
        await axios.post(`${API_BASE}/auth/register`, { username, password });
        
        // Login
        const response = await axios.post(`${API_BASE}/auth/login`, { username, password });
        this.token = response.data.token;
        console.log('✅ Login exitoso');
        return response.data.user_id;
    }

    async testSingleImage(imageFilename) {
        if (!this.token) {
            await this.login();
        }

        // Cargar una sola imagen
        const imagePath = path.join(IMAGES_DIR, imageFilename);
        const imageBuffer = fs.readFileSync(imagePath);
        const base64 = imageBuffer.toString('base64');
        
        const ext = path.extname(imageFilename).toLowerCase();
        let fileType = 'jpg';
        if (ext === '.png') fileType = 'png';
        else if (ext === '.gif') fileType = 'gif';
        else if (ext === '.bmp') fileType = 'bmp';
        else if (ext === '.webp') fileType = 'webp';

        const testImage = [{
            nombre: imageFilename,
            tipo: fileType,
            contenido_base64: base64,
            transformaciones: [
                {
                    nombre: 'escala_grises',
                    especificaciones: ''
                }
            ]
        }];

        console.log(`🖼️ Probando con 1 imagen: ${imageFilename} (${(imageBuffer.length / 1024).toFixed(2)} KB)`);
        
        try {
            const response = await axios.post(`${API_BASE}/images/process-images`, {
                imagenes: testImage
            }, {
                headers: {
                    'Authorization': this.token
                }
            });
            
            console.log('✅ Resultado:', response.data);
            return response.data;
        } catch (error) {
            console.log('❌ Error:', error.response?.data || error.message);
            return null;
        }
    }
}

// Probar con la imagen más pequeña
async function testWithSmallestImage() {
    const client = new SimpleTestClient();
    
    // Encontrar la imagen más pequeña
    const files = fs.readdirSync(IMAGES_DIR);
    const imageFiles = files.filter(file => {
        const ext = path.extname(file).toLowerCase();
        return ['.jpg', '.jpeg', '.png', '.gif', '.bmp', '.webp'].includes(ext);
    });
    
    // Ordenar por tamaño
    const imagesWithSize = imageFiles.map(file => {
        const stats = fs.statSync(path.join(IMAGES_DIR, file));
        return { file, size: stats.size };
    }).sort((a, b) => a.size - b.size);
    
    if (imagesWithSize.length > 0) {
        const smallestImage = imagesWithSize[0];
        console.log(`🔍 Imagen más pequeña: ${smallestImage.file} (${(smallestImage.size / 1024).toFixed(2)} KB)`);
        await client.testSingleImage(smallestImage.file);
    }
}

testWithSmallestImage().catch(console.error);