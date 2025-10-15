const axios = require('axios');
const fs = require('fs');
const path = require('path');
const { SERVER } = require('../config/constants');

const API_BASE = `http://localhost:${SERVER.PORT}/api`;
const IMAGES_DIR = path.join(__dirname, '..', 'images');

class APIClient {
    constructor() {
        this.token = null;
        this.userId = null;
        this.username = null;
    }

    async healthCheck() {
        try {
            const response = await axios.get(`${API_BASE}/system/health`);
            console.log('✅ Health Check:', response.data.message);
            return true;
        } catch (error) {
            console.error('❌ Health Check failed:', error.message);
            return false;
        }
    }

    async register(username, password) {
        try {
            const response = await axios.post(`${API_BASE}/auth/register`, {
                username,
                password
            });
            
            if (response.data.success) {
                console.log('✅ Registro exitoso:', response.data.message);
            } else {
                console.log('❌ Registro fallido:', response.data.error);
            }
            
            return response.data;
        } catch (error) {
            console.error('❌ Error en registro:', error.response?.data || error.message);
            return null;
        }
    }

    async login(username, password) {
        try {
            const response = await axios.post(`${API_BASE}/auth/login`, {
                username,
                password
            });
            
            if (response.data.success) {
                this.token = response.data.token;
                this.userId = response.data.user_id;
                this.username = response.data.username;
                console.log('✅ Login exitoso:', response.data.message);
            } else {
                console.log('❌ Login fallido:', response.data.error);
            }
            
            return response.data;
        } catch (error) {
            console.error('❌ Error en login:', error.response?.data || error.message);
            return null;
        }
    }

    async processImages(images) {
        if (!this.token) {
            console.log('❌ No hay token de autenticación');
            return null;
        }

        try {
            const response = await axios.post(`${API_BASE}/images/process-images`, {
                imagenes: images
            }, {
                headers: {
                    'Authorization': this.token
                }
            });
            
            console.log('✅ Procesamiento de imágenes completado');
            return response.data;
        } catch (error) {
            console.error('❌ Error en procesamiento:', error.response?.data || error.message);
            return null;
        }
    }

    async getUserInfo() {
        if (!this.token) {
            console.log('❌ No hay token de autenticación');
            return null;
        }

        try {
            const response = await axios.get(`${API_BASE}/auth/me`, {
                headers: {
                    'Authorization': this.token
                }
            });
            
            console.log('✅ Información del usuario obtenida');
            return response.data;
        } catch (error) {
            console.error('❌ Error obteniendo info usuario:', error.response?.data || error.message);
            return null;
        }
    }

    async logout() {
        if (!this.token) return;

        try {
            const response = await axios.post(`${API_BASE}/auth/logout`, {}, {
                headers: {
                    'Authorization': this.token
                }
            });
            
            console.log('✅ Logout:', response.data.message);
            this.token = null;
            this.userId = null;
            this.username = null;
        } catch (error) {
            console.error('❌ Error en logout:', error.response?.data || error.message);
        }
    }
}

class ImageProcessor {
    constructor() {
        this.imagesDir = IMAGES_DIR;
    }

    // Verificar si la carpeta images existe
    checkImagesDirectory() {
        if (!fs.existsSync(this.imagesDir)) {
            console.log(`📁 Creando carpeta de imágenes: ${this.imagesDir}`);
            fs.mkdirSync(this.imagesDir, { recursive: true });
            return false;
        }
        return true;
    }

    // Obtener todas las imágenes de la carpeta
    getAllImages() {
        if (!this.checkImagesDirectory()) {
            console.log('ℹ️  Carpeta de imágenes creada. Coloca tus imágenes en la carpeta "images" y ejecuta de nuevo.');
            return [];
        }

        try {
            const files = fs.readdirSync(this.imagesDir);
            const imageFiles = files.filter(file => {
                const ext = path.extname(file).toLowerCase();
                return ['.jpg', '.jpeg', '.png', '.gif', '.bmp', '.webp'].includes(ext);
            });

            console.log(`📷 Encontradas ${imageFiles.length} imágenes en la carpeta:`);
            imageFiles.forEach((file, index) => {
                console.log(`   ${index + 1}. ${file}`);
            });

            return imageFiles;
        } catch (error) {
            console.error('❌ Error leyendo carpeta de imágenes:', error.message);
            return [];
        }
    }

    // Convertir imagen a base64
    imageToBase64(imageFilename) {
        try {
            const imagePath = path.join(this.imagesDir, imageFilename);
            const imageBuffer = fs.readFileSync(imagePath);
            const base64 = imageBuffer.toString('base64');
            
            // Obtener tipo de archivo
            const ext = path.extname(imageFilename).toLowerCase();
            let fileType = 'jpg';
            if (ext === '.png') fileType = 'png';
            else if (ext === '.gif') fileType = 'gif';
            else if (ext === '.bmp') fileType = 'bmp';
            else if (ext === '.webp') fileType = 'webp';

            return {
                base64: base64,
                type: fileType,
                size: imageBuffer.length,
                filename: imageFilename
            };
        } catch (error) {
            console.error(`❌ Error procesando imagen ${imageFilename}:`, error.message);
            return null;
        }
    }

    // Generar transformaciones aleatorias para testing
    generateRandomTransformations() {
        const transformations = [
            // Escala de grises (siempre incluida)
            {
                nombre: 'escala_grises',
                especificaciones: ''
            }
        ];

        // Agregar 1-3 transformaciones adicionales aleatorias
        const additionalTransformations = [
            {
                nombre: 'redimensionar',
                especificaciones: 'ancho=800,alto=600'
            },
            {
                nombre: 'redimensionar', 
                especificaciones: 'ancho=400,alto=300'
            },
            {
                nombre: 'rotar',
                especificaciones: 'angulo=90'
            },
            {
                nombre: 'rotar',
                especificaciones: 'angulo=45'
            },
            {
                nombre: 'reflejar',
                especificaciones: 'tipo=horizontal'
            },
            {
                nombre: 'desenfocar',
                especificaciones: 'radio=2'
            },
            {
                nombre: 'perfilar',
                especificaciones: ''
            },
            {
                nombre: 'ajustar_brillo_contraste',
                especificaciones: 'brillo=1.2,contraste=1.1'
            },
            {
                nombre: 'marca_agua',
                especificaciones: 'texto=Procesado,posicion=esquina_inf_der,tamaño_fuente=20'
            },
            {
                nombre: 'convertir_formato',
                especificaciones: 'formato=PNG'
            }
        ];

        // Seleccionar 1-3 transformaciones aleatorias
        const numAdditional = Math.floor(Math.random() * 3) + 1;
        const selected = [];
        const available = [...additionalTransformations];

        for (let i = 0; i < numAdditional && available.length > 0; i++) {
            const randomIndex = Math.floor(Math.random() * available.length);
            selected.push(available[randomIndex]);
            available.splice(randomIndex, 1);
        }

        return [...transformations, ...selected];
    }

    // Preparar imágenes para el procesamiento
    prepareImagesForProcessing(imageFilenames) {
        const images = [];

        for (const filename of imageFilenames) {
            const imageData = this.imageToBase64(filename);
            if (imageData) {
                images.push({
                    nombre: filename,
                    tipo: imageData.type,
                    contenido_base64: imageData.base64,
                    transformaciones: this.generateRandomTransformations()
                });

                console.log(`   ✅ ${filename} preparada (${(imageData.size / 1024).toFixed(2)} KB)`);
            }
        }

        return images;
    }

    // Mostrar resumen detallado del procesamiento
    displayProcessingSummary(results) {
        if (!results || !results.success) {
            console.log('❌ No se pudo obtener el resumen del procesamiento');
            return;
        }

        console.log('\n📊 RESUMEN DETALLADO DEL PROCESAMIENTO');
        console.log('=' .repeat(50));
        console.log(`✅ Procesamiento exitoso`);
        console.log(`📦 Total de imágenes: ${results.total_processed}`);
        console.log(`👤 Usuario: ${results.user_id}`);
        console.log(`⏰ Timestamp: ${results.timestamp}`);
        console.log(`💬 Mensaje: ${results.message}`);

        console.log('\n🖼️ DETALLES POR IMAGEN:');
        console.log('-'.repeat(50));

        results.results.forEach((result, index) => {
            console.log(`\n${index + 1}. ${result.original_name}`);
            console.log(`   📍 Estado: ${result.status}`);
            console.log(`   🎯 Formato salida: ${result.output_format}`);
            console.log(`   📏 Dimensiones: ${result.final_dimensions}`);
            console.log(`   💾 Archivo guardado: ${result.saved_filename}`);
            console.log(`   📊 Tamaño base64: ${result.processed_image_base64?.length || 0} caracteres`);
            
            console.log(`   🔧 Transformaciones aplicadas (${result.transformations_applied.length}):`);
            result.transformations_applied.forEach(trans => {
                const statusIcon = trans.status === 'completada' ? '✅' : '❌';
                console.log(`      ${statusIcon} ${trans.transformation} (orden: ${trans.order})`);
                if (trans.error) {
                    console.log(`        ⚠️  Error: ${trans.error}`);
                }
            });
        });

        // Estadísticas generales
        const successfulImages = results.results.filter(r => r.status === 'completado').length;
        const totalTransformations = results.results.reduce((sum, r) => sum + r.transformations_applied.length, 0);
        const successfulTransformations = results.results.reduce((sum, r) => 
            sum + r.transformations_applied.filter(t => t.status === 'completada').length, 0
        );

        console.log('\n📈 ESTADÍSTICAS FINALES:');
        console.log('-'.repeat(30));
        console.log(`🖼️  Imágenes exitosas: ${successfulImages}/${results.total_processed}`);
        console.log(`🔧 Transformaciones exitosas: ${successfulTransformations}/${totalTransformations}`);
        console.log(`📈 Tasa de éxito: ${((successfulImages / results.total_processed) * 100).toFixed(1)}%`);
    }
}

// Función principal de prueba
async function testCompleteFlow() {
    const client = new APIClient();
    const imageProcessor = new ImageProcessor();
    
    console.log('🧪 INICIANDO PRUEBA COMPLETA DEL SISTEMA');
    console.log('=' .repeat(50));

    // 1. Health check
    console.log('\n1. 🔍 VERIFICANDO ESTADO DEL SERVIDOR...');
    const healthOk = await client.healthCheck();
    if (!healthOk) {
        console.log('❌ El servidor no está disponible. Asegúrate de que esté ejecutándose.');
        return;
    }
    
    // 2. Buscar imágenes
    console.log('\n2. 📁 BUSCANDO IMÁGENES EN CARPETA...');
    const imageFilenames = imageProcessor.getAllImages();
    
    if (imageFilenames.length === 0) {
        console.log('ℹ️  No se encontraron imágenes. El proceso se detiene.');
        console.log('💡 Coloca imágenes en la carpeta "images" y ejecuta de nuevo.');
        return;
    }

    // 3. Registrar usuario (si es necesario)
    console.log('\n3. 👤 CONFIGURANDO AUTENTICACIÓN...');
    const testUsername = `testuser_${Date.now()}`;
    const testPassword = 'testpass123';
    
    await client.register(testUsername, testPassword);
    
    // 4. Login
    console.log('\n4. 🔐 INICIANDO SESIÓN...');
    const loginResult = await client.login(testUsername, testPassword);
    if (!loginResult || !loginResult.success) {
        console.log('❌ No se pudo iniciar sesión. El proceso se detiene.');
        return;
    }

    // 5. Información del usuario
    console.log('\n5. 📋 OBTENIENDO INFORMACIÓN DEL USUARIO...');
    await client.getUserInfo();

    // 6. Preparar y procesar imágenes
    console.log('\n6. 🖼️ PREPARANDO IMÁGENES PARA PROCESAMIENTO...');
    const imagesToProcess = imageProcessor.prepareImagesForProcessing(imageFilenames);
    
    console.log(`\n7. 🚀 ENVIANDO ${imagesToProcess.length} IMÁGENES PARA PROCESAMIENTO...`);
    const processingResults = await client.processImages(imagesToProcess);
    
    // 7. Mostrar resultados
    if (processingResults) {
        imageProcessor.displayProcessingSummary(processingResults);
    }

    // 8. Logout
    console.log('\n8. 🚪 CERRANDO SESIÓN...');
    await client.logout();

    console.log('\n🎉 PRUEBA COMPLETADA EXITOSAMENTE!');
}

// Función para probar con una sola imagen específica
async function testSingleImage(imageFilename) {
    const client = new APIClient();
    const imageProcessor = new ImageProcessor();

    console.log(`🧪 PRUEBA INDIVIDUAL: ${imageFilename}`);
    console.log('=' .repeat(40));

    // Login con usuario existente o nuevo
    const testUsername = `testuser_${Date.now()}`;
    await client.register(testUsername, 'testpass123');
    await client.login(testUsername, 'testpass123');

    // Procesar imagen individual
    const imageData = imageProcessor.imageToBase64(imageFilename);
    if (imageData) {
        const imagesToProcess = [{
            nombre: imageFilename,
            tipo: imageData.type,
            contenido_base64: imageData.base64,
            transformaciones: [
                {
                    nombre: 'escala_grises',
                    especificaciones: ''
                },
                {
                    nombre: 'redimensionar',
                    especificaciones: 'ancho=600,alto=400'
                },
                {
                    nombre: 'marca_agua',
                    especificaciones: 'texto=Procesado,posicion=centro'
                }
            ]
        }];

        console.log(`\n🖼️ Procesando imagen: ${imageFilename}`);
        const results = await client.processImages(imagesToProcess);
        imageProcessor.displayProcessingSummary(results);
    }

    await client.logout();
}

// Manejo de argumentos de línea de comandos
const args = process.argv.slice(2);

if (args.length > 0) {
    // Procesar imagen específica
    const imageName = args[0];
    testSingleImage(imageName).catch(console.error);
} else {
    // Procesar todas las imágenes
    testCompleteFlow().catch(console.error);
}