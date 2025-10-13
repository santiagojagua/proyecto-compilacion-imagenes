const axios = require("axios");
const fs = require("fs");
const path = require("path");

const API_BASE = "http://localhost:3000/api";

// Función para convertir imagen a base64
function imageToBase64(imagePath) {
  try {
    const imageBuffer = fs.readFileSync(imagePath);
    return imageBuffer.toString('base64');
  } catch (error) {
    console.error(`Error leyendo imagen ${imagePath}:`, error.message);
    return null;
  }
}

// Test 1: Salud del servidor
async function testHealth() {
  try {
    const response = await axios.get(`${API_BASE}/health`);
    console.log("✅ Health check:", response.data);
  } catch (error) {
    console.error("❌ Health check failed:", error.message);
  }
}

// Test 2: Estado SOAP
async function testSoapStatus() {
  try {
    const response = await axios.get(`${API_BASE}/estado-soap`);
    console.log("✅ Estado SOAP:", response.data);
  } catch (error) {
    console.error("❌ Estado SOAP failed:", error.message);
  }
}

// Test 3: Procesar imágenes con JSON
async function testProcesarImagenes() {
  try {
    // Buscar una imagen de prueba
    const testImagePath = findTestImage();
    if (!testImagePath) {
      console.log("⚠️ No se encontró imagen de prueba, usando datos mock");
      // Usar datos mock si no hay imagen real
      await testWithMockData();
      return;
    }

    const imageBase64 = imageToBase64(testImagePath);
    if (!imageBase64) {
      throw new Error("No se pudo convertir la imagen a base64");
    }

    const requestData = {
      user_id: 123,
      imagenes: [
        {
          nombre: "test-image.jpg",
          tipo: "jpg",
          contenido_base64: imageBase64,
          transformaciones: [
            {
              nombre: "escala_grises",
              especificaciones: ""
            },
            {
              nombre: "redimensionar",
              especificaciones: "ancho=800,alto=600"
            },
            {
              nombre: "marca_agua",
              especificaciones: "texto=Procesado,posicion=esquina_inf_der"
            }
          ]
        }
      ]
    };

    console.log("📤 Enviando imágenes para procesamiento...");
    const response = await axios.post(`${API_BASE}/procesar-imagenes`, requestData);
    
    console.log("✅ Procesar imágenes response:", JSON.stringify(response.data, null, 2));

  } catch (error) {
    console.error("❌ Procesar imágenes failed:", error.response?.data || error.message);
  }
}

// Test con datos mock (sin imagen real)
async function testWithMockData() {
  try {
    const requestData = {
      user_id: 123,
      imagenes: [
        {
          nombre: "mock-image.jpg",
          tipo: "jpg",
          contenido_base64: "VGVzdEJhc2U2NERhdGE=", // "TestBase64Data" en base64
          transformaciones: [
            {
              nombre: "escala_grises",
              especificaciones: ""
            }
          ]
        }
      ]
    };

    console.log("📤 Enviando datos mock para procesamiento...");
    const response = await axios.post(`${API_BASE}/procesar-imagenes`, requestData);
    
    console.log("✅ Procesar imágenes (mock) response:", JSON.stringify(response.data, null, 2));

  } catch (error) {
    console.error("❌ Procesar imágenes (mock) failed:", error.response?.data || error.message);
  }
}

// Buscar imagen de prueba
function findTestImage() {
  const extensions = ['.jpg', '.jpeg', '.png', '.bmp', '.gif'];
  const files = fs.readdirSync('.');

  for (const file of files) {
    const ext = path.extname(file).toLowerCase();
    if (extensions.includes(ext)) {
      return file;
    }
  }
  return null;
}

// Ejecutar todos los tests
async function runTests() {
  console.log("🧪 Ejecutando tests del API REST...\n");

  await testHealth();
  console.log("---");
  
  await testSoapStatus();
  console.log("---");
  
  await testProcesarImagenes();
}

// Ejecutar tests si se llama directamente
if (require.main === module) {
  runTests();
}

module.exports = { testHealth, testSoapStatus, testProcesarImagenes };