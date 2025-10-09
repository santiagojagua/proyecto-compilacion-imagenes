# test-api.ps1
$baseUrl = "http://localhost:8080"

Write-Host "`n=== Probando API Lisp ===" -ForegroundColor Green

# Test 1: Root endpoint
Write-Host "`n1. GET /" -ForegroundColor Cyan
try {
    $response = Invoke-WebRequest -Uri "$baseUrl/" -UseBasicParsing
    Write-Host "✅ Status: $($response.StatusCode)" -ForegroundColor Green
    Write-Host "   Respuesta: $($response.Content)"
} catch {
    Write-Host "❌ Error: $_" -ForegroundColor Red
}

# Test 2: Health check
Write-Host "`n2. GET /health" -ForegroundColor Cyan
try {
    $response = Invoke-WebRequest -Uri "$baseUrl/health" -UseBasicParsing
    Write-Host "✅ Status: $($response.StatusCode)" -ForegroundColor Green
    Write-Host "   Respuesta: $($response.Content | ConvertFrom-Json | ConvertTo-Json -Depth 10)"
} catch {
    Write-Host "❌ Error: $_" -ForegroundColor Red
}

# Test 3: API Saludo
Write-Host "`n3. GET /api/saludo" -ForegroundColor Cyan
try {
    $response = Invoke-WebRequest -Uri "$baseUrl/api/saludo" -UseBasicParsing
    Write-Host "✅ Status: $($response.StatusCode)" -ForegroundColor Green
    Write-Host "   Respuesta: $($response.Content | ConvertFrom-Json | ConvertTo-Json -Depth 10)"
} catch {
    Write-Host "❌ Error: $_" -ForegroundColor Red
}

# Test 4: JSON-RPC - Obtener estadísticas
Write-Host "`n4. POST /api/rpc - obtenerEstadisticas" -ForegroundColor Cyan
try {
    $body = @{
        jsonrpc = "2.0"
        method = "obtenerEstadisticas"
        params = @{}
        id = 1
    } | ConvertTo-Json

    $response = Invoke-WebRequest -Uri "$baseUrl/api/rpc" `
        -Method POST `
        -ContentType "application/json" `
        -Body $body `
        -UseBasicParsing
    
    Write-Host "✅ Status: $($response.StatusCode)" -ForegroundColor Green
    Write-Host "   Respuesta: $($response.Content | ConvertFrom-Json | ConvertTo-Json -Depth 10)"
} catch {
    Write-Host "❌ Error: $_" -ForegroundColor Red
}

Write-Host "`n=== Pruebas completadas ===" -ForegroundColor Green