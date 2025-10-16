import socket
from typing import Dict

def parsear_especificaciones(especificaciones: str) -> Dict:
    """Convierte especificaciones de string a diccionario"""
    parametros = {}
    if especificaciones and especificaciones.strip():
        partes = especificaciones.split(',')
        for parte in partes:
            if '=' in parte:
                key, value = parte.split('=', 1)
                key = key.strip()
                value = value.strip()
                
                try:
                    if '.' in value:
                        value = float(value)
                    else:
                        value = int(value)
                except ValueError:
                    pass
                
                parametros[key] = value
    return parametros

def verificar_puerto_disponible(host='localhost', port=9090):
    """Verifica si un puerto está disponible"""
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind((host, port))
        return True
    except OSError:
        return False