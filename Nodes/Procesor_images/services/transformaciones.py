from PIL import Image, ImageFilter, ImageEnhance, ImageDraw, ImageFont
from typing import Dict, List
import io
import Pyro4

@Pyro4.expose
class TransformacionesImagen:
    """Servicio de transformaciones de imágenes usando PIL"""
    
    @staticmethod
    @Pyro4.expose
    def aplicar_escala_grises(imagen: Image.Image, parametros: Dict = {}) -> Image.Image:
        """Convierte la imagen a escala de grises"""
        return imagen.convert('L')
    
    @staticmethod
    @Pyro4.expose
    def aplicar_redimensionar(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Redimensiona la imagen"""
        ancho = parametros.get('ancho')
        alto = parametros.get('alto')
        if ancho and alto:
            return imagen.resize((ancho, alto), Image.Resampling.LANCZOS)
        else:
            raise ValueError("Faltan parámetros ancho/alto para redimensionar")
    
    @staticmethod
    @Pyro4.expose
    def aplicar_recortar(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Recorta la imagen"""
        izquierda = parametros.get('izquierda', 0)
        superior = parametros.get('superior', 0)
        derecha = parametros.get('derecha', imagen.width)
        inferior = parametros.get('inferior', imagen.height)
        return imagen.crop((izquierda, superior, derecha, inferior))
    
    @staticmethod
    @Pyro4.expose
    def aplicar_rotar(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Rota la imagen"""
        angulo = parametros.get('angulo', 0)
        expandir = parametros.get('expandir', True)
        return imagen.rotate(angulo, expand=expandir)
    
    @staticmethod
    @Pyro4.expose
    def aplicar_reflejar(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Refleja la imagen horizontal o verticalmente"""
        tipo_reflejo = parametros.get('tipo', 'horizontal')
        if tipo_reflejo == 'horizontal':
            return imagen.transpose(Image.FLIP_LEFT_RIGHT)
        elif tipo_reflejo == 'vertical':
            return imagen.transpose(Image.FLIP_TOP_BOTTOM)
        else:
            raise ValueError("Tipo de reflejo no válido. Use 'horizontal' o 'vertical'")
    
    @staticmethod
    @Pyro4.expose
    def aplicar_desenfocar(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Aplica desenfoque gaussiano"""
        radio = parametros.get('radio', 2)
        return imagen.filter(ImageFilter.GaussianBlur(radio))
    
    @staticmethod
    @Pyro4.expose
    def aplicar_perfilar(imagen: Image.Image, parametros: Dict = {}) -> Image.Image:
        """Aplica filtro de nitidez"""
        return imagen.filter(ImageFilter.SHARPEN)
    
    @staticmethod
    @Pyro4.expose
    def aplicar_ajustar_brillo_contraste(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Ajusta brillo y contraste"""
        brillo = parametros.get('brillo', 1.0)
        contraste = parametros.get('contraste', 1.0)
        
        if brillo != 1.0:
            enhancer = ImageEnhance.Brightness(imagen)
            imagen = enhancer.enhance(brillo)
        
        if contraste != 1.0:
            enhancer = ImageEnhance.Contrast(imagen)
            imagen = enhancer.enhance(contraste)
        
        return imagen
    
    @staticmethod
    @Pyro4.expose
    def aplicar_marca_agua(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Añade marca de agua o texto"""
        texto = parametros.get('texto', 'Marca de Agua')
        posicion = parametros.get('posicion', 'centro')
        tamaño_fuente = parametros.get('tamaño_fuente', 20)
        color = parametros.get('color', (255, 255, 255, 128))
        
        # Crear imagen transparente para el texto
        if imagen.mode != 'RGBA':
            imagen = imagen.convert('RGBA')
            
        txt_layer = Image.new('RGBA', imagen.size, (255, 255, 255, 0))
        draw = ImageDraw.Draw(txt_layer)
        
        # Intentar cargar fuente
        try:
            font = ImageFont.truetype("arial.ttf", tamaño_fuente)
        except:
            try:
                font = ImageFont.truetype("/usr/share/fonts/truetype/freefont/FreeMono.ttf", tamaño_fuente)
            except:
                font = ImageFont.load_default()
        
        # Calcular posición del texto
        bbox = draw.textbbox((0, 0), texto, font=font)
        text_width = bbox[2] - bbox[0]
        text_height = bbox[3] - bbox[1]
        
        if posicion == 'centro':
            x = (imagen.width - text_width) // 2
            y = (imagen.height - text_height) // 2
        elif posicion == 'esquina_inf_der':
            x = imagen.width - text_width - 10
            y = imagen.height - text_height - 10
        elif posicion == 'esquina_sup_izq':
            x = 10
            y = 10
        elif posicion == 'esquina_sup_der':
            x = imagen.width - text_width - 10
            y = 10
        elif posicion == 'esquina_inf_izq':
            x = 10
            y = imagen.height - text_height - 10
        else:
            x = 10
            y = 10
        
        # Dibujar texto
        draw.text((x, y), texto, font=font, fill=color)
        
        # Combinar con imagen original
        return Image.alpha_composite(imagen, txt_layer)
    
    @staticmethod
    @Pyro4.expose
    def aplicar_convertir_formato(imagen: Image.Image, parametros: Dict) -> Image.Image:
        """Convierte el formato de la imagen"""
        formato = parametros.get('formato', 'PNG').upper()
        
        # Para JPEG, convertir a RGB si es necesario
        if formato == 'JPEG' and imagen.mode in ('RGBA', 'LA', 'P'):
            imagen = imagen.convert('RGB')
        elif formato == 'PNG' and imagen.mode != 'RGBA':
            imagen = imagen.convert('RGBA')
        
        return imagen
    
    @staticmethod
    @Pyro4.expose
    def obtener_transformaciones_disponibles() -> Dict[str, str]:
        """Retorna la lista de transformaciones disponibles"""
        return {
            'escala_grises': 'Conversión a escala de grises',
            'redimensionar': 'Redimensionar imagen',
            'recortar': 'Recortar imagen',
            'rotar': 'Rotar imagen',
            'reflejar': 'Reflejar imagen',
            'desenfocar': 'Desenfocar imagen',
            'perfilar': 'Aumentar nitidez',
            'ajustar_brillo_contraste': 'Ajustar brillo y contraste',
            'marca_agua': 'Añadir marca de agua o texto',
            'convertir_formato': 'Convertir formato de imagen'
        }
    
    @staticmethod
    @Pyro4.expose
    def procesar_imagen_pil(imagen_bytes: bytes, transformaciones: List[Dict]) -> bytes:
        """
        Procesa una imagen usando PIL con múltiples transformaciones
        """
        try:
            # Convertir bytes a imagen PIL
            imagen = Image.open(io.BytesIO(imagen_bytes))
            
            # Aplicar transformaciones
            for transformacion in transformaciones:
                tipo = transformacion.get('tipo')
                parametros = transformacion.get('parametros', {})
                
                if tipo == 'escala_grises':
                    imagen = TransformacionesImagen.aplicar_escala_grises(imagen, parametros)
                elif tipo == 'redimensionar':
                    imagen = TransformacionesImagen.aplicar_redimensionar(imagen, parametros)
                elif tipo == 'recortar':
                    imagen = TransformacionesImagen.aplicar_recortar(imagen, parametros)
                elif tipo == 'rotar':
                    imagen = TransformacionesImagen.aplicar_rotar(imagen, parametros)
                elif tipo == 'reflejar':
                    imagen = TransformacionesImagen.aplicar_reflejar(imagen, parametros)
                elif tipo == 'desenfocar':
                    imagen = TransformacionesImagen.aplicar_desenfocar(imagen, parametros)
                elif tipo == 'perfilar':
                    imagen = TransformacionesImagen.aplicar_perfilar(imagen, parametros)
                elif tipo == 'ajustar_brillo_contraste':
                    imagen = TransformacionesImagen.aplicar_ajustar_brillo_contraste(imagen, parametros)
                elif tipo == 'marca_agua':
                    imagen = TransformacionesImagen.aplicar_marca_agua(imagen, parametros)
                elif tipo == 'convertir_formato':
                    imagen = TransformacionesImagen.aplicar_convertir_formato(imagen, parametros)
            
            # Convertir de vuelta a bytes
            output_buffer = io.BytesIO()
            formato = parametros.get('formato', 'JPEG')
            imagen.save(output_buffer, format=formato, quality=95)
            
            return output_buffer.getvalue()
            
        except Exception as e:
            raise Exception(f"Error en procesamiento PIL: {str(e)}")