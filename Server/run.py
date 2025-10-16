from app import create_app

app = create_app()

app.config['MAX_CONTENT_LENGTH'] = 50 * 1024 * 1024

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)