from app import create_app, db

MODEL_CANDIDATES = [
    "app.models.entitys",
    "app.models.entity"
]

User = None

for modname in MODEL_CANDIDATES:
    try:
        mod = __import__(modname, fromlist=["User"])
        User = getattr(mod, "User", None)
        if User:
            print(f"✅ Importé User desde: {modname}")
            break
    except Exception:
        pass

if not (User):
    raise ImportError(
        "No pude importar User. Revisa app/models/*.py"
    )

app = create_app()
with app.app_context():
    db.create_all()
    print("✅ Base de datos (data.db) y tablas creadas correctamente")