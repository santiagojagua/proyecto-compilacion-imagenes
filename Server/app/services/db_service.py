from app import db
from app.models.entitys import User

def save_request(data):
    req = User(content=data)
    db.session.add(req)
    db.session.commit()
