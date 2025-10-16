const { sessions } = require('../services/sessionManager');
const { MESSAGES } = require('../config/constants');

function authenticateToken(req, res, next) {
    const token = req.headers['authorization'];
    
    if (!token) {
        return res.status(401).json({
            success: false,
            error: MESSAGES.AUTH.TOKEN_REQUIRED
        });
    }

    const session = sessions.get(token);
    if (!session) {
        return res.status(401).json({
            success: false,
            error: MESSAGES.AUTH.INVALID_TOKEN
        });
    }

    req.user = session;
    next();
}

module.exports = {
    authenticateToken
};