const sessions = new Map();

function generateToken() {
    return 'token_' + Math.random().toString(36).substr(2, 9) + '_' + Date.now();
}

function createSession(userData) {
    const token = generateToken();
    const session = {
        user_id: userData.user_id,
        username: userData.username,
        login_time: new Date().toISOString()
    };
    
    sessions.set(token, session);
    return token;
}

function getSession(token) {
    return sessions.get(token);
}

function deleteSession(token) {
    return sessions.delete(token);
}

function cleanupExpiredSessions(maxAge = 24 * 60 * 60 * 1000) { // 24 horas por defecto
    const now = Date.now();
    for (const [token, session] of sessions.entries()) {
        const sessionTime = new Date(session.login_time).getTime();
        if (now - sessionTime > maxAge) {
            sessions.delete(token);
        }
    }
}

// Limpiar sesiones expiradas cada hora
setInterval(cleanupExpiredSessions, 60 * 60 * 1000);

module.exports = {
    sessions,
    generateToken,
    createSession,
    getSession,
    deleteSession,
    cleanupExpiredSessions
};