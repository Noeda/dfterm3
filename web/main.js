var connection = require('./connection.js');

var ob = new connection.Dfterm3Connection;
ob.on("login_failed", function() {
    document.write("Login failed.");
});

ob.on("change:logged_in", function(new_value) {
    document.write("Have I logged in?");
    document.write(new_value.get("logged_in"));
});

ob.on("change:connected", function(new_value) {
    document.write("Have I connected?");
    document.write(new_value.get("connected"));
});

ob.on("error_in_websocket_connection", function() {
    document.write("Some error happened while trying to make a WebSocket " +
                   "connection.");
});

window.stupid = ob; // prevent garbage collection

