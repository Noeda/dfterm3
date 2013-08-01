dfterm3_playing = function() {

    var exports = { };

    if ( WebSocket === undefined ) {
        exports.websockets_are_supported = false;
        return;
    }
    exports.websockets_are_supported = true;

    var socket = new WebSocket( "ws://127.0.0.1:8000/" );
    socket.onopen = function() {
        socket.send("Hello Dfterm3.");
    }

    return exports;
}();

