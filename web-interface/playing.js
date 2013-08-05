dfterm3_playing = function() {

    var exports = { };

    // protocol message types.
    var SCREEN_DATA = 1;
    var JSON_DATA = 2;

    if ( WebSocket === undefined ) {
        exports.websockets_are_supported = false;
        return exports;
    }
    exports.websockets_are_supported = true;

    exports.play = function( host, host_element ) {
        var terminal = dfterm3_terminal.createTerminal(80, 24);
        var status_element = document.createElement("span");

        status_element.textContent =
            "Trying to make a connection to Dfterm3 server..."

        var status = function(str) {
            status_element.textContent = str;
        }

        host_element.appendChild( terminal.getDOMObject() );
        host_element.appendChild( status_element );

        var handleScreenData = function( array ) {
            var array_view = new Uint8Array( array, 1 );
            var w = (array_view[0] << 8) | array_view[1];
            var h = (array_view[2] << 8) | array_view[3];
            terminal.resize( w, h );
            var num_elements = (array_view[4] << 24) |
                               (array_view[5] << 16) |
                               (array_view[6] << 8) |
                               (array_view[7]);

            for ( var i = 0; i < num_elements; ++i ) {
                var offset = 8 + i*6;
                var cp437_code = array_view[offset];
                var colors = array_view[offset+1];
                var foreground_color = colors & 0x0f;
                var background_color = (colors & 0xf0) >> 4;
                var x = (array_view[offset+2] << 8) | array_view[offset+3];
                var y = (array_view[offset+4] << 8) | array_view[offset+5];
                terminal.setCP437CellAt( x, y, cp437_code
                                       , foreground_color
                                       , background_color );
            }
        }

        var socket = new WebSocket( host );
        socket.onopen = function() {
            status("Connection established.");
        }
        socket.onclose = function() {
            status("No connection.");
        }
        socket.onmessage = function(event) {
            var reader = new FileReader();
            reader.readAsArrayBuffer(event.data);
            reader.onload = function() {
                var array = reader.result;
                var array_view = new Uint8Array( array, 0, 1 );
                if ( array_view[0] == SCREEN_DATA ) {
                    handleScreenData( array );
                } else if ( array_view[0] == JSON_DATA ) {
                    // nothing yet.
                }
            }
        }
    }

    return exports;
}();

