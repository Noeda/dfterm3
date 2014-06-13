var d3module = require('./dfterm3_protocol');
var login_box = require('./login_box');

var d3 = d3module.makeWebSocketDfterm3Connection( 'ws://127.0.0.1:8000' );
var lbox = login_box.makeLoginBox();

document.documentElement.appendChild(lbox.dom);

d3.onConnectionEstablished = function(d3) {
    lbox.setD3( d3 );
}

d3.onConnectionLost = function() {
    lbox.setD3( null );
}

