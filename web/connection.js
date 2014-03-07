/* This module handles communication with the Dfterm3 server.
 *
 * Don't introduce any browserisms (that is, DOM) in this module except where
 * unavoidable (like the WebSocket API if you count that as browserism).
 *
 * This way we'll avoid tying the communication parts with presentation parts.
 *
 *
 * Usage:
 *
 * var connection = require('connection.js');
 * var dfterm3 = new connection.Dfterm3Connection;
 *
 * TODO: if someone asks, write some more docs.
 */

var Backbone = require('backbone');

var WANTED_MAJOR_VERSION = 0;

var Dfterm3Connection = Backbone.Model.extend({
    initialize: function() {
        var t = this;

        function set_disconnect_attributes() {
            t.set({ "connected": false
                 ,  "logged_in": false })
        }
        function disconnect() {
            if ( "websocket" in t ) {
                t.websocket.close();
                delete t.websocket;
            }
        }

        t.snd = function(something) {
            return t.websocket.send( JSON.stringify(something) );
        }

        t.websocket = new WebSocket( t.get("websocket_target") );
        t.set( {"connecting": true });
        t.websocket.onopen = function() {
            t.set({ "connected": true })
        }
        t.websocket.onclose = function() {
            delete t.websocket;
            set_disconnect_attributes();
            t.set( {"connecting": false });
        }
        t.websocket.onerror = function() {
            disconnect();
            set_disconnect_attributes();
            t.trigger( "error_in_websocket_connection" );
            t.set( {"connecting": false });
        }
        t.websocket.onmessage = function(e) {
            var msg = JSON.parse( e.data );

            if ( msg.message === "handshake" ) {
                if ( msg.major !== WANTED_MAJOR_VERSION ) {
                    disconnect();
                    set_disconnect_attributes();
                    t.trigger( "incompatible_dfterm3_server" );
                } else {
                    if ( t.get("do_implicit_login") === true ) {
                        t.do_login();
                    }
                }
            }
            else if ( msg.message === "login_acknowledgement" ) {
                if ( msg.status === true ) {
                    t.set({ "logged_in": true })
                } else {
                    t.set({ "do_implicit_login": false });
                    t.trigger( "login_failed" );
                }
            }
        }
    }

   ,do_login: function() {
        var t = this;

        if ( t.get("connected") !== true ) {
            throw "do_login() can only be performed if we have connected.";
        }
        if ( t.get("logged_in") === true ) {
            throw ("do_login() can only be performed if we " +
                   "haven't logged in yet.");
        }

        if ( t.get("guest") === true ) {
            t.snd({ "message":  "login"
                  , "identity": { "guest": true } })
        } else {
            if ( typeof t.get("username") !== "string" ) {
                throw "do_login(): username has not been set properly.";
            }

            var ob = { "message":  "login"
                     , "identity": { "user":  t.get("username") } }

            if ( typeof t.get("password") === "string" ) {
                ob.password = t.get("password");
            }

            t.snd(ob);
        }
    }

   ,defaults: {
        "websocket_target":    "ws://127.0.0.1:8000"
       ,"connecting":          false
       ,"connected":           false
       ,"logged_in":           false
       ,"username":            false
       ,"guest":               true
       ,"do_implicit_login":   false
       ,"password":            false
    }
});

module.exports = { "Dfterm3Connection": Dfterm3Connection }

