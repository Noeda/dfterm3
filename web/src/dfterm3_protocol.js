/*
 * This file implements the Dfterm3 Client to Server protocol.
 * It's agnostic to the medium which is used to send the data.
 *
 *
 * Quick documentation:
 *
 * These docs assume dfterm3_protocol has been exported to name 'd3':
 * var d3module = require('dfterm3_protocol');
 *
 *
 * You need to make the connection to Dfterm3 server yourself. However, you
 * need not to handle any of the low-level details; those are provided by this
 * module.
 *
 * Create an object to handle the protocol:
 *
 * var d3 = d3module.makeDfterm3Connection( 'Nostrils', sender_function );
 *
 * You probably want to use the WebSocket aware function though:
 *
 * var d3 = d3module.makeWebSocketDfterm3Connection( 'Nostrils'
 *                                                 , 'ws://somewhere:8000' );
 *
 * If you use the former function, then 'sender_function' should be a function
 * that takes one argument. It should serialize the argument to JSON and send
 * it over WebSockets to Dfterm3.
 *
 * Whenever you receive data from WebSocket, you should deserialize it from
 * JSON and then call:
 *
 * var json = <get this from some of your network code>
 * d3.handle( json );
 *
 * By default, when you create the d3 object, the sender_function is
 * immediately called with the given name, in an attempt to authenticate to
 * Dfterm3. If this fails, the appropriate event handler function is called
 * (see below).
 *
 * If you use the WebSocket creator function then you don't have to handle any
 * of that above; it's all done for you.
 *
 * These are all the various events you can set on d3.
 *
 * d3.onConnectionLost = function(d3)     When connecting fails or is
 *                                        disconnected. Only used if you used
 *                                        'makeWebSocketDfterm3Connection'.
 * d3.onLogin = function(d3)                 Called when login is successful.
 * d3.onLoginFailure = function(d3)          Called when login fails.
 * d3.onSubscribe = function(d3)             Called when subscribing to a game
 *                                           succeeds.
 * d3.onSubscribingFailure = function(d3, reason)
 *                                           Called when subscribing to a game
 *                                           fails. 'reason' may be a
 *                                           human-readable strings telling
 *                                           why.
 * d3.onUnsubscribe = function(d3)           Called when being unsubscribed
 *                                           from a game, for whatever reason.
 * d3.onChatJoin = function(d3, who)         Called when someone joins the
 *                                           chatroom of a game.
 * d3.onChatPart = function(d3, who)         Called when someone leaves the
 *                                           chatroom of a game.
 * d3.onChatMessage = function(d3, who, msg) Someone spoke in a chatroom.
 * d3.onPlayerChange = function(d3, who)     Someone just played the game.
 * d3.onGamesList = function(d3, list)       The server is reporting a list of
 *                                           playable games. The list is of the
 *                                           form:
 *                                           [ { key: KEY, name: NAME } ... ]
 *                                           Where KEY is used to uniquely
 *                                           identify the game and NAME is the
 *                                           name the server administrator
 *                                           chose for this game.
 * d3.onResize = function(d3, w, h)          The game screen has changed size.
 *                                           The given values are the new width
 *                                           and height, measured in number of
 *                                           cells.
 * d3.onCellChange = function( d3
 *                           , x
 *                           , y
 *                           , cp437_code_point
 *                           , foreground_color
 *                           , background_color );
 *
 *     Called when a cell at some position has changed. 'x' and 'y' give the
 *     position, where the origin is top-left at (0, 0).
 *     'cp437_code_point' is a number but you can turn it into a character with
 *     d3module.cp437ToChar(cp437_code_point);
 *     'foreground_color' and 'background_color' are integers denoting the
 *     foreground and background color of the cell. See a table below for what
 *     these values mean.
 *
 * These are all the various actions you can do with d3:
 *
 * d3.requestGamesList(callback)              Request a list of playable games.
 *                                            The callback is called the same
 *                                            way as 'onGamesList' (see above).
 * d3.subscribe(key, callback)                Subscribe to a game. The key
 *                                            comes from a games list (see
 *                                            above).
 * d3.unSubscribe()                           Unsubscribe from whatever game
 *                                            you may have subscribed to.
 * d3.chat(msg)                               Chat in the current chatroom.
 * d3.doInput( key_direction                  Use this to play the game itself.
 *           , which                          'key_direction' is either 'up',
 *           , code_point                     'down' or 'up_and_down'. Use that 
 *           , shift_down                     to communicate if key was pressed
 *           , alt_down                       down, released or pressed and
 *           , ctrl_down );                   released right away.
 *                                            'which' is an unfortunately
 *                                            browser specific value. It comes
 *                                            from the DOM 'keydown' and
 *                                            'keyup' event. It can be zero if
 *                                            the key has no 'which' value.
 *                                            'code_point' is the unicode code
 *                                            point of the key that was
 *                                            pressed. You can use 0 if the key
 *                                            has no unicode code point.
 *
 * You should use DOM events 'keypress' to send input events in which 'which'
 * is not set and 'code_point' set, and 'keydown' and 'keyup' DOM events to
 * send input events in 'which' is set and 'code_point' is not set. This is an
 * empirically found scheme which seemd to work well enough. Use 'up_and_down'
 * for 'keypress' events.
 *
 * d3.handle( decoded_json );                 Handle some JSON data that was
 *                                            received from the Dfterm3 server.
 *                                            If you used
 *                                            makeWebSocketDfterm3Connection
 *                                            then you don't need to use this.
 *
 * d3.authenticate( name, callback );         In case authentication failed
 *                                            during creation, you can use this
 *                                            to try another name. Callback is
 *                                            called with the d3 object and a
 *                                            boolean that is true if
 *                                            authentication succeeded and
 *                                            false if it didn't.
 *
 *
 * Color table:
 *
 *     0      black
 *     1      red
 *     2      green
 *     3      yellow
 *     4      blue
 *     5      magenta
 *     6      cyan
 *     7      gray
 *     8      dark gray
 *     9      bright red
 *     10     bright green
 *     11     bright yellow
 *     12     bright blue
 *     13     bright magenta
 *     14     bright cyan
 *     15     white
 */

var _ = require('underscore');

exports.makeWebSocketDfterm3Connection = function( name, url )
{
    var ws = new WebSocket( url );
    var sender = function( encode_me ) {
        ws.send( JSON.stringify(encode_me) );
    };
    var d3 = exports.makeDfterm3Connection( name, sender );
    ws.onclose = function() {
        if ( d3.onConnectionLost ) {
            d3.onConnectionLost( d3 );
        }
    }
    ws.onerror = function() {
        if ( d3.onConnectionLost ) {
            d3.onConnectionLost( d3 );
        }
    }
    ws.onmessage = function(event) {
        var decoded = JSON.parse(event.data);
        d3.handle(decoded);
    }
    return d3;
}

exports.makeDfterm3Connection = function( name
                                        , sender )
{
    var my_name;    // the current nickname in Dfterm3
    var connection_status = 'handshaking';
    // ^ current status of the connection
    var ob = { }
    // The object that we return and hang publicly accessible data.

    var chatters = { };
    // Set of nicknames that are currently (known) to be in the chat.

    var call = function(fun) { fun(ob); };
    // helper function that calls whatever function with the above object.

    var terminal_w;
    var terminal_h;
    // last known terminal size.


    // function below handles the game changesets
    var handleGameChangesets = function(base64) {
        var buf = new Buffer(base64, 'base64');
        var w = (buf[0] << 8) | buf[1];
        var h = (buf[2] << 8) | buf[3];

        // inform about terminal size changes, if needed
        if ( w !== terminal_w || h !== terminal_h ) {
            terminal_w = w;
            terminal_h = h;
            if ( ob.onResize ) {
                ob.onResize( ob, terminal_w, terminal_h );
            }
        }

        var num_elements = (buf[4] << 24) |
                           (buf[5] << 16) |
                           (buf[6] << 8) |
                           (buf[7]);

        var ob_onCellChange = ob.onCellChange;  // maybe this makes it faster?
        if ( !ob_onCellChange ) {
            ob_onCellChange = function () { };
        }

        for ( var i = 0; i < num_elements; ++i ) {
            var offset = 8 + i*6;
            var cp437_code = buf[offset];
            var colors = buf[offset+1];
            var foreground_color = colors & 0x0f;
            var background_color = (colors & 0xf0) >> 4;
            var x = (buf[offset+2] << 8) | buf[offset+3];
            var y = (buf[offset+4] << 8) | buf[offset+5];
            // inform about changes at individual cells
            ob_onCellChange( ob, x, y
                           , cp437_code, foreground_color, background_color );
        }
    }

    // these various arrays kep track of which callbacks need to be called,
    // that have not yet been called.
    var pending_rgl_callbacks = [ ];  // requestGamesList
    var pending_subscribe_callbacks = [ ];
    var pending_auth_callbacks = [ ];

    ob.authenticate = function( name, callback ) {
        sender( { tag: 'Authenticate'
                , name: name } );
        if ( callback ) {
            pending_auth_callbacks.push( callback );
        }
        return;
    }

    ob.requestGamesList = function( callback ) {
        sender( { tag: 'ListGames'
                , contents: [] } );
        if ( callback ) {
            pending_rgl_callbacks.push( callback );
        }
        return;
    }

    ob.subscribe = function( key, callback ) {
        if ( typeof key == 'object' ) {
            key = key.key;
        }
        sender( { tag: 'Subscribe'
                , contents: key } );
        if ( callback ) {
            pending_subscribe_callbacks.push( callback );
        }
        return;
    }

    ob.unSubscribe = function() {
        sender( { tag: 'Unsubscribe'
                , contents: [] } );
    }

    ob.chat = function( content ) {
        sender( { tag: 'Chat'
                , contents: content } );
    }

    ob.doInput = function( key_direction
                         , which
                         , code_point
                         , shift_down
                         , alt_down
                         , ctrl_down ) {
        var kd;
        if ( key_direction == 'up' ) {
            kd = 'Up';
        } else if ( key_direction == 'down' ) {
            kd = 'Down';
        } else if ( key_direction == 'up_and_down' ) {
            kd = 'UpAndDown';
        } else {
            throw ("Dfterm3 input: " +
                   "expected 'up', 'down' or 'up_and_down' as key direction.");
        }
        var w = which || 0;
        var c = code_point || 0;

        sender( { tag: 'DoInput'
                , contents: [ kd
                            , [ w
                              , c
                              , shift_down || false
                              , alt_down || false
                              , ctrl_down || false ] ] } );
    }

    ob.handle = function( json ) {
        if ( json.tag === 'LoginSuccessful' ) {
            if ( connection_status === 'handshaking' ) {
                connection_status = 'idle';
                _.each( pending_auth_callbacks
                        , function(fun) { fun(ob, true); });
                pending_auth_callbacks = [ ];
                if ( ob.onLogin ) {
                    ob.onLogin( ob );
                }
            }
        } else if ( json.tag === 'LoginFailed' ) {
            if ( connection_status === 'handshaking' ) {
                _.each( pending_auth_callbacks
                      , function(fun) { fun(ob, false); });
                pending_auth_callbacks = [ ];
                if ( ob.onLoginFailure ) {
                    ob.onLoginFailure( ob );
                }
            }
        } else if ( json.tag === 'CannotSubscribe' ) {
            if ( connection_status === 'idle' ) {
                _.each( pending_subscribe_callbacks
                      , function(fun) { fun(ob, false); } );
                pending_subscribe_callbacks = [ ];
                if ( ob.onSubscribingFailure ) {
                    ob.onSubscribingFailure( ob, json.contents );
                }
            }
        } else if ( json.tag === 'Subscribed' ) {
            if ( connection_status === 'idle' ) {
                connection_status = 'subscribed';
                _.each( pending_subscribe_callbacks
                      , function(fun) { fun(ob, true) } );
                pending_subscribe_callbacks = [ ];
                if ( ob.onSubscribe ) {
                    ob.onSubscribe( ob );
                }
            }
        } else if ( json.tag === 'GameClosed' ) {
            if ( connection_status === 'subscribed' ) {
                connection_status = 'idle';
                terminal_w = null;
                terminal_h = null;
                chatters = { };
                if ( ob.onUnsubscribe ) {
                    ob.onUnsubscribe( ob );
                }
            }
        } else if ( json.tag === 'Joined' ) {
            chatters[json.contents] = true;
            if ( ob.onChatJoin ) {
                ob.onChatJoin( ob, json.contents );
            }
        } else if ( json.tag === 'Parted' ) {
            chatters[json.contents] = false;
            if ( ob.onChatPart ) {
                ob.onChatPart( ob, json.contents );
            }
        } else if ( json.tag === 'Chatted' ) {
            chatters[json.contents[0]] = true;
            if ( ob.onChatMessage ) {
                ob.onChatMessage( ob, json.contents[1] );
            }
        } else if ( json.tag === 'PlayerChanged' ) {
            if ( ob.onPlayerChange ) {
                ob.onPlayerChange( ob, json.contents[0] );
            }
        } else if ( json.tag === 'GameChangeset' ) {
            // this is more complex so delegate let's not pollute this place
            // with the implementation
            handleGameChangesets( json.contents );
        } else if ( json.tag === 'Games' ) {
            var result = _.map(json.contents, function(item) {
                return { key: item[0]
                       , name: item[1] };
            });
            _.each(pending_rgl_callbacks, function(fun) { fun( ob, result ); });
            pending_rgl_callbacks = [ ];

            if ( ob.onGamesList ) {
                ob.onGamesList( ob, result );
            }
        } else {
            // unknown message...
        }
    }

    // start right away by trying to handshake
    sender( { tag: 'Authenticate'
            , name: name } );

    return ob;
}

var cp437_table = { };
cp437_table[0] = 0x00a0;
cp437_table[1] = 0x263a;
cp437_table[2] = 0x263b;
cp437_table[3] = 0x2665;
cp437_table[4] = 0x2666;
cp437_table[5] = 0x2663;
cp437_table[6] = 0x2660;
cp437_table[7] = 0x2022;
cp437_table[8] = 0x25d8;
cp437_table[9] = 0x25cb;
cp437_table[10] = 0x25d9;
cp437_table[11] = 0x2642;
cp437_table[12] = 0x2640;
cp437_table[13] = 0x266a;
cp437_table[14] = 0x266b;
cp437_table[15] = 0x263c;
cp437_table[16] = 0x25ba;
cp437_table[17] = 0x25c4;
cp437_table[18] = 0x2195;
cp437_table[19] = 0x203c;
cp437_table[20] = 0x00b6;
cp437_table[21] = 0x00a7;
cp437_table[22] = 0x25ac;
cp437_table[23] = 0x21a8;
cp437_table[24] = 0x2191;
cp437_table[25] = 0x2193;
cp437_table[26] = 0x2192;
cp437_table[27] = 0x2190;
cp437_table[28] = 0x221f;
cp437_table[29] = 0x2194;
cp437_table[30] = 0x25b2;
cp437_table[31] = 0x25bc;
cp437_table[32] = 0x00a0;   // non-breaking space prevents spans from
                            // becoming empty and messing up the terminal
// values between 32-126 are the same as ASCII
cp437_table[127] = 0x2302;
cp437_table[128] = 0x00c7;
cp437_table[129] = 0x00fc;
cp437_table[130] = 0x00e9;
cp437_table[131] = 0x00e2;
cp437_table[132] = 0x00e4;
cp437_table[133] = 0x00e0;
cp437_table[134] = 0x00e5;
cp437_table[135] = 0x00e7;
cp437_table[136] = 0x00ea;
cp437_table[137] = 0x00eb;
cp437_table[138] = 0x00e8;
cp437_table[139] = 0x00ef;
cp437_table[140] = 0x00ee;
cp437_table[141] = 0x00ec;
cp437_table[142] = 0x00c4;
cp437_table[143] = 0x00e5;
cp437_table[144] = 0x00c9;
cp437_table[145] = 0x00e6;
cp437_table[146] = 0x00c6;
cp437_table[147] = 0x00f4;
cp437_table[148] = 0x00f6;
cp437_table[149] = 0x00f2;
cp437_table[150] = 0x00fb;
cp437_table[151] = 0x00f9;
cp437_table[152] = 0x00ff;
cp437_table[153] = 0x00d6;
cp437_table[154] = 0x00dc;
cp437_table[155] = 0x00a2;
cp437_table[156] = 0x00a3;
cp437_table[157] = 0x00a5;
cp437_table[158] = 0x20a7;
cp437_table[159] = 0x0192;
cp437_table[160] = 0x00e1;
cp437_table[161] = 0x00ed;
cp437_table[162] = 0x00f3;
cp437_table[163] = 0x00fa;
cp437_table[164] = 0x00f1;
cp437_table[165] = 0x00d1;
cp437_table[166] = 0x00aa;
cp437_table[167] = 0x00ba;
cp437_table[168] = 0x00bf;
cp437_table[169] = 0x2310;
cp437_table[170] = 0x00ac;
cp437_table[171] = 0x00bd;
cp437_table[172] = 0x00bc;
cp437_table[173] = 0x00a1;
cp437_table[174] = 0x00ab;
cp437_table[175] = 0x00bb;
cp437_table[176] = 0x2591;
cp437_table[177] = 0x2592;
cp437_table[178] = 0x2593;
cp437_table[179] = 0x2502;
cp437_table[180] = 0x2524;
cp437_table[181] = 0x2561;
cp437_table[182] = 0x2562;
cp437_table[183] = 0x2556;
cp437_table[184] = 0x2555;
cp437_table[185] = 0x2563;
cp437_table[186] = 0x2551;
cp437_table[187] = 0x2557;
cp437_table[188] = 0x255d;
cp437_table[189] = 0x255c;
cp437_table[190] = 0x255b;
cp437_table[191] = 0x2510;
cp437_table[192] = 0x2514;
cp437_table[193] = 0x2534;
cp437_table[194] = 0x252c;
cp437_table[195] = 0x251c;
cp437_table[196] = 0x2500;
cp437_table[197] = 0x253c;
cp437_table[198] = 0x255e;
cp437_table[199] = 0x255f;
cp437_table[200] = 0x255a;
cp437_table[201] = 0x2554;
cp437_table[202] = 0x2569;
cp437_table[203] = 0x2566;
cp437_table[204] = 0x2560;
cp437_table[205] = 0x2550;
cp437_table[206] = 0x256c;
cp437_table[207] = 0x2567;
cp437_table[208] = 0x2568;
cp437_table[209] = 0x2564;
cp437_table[210] = 0x2565;
cp437_table[211] = 0x2559;
cp437_table[212] = 0x2558;
cp437_table[213] = 0x2552;
cp437_table[214] = 0x2553;
cp437_table[215] = 0x256b;
cp437_table[216] = 0x256a;
cp437_table[217] = 0x2518;
cp437_table[218] = 0x250c;
cp437_table[219] = 0x2588;
cp437_table[220] = 0x2584;
cp437_table[221] = 0x258c;
cp437_table[222] = 0x2590;
cp437_table[223] = 0x2580;
cp437_table[224] = 0x03b1;
cp437_table[225] = 0x00df;
cp437_table[226] = 0x0393;
cp437_table[227] = 0x03c0;
cp437_table[228] = 0x03a3;
cp437_table[229] = 0x03c3;
cp437_table[230] = 0x00b5;
cp437_table[231] = 0x03c4;
cp437_table[232] = 0x03a6;
cp437_table[233] = 0x0398;
cp437_table[234] = 0x03a9;
cp437_table[235] = 0x03b4;
cp437_table[236] = 0x221e;
cp437_table[237] = 0x03c6;
cp437_table[238] = 0x03b5;
cp437_table[239] = 0x2229;
cp437_table[240] = 0x2261;
cp437_table[241] = 0x00b1;
cp437_table[242] = 0x2265;
cp437_table[243] = 0x2264;
cp437_table[244] = 0x2320;
cp437_table[245] = 0x2321;
cp437_table[246] = 0x00F7;
cp437_table[247] = 0x2248;
cp437_table[248] = 0x00b0;
cp437_table[249] = 0x2219;
cp437_table[250] = 0x00b7;
cp437_table[251] = 0x221a;
cp437_table[252] = 0x207f;
cp437_table[253] = 0x00b2;
cp437_table[254] = 0x25a0;
cp437_table[255] = 0x00a0;

exports.cp437ToChar = function(cp437_code) {
    return String.fromCharCode(cp437_table[cp437_code]);
}

