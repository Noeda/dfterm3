dfterm3_playing = function() {
    // imports
    var do_after = dfterm3_timings.do_after;
    var seconds = dfterm3_timings.seconds;
    var fade_out = dfterm3_timings.fade_out;
    var fade_in = dfterm3_timings.fade_in;

    var exports = { };

    // protocol message types.
    var SCREEN_DATA = 1;
    var JSON_DATA = 2;
    var CHAT_MESSAGE = 3;
    var LOGIN_REJECTED = 3;
    var LOGIN_ACCEPTED = 4;
    var GAME_IS_GONE = 5;

    var logged_in = false;

    if ( WebSocket === undefined ) {
        exports.websockets_are_supported = false;
        return exports;
    }
    exports.websockets_are_supported = true;

    // Call this when you want to connect to Dfterm3.
    exports.play = function( host           // Where is Dfterm3? This is a
                                            // WebSocket address, e.g.
                                            // "ws://127.0.0.1:8000/"
                           , host_dom_element
                                              // ^ The DOM element inside which
                                              // all the game stuff will be
                                              // put in.
                           ) {
        var terminal_dom_element = document.createElement("div");
        var title = document.createElement("span"); // title above the terminal
                                                    // itself.
        var terminal = undefined;                  // The terminal. Inside this
                                                   // you can see the actual
                                                   // game.
        var br_element = document.createElement("br"); // separates title from
                                                       // the game
        var second_br_element = document.createElement("br"); // separates game
                                                              // from whatever
                                                              // comes after it
        var status_element = document.createElement("span");

        var go_back_button = document.createElement("div");
        var go_back_button_a = document.createElement("a");
        go_back_button_a.textContent = "Go back to game list";
        go_back_button_a.setAttribute("href", "#");
        go_back_button.appendChild(go_back_button_a);
        go_back_button.setAttribute("class", "go_back_button");

        go_back_button_a.onclick = function () {
            socket.send("\x02");
        }

        title.setAttribute("id", "game_title");

        var cover = document.createElement("div");
        cover.setAttribute("class", "cover");

        var showCover = function() {
            cover.style.display = "block";
        }
        var hideCover = function() {
            cover.style.display = "none";
        }

        terminal_dom_element.setAttribute( "class"
                                         , "dfterm3" );

        var game_is_gone_box = document.createElement("h3");
        game_is_gone_box.setAttribute("id", "game_is_gone_box");
        game_is_gone_box.textContent = "Game is not active";
        game_is_gone_box.style.display = "none";



        var who_is_playing_box = document.createElement("h3");
        var resetWhoIsPlaying = function() {
            who_is_playing_box.textContent = "No one is currently playing";
        }
        resetWhoIsPlaying();
        who_is_playing_box.setAttribute("id", "who_is_playing");

        var chat_box = document.createElement("div");
        var login_form = document.createElement("form");
        var login_text = document.createElement("input");
        var login_title = document.createElement("h3");
        var login_label = document.createElement("label");
        var login_hint = document.createElement("h4");

        login_hint.setAttribute("id", "login_hint");
        login_hint.textContent = "You need to login before you can play.";
        login_form.setAttribute("action", "#chat");
        login_text.setAttribute("type", "text");
        login_label.textContent = "User name:";
        login_form.appendChild( login_title );
        login_form.appendChild( login_hint );
        login_form.appendChild(login_label);
        login_form.appendChild( login_text );
        login_text.setAttribute("id", "login_text");
        login_text.setAttribute("maxlength", 20);
        login_title.textContent = "Login";

        chat_box.appendChild( login_form );
        chat_box.setAttribute( "class", "chat" );
        var chat_title = document.createElement("h3");
        chat_title.textContent = "Chat";
        chat_box.appendChild( chat_title );
        var chat_form = document.createElement("form");
        var chat_text = document.createElement("input");
        var chat_hr = document.createElement("hr")
        chat_form.setAttribute("action", "#chat");
        chat_text.setAttribute("type", "text");
        chat_text.style.display = "none";
        chat_text.setAttribute("maxlength", 800);
        chat_form.appendChild( chat_text );
        chat_box.appendChild( chat_form );
        chat_box.appendChild( chat_hr );

        var clearChat = function () {
            var start;
            while ( (start = chat_hr.nextSibling) ) {
                chat_box.removeChild(start);
            }
        }

        login_form.onsubmit = function (event) {
            var login_name = login_text.value;
            login_text.value = "";

            socket.send("\x04" + login_name);
            event.preventDefault();
        }

        chat_form.onsubmit = function (event) {
            var msg = chat_text.value;
            chat_text.value = "";

            socket.send("\x03" + msg);
            event.preventDefault();
        }

        host_dom_element.appendChild( cover );
        host_dom_element.appendChild( terminal_dom_element );

        status_element.textContent =
            "Trying to make a connection to Dfterm3 server..."
        status_element.setAttribute( "class"
                                   , "dfterm3_connection_status_element_good" );

        title.textContent = "Unnamed"

        var status = function( str, badness ) {
            fade_status_in_after( 0.0 );
            status_element.textContent = str;
            if ( badness ) {
                status_element.setAttribute(
                          "class"
                        , "dfterm3_connection_status_element_bad" );
            } else {
                status_element.setAttribute(
                          "class"
                        , "dfterm3_connection_status_element_good" );
            }
            fade_status_out_after( 2.0 );
        }

        var fade_status_out_after = function ( time ) {
            fade_out( status_element, time );
        }

        var fade_status_in_after = function ( time ) {
            fade_in( status_element, time );
        }

        // Removes the terminal elements from the page, if they are there.
        // Otherwise does nothing.
        var stopTerminal = function () {
            if ( !terminal ) {
                return;
            }
            terminal_dom_element.removeChild( terminal.getDOMObject() );
            terminal_dom_element.removeChild( who_is_playing_box );
            terminal_dom_element.removeChild( game_is_gone_box );
            terminal_dom_element.removeChild( title );
            terminal_dom_element.removeChild( br_element );
            terminal_dom_element.removeChild( second_br_element );
            terminal_dom_element.removeChild( go_back_button );
            terminal_dom_element.removeChild( chat_box );
            terminal = undefined;

            resetWhoIsPlaying();
            clearChat();
        }

        // Starts a terminal on the page. Removes the old terminal if it is
        // there.
        var startTerminal = function( w, h ) {
            stopTerminal();
            terminal = dfterm3_terminal.createTerminal( w, h );

            terminal_dom_element.appendChild( title );
            terminal_dom_element.appendChild( game_is_gone_box );
            terminal_dom_element.appendChild( who_is_playing_box );
            terminal_dom_element.appendChild( br_element );
            terminal_dom_element.appendChild( terminal.getDOMObject() );
            terminal_dom_element.appendChild( second_br_element );
            terminal_dom_element.appendChild( go_back_button );
            terminal_dom_element.appendChild( chat_box );

            who_is_playing_box.style.display = "inline";
            game_is_gone_box.style.display = "none";

            terminal.getDOMObject().setAttribute("tabindex", 1);

            var key = function(prefix) {
                return function(event) {
                    socket.send(prefix + JSON.stringify(
                                { which: event.which
                                , code_point: 0
                                , shift: event.shiftKey
                                , alt:  event.altKey
                                , ctrl: event.ctrlKey }));

                    if ( event.which == 39 ||
                         event.which == 37 ||
                         event.which == 38 ||
                         event.which == 40 ||
                         event.which == 9 ||
                         event.which == 32 ) {
                         event.preventDefault();
                    }
                }
            }

            var keypress = function(event) {
                socket.send("\x07" + JSON.stringify(
                            { which: 0
                            , code_point: event.charCode
                            , shift: event.shiftKey
                            , alt:  event.altKey
                            , ctrl: event.ctrlKey }));
            }

            terminal.getDOMObject().addEventListener("keypress", keypress, true);
            terminal.getDOMObject().addEventListener( "keydown"
                                                    , key("\x05"), true);
            terminal.getDOMObject().addEventListener("keyup"
                                                    , key("\x06")
                                                    , true);
        }

        title.setAttribute("class", "dfterm3_terminal_title_bar");

        terminal_dom_element.appendChild( status_element );

        // This function parses the WebSocket data received from Dfterm3 when
        // it is screen data. It updates the terminal (if there is one).
        var handleScreenData = function( array ) {
            stopGameList();
            var array_view = new Uint8Array( array, 1 );
            var w = (array_view[0] << 8) | array_view[1];
            var h = (array_view[2] << 8) | array_view[3];
            if ( !terminal ) {
                startTerminal( w, h );
            } else {
                terminal.resize( w, h );
            }
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

        var game_list_ul = undefined;
        var game_list_h1 = undefined;

        var stopGameList = function() {
            if ( !game_list_ul ) {
                return;
            }
            terminal_dom_element.removeChild( game_list_ul );
            terminal_dom_element.removeChild( game_list_h1 );
            game_list_ul = undefined;
        }

        var startGameList = function(msg) {
            stopGameList();

            var loading_in_progress = false;

            game_list_ul = document.createElement("ul");
            for ( var i = 0; i < msg.length; ++i ) {
                var x = function() {
                    var li = document.createElement("li");
                    var a = document.createElement("a");
                    var loading_indicator = document.createElement("span");
                    loading_indicator.setAttribute( "class"
                                                  , "loading_indicator" );
                    loading_indicator.style.display = "none";
                    loading_indicator.textContent = "starting up...";

                    a.setAttribute("href", "#");
                    a.textContent = msg[i][0];
                    var choice = msg[i][1];
                    var content = msg[i][0];
                    a.onclick = function () {
                        if ( loading_in_progress === false ) {
                            loading_in_progress = true;
                            socket.send("\x01" + choice);
                            title.textContent = content;
                            loading_indicator.style.display = "inline";
                        }
                    }
                    li.appendChild(a);
                    li.appendChild(loading_indicator);
                    game_list_ul.appendChild(li);
                }();
            }

            game_list_h1 = document.createElement("h1");
            game_list_h1.textContent = "Choose your game";

            if ( msg.length == 0 ) {
                game_list_h1.textContent = "No games currently running";
            }

            terminal_dom_element.appendChild( game_list_h1 );
            terminal_dom_element.appendChild( game_list_ul );
        }

        var handleGameListMessage = function(msg) {
            stopTerminal();
            startGameList(msg);
        }

        var handleChatMessage = function(from_who, content) {
            var line = document.createElement("p");
            var from_who_span = document.createElement("span");
            line.appendChild(from_who_span);
            var content_span = document.createElement("span");
            line.appendChild(content_span);

            from_who_span.setAttribute("class", "chat_from_who");
            content_span.setAttribute("class", "chat_content");

            from_who_span.textContent = from_who + ":"
            content_span.textContent = content;
            chat_box.insertBefore( line, chat_hr.nextSibling );
        }

        var jsonMessage = function(msg) {
            if ( msg[0] == "game_list" ) {
                handleGameListMessage( msg[1] );
            } else if ( msg[0] == "chat" ) {
                handleChatMessage( msg[1], msg[2] );
            } else if ( msg[0] == "who_is_playing" ) {
                who_is_playing_box.textContent = "Last played by: " + msg[1];
            }
        }

        showCover();

        var socket = new WebSocket( host );
        socket.onopen = function() {
            status("Connection established.", false );
            hideCover();
        }
        socket.onclose = function() {
            status("No connection to server.", true );
            showCover();
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
                    var reader2 = new FileReader();
                    reader2.readAsText(event.data);
                    reader2.onload = function() {
                        jsonMessage( JSON.parse(reader2.result.substring(1)) );
                    }
                } else if ( array_view[0] == LOGIN_REJECTED ) {
                    status( "Login rejected", true );
                } else if ( array_view[0] == LOGIN_ACCEPTED ) {
                    status( "Logged in", false );
                    chat_text.style.display = "block";
                    login_form.style.display = "none";
                } else if ( array_view[0] == GAME_IS_GONE ) {
                    game_is_gone_box.style.display = "inline";
                    who_is_playing_box.style.display = "none";
                }
            }
        }
    }

    return exports;
}();

