/*
 * This file implements the DOM login box to login into Dfterm3.
 *
 * var lbox = require('login_box');
 *
 * This module exports just one function:
 *
 *     var x = lbox.makeLoginBox( style_prefix );
 *
 * This returns an object. x.dom is a DOM element you might want to attach to
 * the document.
 *
 * Call x.setD3( d3 ) when you have a Dfterm3 connection and a dfterm3_protocol
 * object from that module. If the connection is lost, you can call
 * x.setD3( null ); Calling setD3 will make the login box hook onLogin and
 * onLoginFailure.
 *
 * The login box hides itself if there is no connection or d3 is already logged
 * in.
 *
 * Styling:
 *
 * There is no styling by default but the class names are as follows:
 *
 * The form has class 'login_box'.
 * The name field has class 'login_box_name'.
 * The label for name field has class 'login_box_name_label'.
 * The submit button has class 'login_box_submit'.
 */

exports.makeLoginBox = function( style_prefix )
{
    var obj = { };
    var d3 = null;
    style_prefix = style_prefix || '';


    var setclass = function( el, class_name ) {
        el.setAttribute( 'class', style_prefix + class_name );
    }
    var setattr = function( el, attr, value ) {
        el.setAttribute( attr, value );
    }

    // All the stuffs are inside a form.
    var elem = document.createElement( 'form' );
    setclass( elem, 'login_box' );
    elem.style.display = 'none'; // which by default should not be seen

    // At the moment we only have one field to fill: the name
    // Later we might have a password field as well.
    var name_field_label = document.createElement( 'label' );
    setclass( name_field_label, 'login_box_name_label' );
    elem.appendChild( name_field_label );

    name_field_label.textContent = 'Name';

    var name_field = document.createElement( 'input' );
    setclass( name_field, 'login_box_name' );
    name_field_label.appendChild( name_field );

    setattr( name_field, 'type', 'text' );
    setattr( name_field, 'placeholder', 'type your username here' );
    setattr( name_field, 'required', true );
    setattr( name_field, 'maxlength', 30 ); // The server limits this to 30

    // Put the submit button on this thing.
    var submit_button = document.createElement( 'input' );
    setclass( submit_button, 'login_box_submit' );
    elem.appendChild( submit_button );

    // A message that tells if the login failed.
    var login_failed = document.createElement( 'p' );
    setclass( login_failed, 'login_box_failed' );
    elem.appendChild( login_failed );
    login_failed.style.display = 'none';
    login_failed.textContent = 'Login failed';

    setattr( submit_button, 'type', 'submit' );
    setattr( submit_button, 'value', 'Login' );

    // Login submitting
    elem.onsubmit = function(event) {
        d3.authenticate( name_field.value );
        login_failed.style.display = 'none';
        event.preventDefault();
    }

    // Checks if the login box should be visible.
    var updateLoginBoxVisibility = function() {
        // No displaying it if we don't have any connection.
        if ( d3 && d3.status() === 'handshaking' ) {
            elem.style.display = 'inline';
        } else {
            elem.style.display = 'none';
        }
    }

    var hooked_to;

    var hookTo = function( d3 ) {
        if ( d3 === null ) {
            hooked_to = null;
            return;
        }
        // Try to prevent hooking twice on the same d3
        // It's not reliable. (hook to 1 -> hook to 2 -> hook back to 1 is not
        // detected).
        //
        // Probably could be made to detect it always but let's have some faith
        // in users not doing the above hook cycle.
        if ( hooked_to === d3 ) {
            return;
        }
        var old = d3.onLogin;
        d3.onLogin = function(d3) {
            updateLoginBoxVisibility();
            if ( old ) {
                return old(d3);
            }
        }
        old = d3.onLoginFailure;
        d3.onLoginFailure = function(d3 ) {
            updateLoginBoxVisibility();
            login_failed.style.display = 'inline';
            if ( old ) {
                return old(d3);
            }
        }
        hooked_to = d3;
    }

    obj.setD3 = function( new_d3 ) {
        d3 = new_d3;
        hookTo( d3 );
        updateLoginBoxVisibility();
        login_failed.style.display = 'none';
    }

    obj.dom = elem;
    return obj;
}

