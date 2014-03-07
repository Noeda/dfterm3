/* This draws the login UI box.
 *
 * The box will only be shown if:
 *
 *    * We are connected to the server.
 *    * We are not logged in.
 *    * The connection has not been set to implicitly login
 *
 * It will be automatically shown if these conditions get satisfied.
 *
 * Quick usage:
 *    var login = new LoginBox({ model: connection })
 *    document.body.appendChild(login.el);
 *
 * where 'connection' is a Dfterm3Connection from connection.js.
 */

var Backbone = require('backbone');
var _ = require('underscore');

var LoginBox = Backbone.View.extend({
    id: "login-box"
   ,tagName:   "form"

   ,initialize: function() {
       this.listenTo( this.model
                    , "change:connected " +
                      "change:logged_in " +
                      "change:do_implicit_login"
                    , this.render );
       this.render();
   }

   ,events: {
       "submit": "submit"
   }

   ,submit: function() {
       return false;
   }

   ,template: _.template(
       '<label id="username_label" for="username">User name</label>' +
       '<input id="username" type="text"><br>' +
       '<label id="password_label" for="password">Password</label>' +
       '<input id="password" type="password"><br>' +
       '<button id="login_button" class="button" type="submit">Login</button>')

   ,render: function() {
       if ( this.model.get("connected") === true &&
            this.model.get("logged_in") === false &&
            this.model.get("do_implicit_login") === false ) {
           this.$el.html(this.template());
           this.$el.show();
       } else {
           this.$el.hide();
       }
       return this;
   }

});

module.exports = { "LoginBox": LoginBox }

