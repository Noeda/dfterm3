/* This draws a status "bar" (not necessarily a bar because the appearance is
 * determined by style sheet)
 *
 * At the moment, the bar simply displays if you have connected and as which
 * user you are logged in as.
 *
 * Quick usage:
 *
 *     var status = new StatusBar({ model: connection })
 *     document.body.appendChild(status.el);
 *
 * where 'connection' is a Dfterm3Connection from connection.js.
 */

var Backbone = require('backbone');
var _ = require('underscore');

var StatusBar = Backbone.View.extend({
    id:        "status-bar"
   ,tagName:   "p"

   ,initialize: function() {
       this.listenTo( this.model
                    , "change:connected " +
                      "change:logged_in " +
                      "change:guest " +
                      "change:connecting"
                    , this.render );
       this.render();
   }

   ,render: function() {
       if ( this.model.get("connected") === false ) {
           if ( this.model.get("connecting") === true ) {
               this.$el.text("connecting");
           } else {
               this.$el.text("not connected");
           }
       } else if ( this.model.get("logged_in") === false ) {
           this.$el.text("not logged in");
       } else if ( this.model.get("guest") === true ) {
           this.$el.text("guest");
       } else {
           this.$el.text("logged in as " + this.model.get("username"));
       }
   }
    });

module.exports = { "StatusBar": StatusBar }

