/* The UI part where list of games are shown.
 *
 * Currently, as DOM objects, it's a table with two columns:
 *
 *
 * Game name  |  Instance name
 * ---------------------------
 *   DF       |   Great game
 *   DF       |   Community game
 *   ...      |      ...
 *
 *
 * Only shown if connected to the server. This one has to be updated manually
 * but it will be updated automatically once upon connection to the server.
 *
 * Quick usage:
 *
 *     var game_list = new GameSelector({ model: connection });
 *     document.body.appendChild(game_list.el);
 *
 * You can manually update the game list by calling on connection:
 *
 *     connection.update_game_list();
 *
 * where 'connection' is a Dfterm3Connection from connection.js.
 */

var Backbone = require('backbone');
var _ = require('underscore');

var GameSelector = Backbone.View.extend({
    id:       "game-selector"
   ,tagName:  "ul"

   ,initialize: function() {
       this.listenTo( this.model
                    , "change:available_games " +
                      "change:connected " +
                      "change:logged_in "
                    , this.render );
       this.render();
   }

   /* make sure not to open doors for XSS attacks... */
   ,template: _.template(
       '<table>' +
       '<tr><th>Type</th><th>Game</th></tr>' +
       '<% _.each(games, function(game) { %> <tr>' +
       '<td><%- game.game_name %></td>' +
       '<td><%- game.instance_name %></td>' +
       '</tr> <% }); %>' +
       '</table>'
       )

   ,render: function() {
       if ( this.model.get("connected") === true &&
            this.model.get("logged_in") === true ) {
           var games = this.model.get("available_games");
           this.$el.html( this.template( {games: games} ) );
           this.$el.show();
       } else {
           this.$el.hide();
       }
   }
});

module.exports = { "GameSelector": GameSelector };

