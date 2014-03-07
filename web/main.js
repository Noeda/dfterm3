var jquery = require('jquery2');
var Backbone = require('backbone');
Backbone.$ = jquery;

var connection = require('./connection.js');
var login = require('./login_ui.js');
var status_ui = require('./status_bar.js');

var ob = new connection.Dfterm3Connection;
var login_box = new login.LoginBox({ model: ob });
var status_bar = new status_ui.StatusBar({ model: ob });

document.body.appendChild(status_bar.el);
document.body.appendChild(login_box.el);

