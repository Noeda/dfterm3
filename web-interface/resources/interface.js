/*
 * JavaScript UI enhancements
 */

$(function(){
	var _I={};
	function ModalWindow(node, opts) {
		var self = {};
		opts = $.extend({
			'overlay': true,
			'animation_dur': 250,
			'hidden': true
		}, opts);
		self.animation_dur = opts.animation_dur;
		// Make sure node is a single jQuery element
		node = $(node);
		if (node.length > 1) {
			node = node.first();
		}
		node.addClass('modal-window');
		self.node = node;
		// Create an overlay
		if (opts.overlay) {
			self.overlay = $('<div>').addClass('overlay').appendTo(node.parent());
		}
		else {
			self.overlay = $('<div>');
		}
		
		if (opts.hidden) {
			self.node.hide(0);
			self.overlay.hide(0);
		}
		
		self.hide = function(dur){
			if (dur === undefined) {
				dur = self.animation_dur;
			}
			self.node.fadeOut(dur);
			self.overlay.fadeOut(dur);
		};
		self.show = function(dur){
			if (dur === undefined) {
				dur = self.animation_dur;
			}
			self.node.fadeIn(dur);
			self.overlay.fadeIn(dur);
		}
		
		self.close_button = $('<a>').addClass('close button').html('&times;')
			.prependTo(self.node).css({'float':'right'}).click(function(e){
				e.preventDefault(); self.hide();
			}).attr('href', '#');
		
		if (this instanceof ModalWindow) $.extend(this, self); return self;
	}
	_I.ModalWindow = ModalWindow;
	
	function ModalConfirmation(opts) {
		var self = {
			node:$('<div>').hide().appendTo('body').css({
				height: '20%',
				width: '40%',
				'margin-left': '10%',
				'font-size': '1.2em',
			})
		};
		self.opts = opts = $.extend({
			ok_text: 'OK',
			cancel_text: 'Cancel',
			title: 'Confirm',
			text: '',
			'default': 1,
			timeout: false,
			modal: true,
			ok: function(){},
			cancel: function(){},
			can_close: false,
		}, opts);
		
		self.win = ModalWindow(self.node, {overlay:opts.modal});
		if (!opts.can_close) {
			self.win.close_button.hide();
		}
		
		self.events = $({});
		self.events.on('ok', opts.ok);
		self.events.on('cancel', opts.cancel);
		
		self.ok_handler = function(e){
			e.preventDefault();
			self.events.trigger('ok').trigger('close', true);
		};
		self.cancel_handler = function(e){
			e.preventDefault();
			self.events.trigger('cancel').trigger('close', false);
		};
		
		self.node.append($('<p>').css({'font-weight': 'bold', 'margin-top':0}).text(opts.title))
			.append($('<p>').text(opts.text))
			.append($('<p>')
				.append($('<button>').addClass('btn btn-primary').text(opts.ok_text).click(self.ok_handler))
				.append($('<button>').addClass('btn btn-default').text(opts.cancel_text).click(self.cancel_handler))
			);
		self.node.find('button').css({
			'float': 'right',
		});
		
		self.show = function(){
			self.win.show();
			return self.events;
		};
		
		self.hide = function(){
			self.win.hide();
			return self.events;
		};
		
		self.events.on('close', self.hide);
		
		if (this instanceof ModalConfirmation) $.extend(this, self); return self;
	}
	_I.ModalConfirmation = ModalConfirmation;
	
	/*
	 * Add a close/hide button to error messages
	 * Clicking the button causes the message to fade out
	 */
	$('.admin_flash_success, .admin_flash_failure').each(function(i, message){
		message = $(message).addClass('alert alert-dismissable');
		var close_button = $('<button>').attr({type:'button'}).html('&times;').addClass('close').click(function(e){
			e.preventDefault();
			$(message).fadeOut(300);
		});
		if (message.find('p').length)
			close_button.appendTo(message.find('p:nth(0)'));
		else
			close_button.appendTo(message);
	});
	
	$('.admin_flash_failure').addClass('alert-danger');
	$('.admin_flash_success').addClass('alert-success');
	
	/*
	 * Change the change password form to be modal, hidden by default.
	 */
	change_password_form = ModalWindow('div.admin_password_form');
	// Check for errors and display the password form again if they exist
	if ($('.admin_flash_failure').length && window.location.href.indexOf('change_password') > 0) {
		change_password_form.show(0);
		// Move message above password form
		$('.admin_flash_failure').insertAfter(change_password_form.node.find('h3'));
	}
	$('<button>').insertAfter(change_password_form.node.find('input[type=submit]'))
		.text('Cancel').addClass('btn btn-default').click(function(e){
			e.preventDefault();
			// clear form
			change_password_form.node.find('input[type=password]').val('');
			change_password_form.hide();
		});
	change_password_form.node.find('input[type=submit]').addClass('btn btn-primary');
	
	/*
	 * Move buttons to navbar
	 */
	
	var logout_form = $('form[action=logout]').hide();
	$('<li>').append($('<a href="#">Log out</a>')).appendTo('#admin-nav').click(function(e){
		e.preventDefault();
		logout_form.submit();
	});
	
	// Dropdown
	$('#admin-nav').prepend($('<li class="dropdown">' +
		'<a href="#" class="dropdown-toggle" data-toggle="dropdown">Settings <b class="caret"></b></a>' +
		'<ul class="dropdown-menu"></ul></li>'
	));
	
	$('#admin-nav ul.dropdown-menu').append('<li><a href="#">Change password</a></li>').click(function(e){
		e.preventDefault();
		change_password_form.show();
	});
	
	/*
	 * Button classes
	 */
	
	$('form[action=modify_game] input[type=submit][name=unregister]')
		.addClass('btn btn-danger');
	
	// Change all generic submit buttons to .btn-primary
	$('input[type=submit]').not('[class*=btn]').addClass('btn btn-primary');
	
	/*
	 * Make the manual registration form modal
	 */
	
	manual_add_form = ModalWindow('div.manual_add_game');
	$('<button>').text('Register a Dwarf Fortress manually').addClass('btn btn-default')
		.insertAfter('div.manual_add_game').click(manual_add_form.show);
	
	/*
	 * Confirmation for unregistering games
	 */
	$('form[action=modify_game] input[type=submit][name=unregister]').click(function(e){
		e.preventDefault();
		var form = $(this).closest('form'),
			game_name = $(this).closest('tr').find('td.game_name').text();
		ModalConfirmation({
				title: 'Unregister game',
				text: 'Are you sure you want to unregister the game "' + game_name + '"?',
				ok_text: 'Unregister',
				ok: function(){
					form.find('[name=unregister]').attr('type', 'hidden');
					form.submit();
				}
		}).show();
	});
	
	
	window.Interface = _I;
});
