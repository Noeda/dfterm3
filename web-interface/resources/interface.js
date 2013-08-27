/*
 * JavaScript UI enhancements
 */

$(function(){
	var _I={};
	function ModalWindow(node, opts) {
		var self = {};
		opts = $.extend({
			'overlay': true,
			'animation_dur': 500,
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
			.append($('<button>').text(opts.ok_text).click(self.ok_handler))
			.append($('<button>').text(opts.cancel_text).click(self.cancel_handler));
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
	$('.admin_flash').each(function(i, message){
		var hide_button = $('<a>').attr({href:'#'}).html('&times;')
			.appendTo($(message).find('p')).click(function(e){
				e.preventDefault();
				$(message).fadeOut(500);
			}).addClass('close button');
	});
	
	/*
	 * Change the change password form to be 'modal', hidden by default.
	 */
	change_password_form = ModalWindow('.admin_password_form');
	if ($('.admin_flash').length) {
		change_password_form.show(0);
		$('.admin_flash').each(function(i, e){
			if ($(e).text().toLowerCase().indexOf('password set') >= 0) {
				$(e).css({color: 'green'});
				setTimeout(change_password_form.hide, 500);
			}
		});
	}
	change_password_button = $('<button>').text('Change password')
		.insertBefore($('form[action=logout] input'))
		.click(function(e){
			e.preventDefault();
			change_password_form.show();
		});
	$('<button>').insertAfter(change_password_form.node.find('input[type=submit]'))
		.text('Cancel').click(function(e){
			e.preventDefault();
			change_password_form.hide();
		});
	
	/*
	 * Confirmation for logout
	 */
	$('form[action=logout] input[type=submit]').click(function(e){
		e.preventDefault();
		var form = $(this).closest('form');
		if (!_I.logout_confirmation) {
			_I.logout_confirmation = ModalConfirmation({
				title: 'Log out',
				text: 'Are you sure you want to log out now?',
				ok_text: 'Log out',
				ok: function(){
					form.submit();
				}
			});
		}
		_I.logout_confirmation.show();
	});
	
	window.Interface = _I;
});
