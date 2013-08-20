/*
 * JavaScript UI enhancements
 */

$(function(){
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
		})
});
