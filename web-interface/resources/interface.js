/*
 * JavaScript UI enhancements
 */

$(function(){
	$('.admin_flash').each(function(i, message){
		var hide_button = $('<a>').attr({href:'#'}).html('&times;')
			.appendTo($(message).find('p')).click(function(e){
				e.preventDefault();
				$(message).fadeOut(500);
			}).addClass('close button');
	});
});
