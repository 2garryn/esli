
// just for design 

var link_is_shown = false;
var is_grey = false;
var time = 3;
var timer_descr = 0;

$(function () {
  $(window).load(function () {
    $('#input_url').focus(); 
  });
});
$(document).ready(function(){
   $("#copy").hover(function () {
	   $(this).css( 'cursor', 'pointer');
	   $(this).css( 'color', '#333333');  
      }, 
      function () {
	   $(this).css( 'cursor', 'default');
	   $(this).css( 'color', '#999999');
   }
       );
	   
});

$(document).keypress(function(e) {
    if(e.which == 13) {
        request();

    }
});


function on_button(){
    $('#b1').css('background-color',"#ffffff");
    $('#b1').css( 'cursor', 'pointer');
};

function out_button(){
    $('#b1').css('background-color',"#cccccc");
    $('#b1').css( 'cursor', 'default');
};



function request(){

    var value = jQuery.trim($('#input_url').val());   
    if (value != ""){
	if (check_valid(value)){
	    right_url(value);
	    return true
	}
	else{
	    $('#input_url').val(value);
	    wrong_url("Url is wrong! Check it :-)");
	    return false;
	}
    }
    else {
	$('#input_url').val("");
	$('#input_url').focus(); 
	return false;
    };
}

function check_valid(value){
    if (value.split(' ').length > 1)
	return false
    else
        return true;
}

function wrong_url(Msg){
    clear_msg_field()
    clear_is_link();
    clear_is_long_link();
    show_msg_field(Msg);
    $('#input_url').focus();
}


function clear_msg_field(){
    $('#msg_field').text("");
}

function show_msg_field(msg){
    $('#msg_field').fadeIn(150);
    $('#msg_field').text(msg);
    
}

function clear_is_link(){
    $('#is_link').attr('href',"");
    $('#is_link').text(""); 
    $('#copy').text("");
    link_is_shown  = false;
}

function clear_is_long_link(){
    $('#is_long_link').attr('href',"");
    $('#is_long_link').text(""); 
}

function show_is_link(Link){
    $('#is_link').fadeIn(150);
    $('#is_link').attr('href',Link);
    $('#is_link').text(Link); 
    $('#copy').fadeIn(150);
    $('#copy').text("-copy-");
    link_is_shown  = true;
}

function show_is_long_link(Link){
    var shortlink;
    if (Link.length > 80)
	shortlink = Link.substr(0, 80) + "..."
    else
	shortlink = Link;
	    
    $('#is_long_link').fadeIn(150);
    $('#is_long_link').attr('href', Link);
    $('#is_long_link').text(shortlink); 
}

function clear_input_url(){
    $('#input_url').val("");
}

function right_url(URL){
    send_request(URL);
}


function send_request(value){
    var response;
    $.ajax({
	    type: "POST",
		url: "create",
		data: value,
		success: function(response){processing_ok(response);},
		dataType: "text",
		statusCode: {
		    400: function() {
		    processing_bad_request()
			},
		    501: function() {
		    processing_server_error()
			},
		    423: function() {
		    return 0
			}
		}
	});
}

function processing_ok(response){
    clear_msg_field();
    clear_is_link();
    show_is_link(response); 
    var ll = $('#input_url').val();
    clear_is_long_link();
    show_is_long_link(ll);
    clear_input_url();
    disabling();
}

function processing_bad_request(){
    wrong_url("Url is wrong! Check it :-)");
}
function processing_server_error(){
    wrong_url("Something goes wrong :-( Try later...");
}


function cut_long_url(str, len){
    return str;
}

function disabling(){
    $('#input_url').attr('disabled','disabled');
    $('#b1').attr('disabled','disabled');
    $('#counter').text(time);
    timer_descr = setInterval(function() {check_int()}, 1000);
    return 0;
}

function check_int(){
    time -= 1;
    if (time == 0){
	$('#input_url').removeAttr('disabled');
	$('#b1').removeAttr('disabled');
	$('#counter').text('');
	time = 3;
	clearInterval(timer_descr);
	$('#input_url').focus();
    } else {
	$('#counter').text(time);
    }
}