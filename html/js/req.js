
function get_short_link(){
	$.ajax({
		type: "POST",
		url: "create",
		data: $('#input_url').val(),
		success: function(response){processing_answer(response);},
		dataType: "text",
		statusCode: {
			400: function() {
				processing_bad_request()
			},
			501: function() {
				processing_server_error()
			}
		}
	});
}

$(function () {
  $(window).load(function () {
    $('#input_url').focus();
  });
})


function processing_answer(response){
//	alert(response);
	var content, new_content;
//	content = $('#sl_indicator').html();
	new_content = '<a href="'+response+'\">' +response + '</a><br><br>';
	$('#sl_indicator').html(new_content);
	$('#input_url').val("");
}

function processing_bad_request(){
	alert("bad request");
}
function processing_server_error(){
	alert("server error")
}