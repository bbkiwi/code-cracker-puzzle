//global var to hold json response
var jsonResults = {};

function reportResults() {
  console.log(jsonResults)
}

function loadDoc(url, cfunc, id) {
  var xhttp;
  xhttp=new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      jsonResults = JSON.parse(xhttp.responseText);
      cfunc(xhttp, id);
    }
  };
  xhttp.open("GET", url, true);
  xhttp.send();

}
function setInnerHTMLAJAXresponse(xhttp, id) {
  document.getElementById(id).innerHTML = xhttp.responseText;
}

function setInnerHTMLAJAXresponseByCssSelector(xhttp, selector) {
  document.querySelector(selector).innerHTML = xhttp.responseText;
}

function setInnerHTMLAJAXstoredResponseByCssSelector(selector) {
//  console.log(jsonResults)
//  console.log(JSON.stringify(jsonResults))
//  document.querySelector(selector).innerHTML = jsonResults.fname;
//  document.querySelector(selector).innerHTML = JSON.stringify(jsonResults);
  document.querySelector(selector).innerHTML = jsonResults.letter;
}

function doNothing(xhttp, id) {
}


$( document ).ready(function() {

    $( window ).load(function() { 

var demo = document.getElementById("easeSVG"),
	$controls = $("#controls"),
	easeArray = [  "Power0", "Power1", "Power2", "Power3", "Power4", "Linear", "Sine", "Expo", "Circ", "Bounce", "Back", "Elastic" ],
	easeType = ".easeIn", // initial ease selection
	tm = 1, // initial tween time
	colorArray = ["#94c356", "#46a4cc", "#50a39e", "#a63e4b", "#e3aa59", "#a63ba0", "#a2a2a2", "#0f0f0f"], // swatch colors
	iconArray = [".basic",".alien",".dogs",".cinema"], //icon groups
	$dragger = $(".dragger"),
	$dragBounds = $(".dragBounds"),
	regularBorder = "#B8ABA4", // non active button border
	activeBorder = "#29abe2", // currently selected option
	$swatches = $(".swatch"),
	$boardCellButton = $(".boardCellControl"),
	$iconButton = $(".iconButton"),
	$easeButton = $(".easeButton");


TweenMax.set([ $boardCellButton[9], $iconButton[0], $easeButton[0] ], {borderColor:activeBorder}); // highlight initial settings as active buttons
TweenMax.set([$controls, demo], {transformOrigin:"center", xPercent:-50});


$swatches.each(function(i) {
	TweenMax.set(this, {backgroundColor:colorArray[i]}) // fill the 8 swatches from the color array
});

/* 	set the dragger in the 1.0 second position
	onDrag update the text and time variable
 	dragBounds width = 226
	set a 6 second drag limit: 226/6 = 37.66 
*/

TweenMax.set($dragger,{x:37.66});


Draggable.create( $dragger, {
	type:"x",
	bounds: $dragBounds,
	onDrag: trackDrag
});

function trackDrag() {
	tm = (this.x/37.66).toFixed(1);
//    loadDoc('http://localhost:8080/savedata?slider=' + tm, setInnerHTMLAJAXresponseByCssSelector, '#cellInfo strong');
    loadDoc('http://localhost:8080/savedata?slider=' + tm, doNothing, "dummy");
	$( ".dragData strong" ).html( tm );
}



// gently bring all elements into view over 3/4 seconds
TweenMax.to([$controls, demo], .75, {autoAlpha:1});



/* ---------------------------------------------------------------
	 Button controls for all the options 
	---------------------------------------------------------------*/

//change the base color of the symbols
$swatches.click(function() {
	TweenMax.set( $boardCellButton, { backgroundColor:colorArray[ $(this).index() ] });
//	console.log(colorArray[ $(this).index() ])
})	

//change the icon theme and highlight the active button
$iconButton.click(function() {
	var iconType = this.getAttribute("data-icon");
	TweenMax.set(iconArray,{autoAlpha:0}); // sets all icons transparent
	TweenMax.set(iconType,{autoAlpha:1});  // sets desired icon non-transparent	
	TweenMax.set($iconButton,{borderColor:regularBorder});
	TweenMax.set(this, {borderColor:activeBorder});	
})

//animate the viewBox and highlight the active button
$boardCellButton.click(function() {
//	var newView = this.getAttribute("data-view");
//	TweenMax.to(demo, 1, {attr:{viewBox:newView}, ease:Power3.easeInOut});
	TweenMax.set($boardCellButton,{borderColor:regularBorder})
	TweenMax.set(this, {borderColor:activeBorder});
	console.log('http://localhost:8080/savedata?letter=' + $(this).text() + "&position=" + this.id)
//	loadDoc('http://localhost:8080/savedata?letter=' + $(this).text() + "&position=" + this.id, setInnerHTMLAJAXresponse, 'cellInfo')
//	loadDoc('http://localhost:8080/savedata?letter=' + $(this).text() + "&position=" + this.id, setInnerHTMLAJAXresponseByCssSelector, '#cellInfo strong')
	loadDoc('http://localhost:8080/savedata?letter=' + $(this).text() + "&position=" + this.id, doNothing, '#cellInfo strong')
//    $( "#cellInfo strong" ).html( 'letter=' + $(this).text() + "&position=" + this.id);
})

//switch the ease type and highlight the active button
$easeButton.click(function() {
	easeType = this.getAttribute("data-ease");
	$( ".currentEase strong" ).html( easeType );
	TweenMax.set($easeButton,{borderColor:regularBorder});
	TweenMax.set(this, {borderColor:activeBorder});	
})


$("#runButton").click(function() {
    setInnerHTMLAJAXstoredResponseByCssSelector('#cellInfo strong')
})
	// end window load
	});
// end doc ready
});