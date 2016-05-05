
var useFlashImagesPresentation = true;

var recordChoices = true;

//---------


var game_end_message = "You have completed a game!  Click 'Next Game' to continue";
var experiment_end_message = "This stage is over!  Please alert the experimentor. <br /> <br /> <br />";

var stage = -1;
var game = 0;

var numberOfGamesInAMatch = 4;

var currentOrder = orders[0];
var currentRole = "director";

var currentStims = 'images';
var currentlyShownImage = 0;

var images = ['images/image1.png','images/image2.png','images/image3.png','images/image4.png','images/image5.png','images/image6.png','images/image7.png','images/image8.png'];

var coverImages = ["A.png",'B.png','C.png'];
var flashImageLength = 1000;
var flashOn = false;
var flashTimeoutVariable = 0;



var audioFiles = ['audio/CREAK.mp3', "audio/ice_cracking.mp3",'audio/AirLeak.mp3', "audio/Palloncino.mp3",'audio/arbre.mp3', "audio/bats.mp3",'audio/belf-0723-2.mp3', "audio/BoingBoing.mp3"];

var results = "game,stage,currentTarget,currentDistractors,timeSinceStart,choiceButton,choiceItem,date\n";


function clearImages (){
	for(var i=0; i<3 ;++i){
		hideMe("image"+i);
	}
}

function showMe(x){
	document.getElementById(x).style.display = 'inline';
}

function hideMe(x){
	document.getElementById(x).style.display = 'none';
}

function showImages(){
	var x= currentOrder[stage];

	var currentTarget = x[0][0];
	var currentDistractors = x[1];
	if(x[0].length > x[1].length){
		var currentTarget = x[1][0];
		var currentDistractors = x[0];
	}

	if(currentRole =='director'){
		clearImages();
		document.getElementById('image1').src = "images/image"+(currentTarget+1)+".png";
		
		showMe("image1");
	} else{
		clearImages();
		for(var i=0; i< currentDistractors.length; ++i){
			document.getElementById('image'+i).src = "images/image"+(currentDistractors[i]+1)+".png";

		}
		if(useFlashImagesPresentation){
			resetCoverImages();
		} else{
			showMe('image0');
			currentlyShownImage = 0;
		}

	}
}

function cycleImages(x){
	
	currentlyShownImage +=x;

	var curOr= currentOrder[stage];

	var currentTarget = curOr[0][0];
	var currentDistractors = curOr[1];
	if(curOr[0].length > curOr[1].length){
		var currentTarget = curOr[1][0];
		var currentDistractors = curOr[0];
	}
		
		if(currentlyShownImage>2){
			currentlyShownImage = 0;
		}
		if(currentlyShownImage<0){
			currentlyShownImage = 2;
		}
		
		clearImages();
		
		if(currentStims=='audio'){
			playAudio(audioFiles[currentDistractors[currentlyShownImage]]);
		}
		
		if(currentStims=='images' & useFlashImagesPresentation){
			document.getElementById("image"+currentlyShownImage).src = images[currentDistractors[currentlyShownImage]];
			setTimeout("resetCoverImage(" + currentlyShownImage+ ")",flashImageLength);
			flashOn = true;
		}

		showMe("image"+currentlyShownImage);
	
}

function flashImage(imNum, file){
	document.getElementById("image"+imNum).src = file;
	flashTimeoutVariable = setTimeout("resetCoverImages()",flashImageLength);
	flashOn = true;
}

function resetCoverImages(){
		
		for(var i=0; i < 3; ++ i){
			document.getElementById("image"+i).src = coverImages[i];
			showMe("image"+i);
		}
		flashOn = false;
}

function resetCoverImage(i){

			document.getElementById("image"+i).src = coverImages[i];
			showMe("image"+i);
			flashOn = false;
		
}

function pressNextRound(){

	// save timing
	var d = new Date();
	var n = d.getTime(); 
	var timeSinceStart = n - startTime;

	// in case we're in the middle of playing something
	clearTimeout(flashTimeoutVariable);
	flashOn = false;

	console.log("Next Round");
	console.log([game,stage,"","_",timeSinceStart,"","NextRound"])

	var d = new Date();
	var datestring = d.toLocaleString(); 

	trialString = [game,stage,"","_",timeSinceStart,"","NextRound",datestring].join();
	results += trialString +"\n";


	nextRound();
}

function nextRound(){
	stage += 1;

	hideMe("choose0");
	hideMe("choose1");
	hideMe("choose2");



	// decide what to do
	if(stage >=  currentOrder.length){
		// move on
		game += 1;

		if(game==numberOfGamesInAMatch){
			// end experiment
			endExperiment();
		} else{
			currentOrder = orders[game];

			stage = -1;
			clearImages();
			showMe("NextGame");
			document.getElementById("Instructions").innerHTML = game_end_message;
			hideMe("NextRound");
		}

	} else{
		// run a round

		if(!recordChoices){
			showMe("NextRound");
		}

		document.getElementById("Instructions").innerHTML = "Round "+(stage+1);

		if(currentRole=='director'){
			currentRole = 'matcher';
			if(recordChoices){
				hideMe("NextRound");
				setTimeout('showMe("choose0");',500);
				setTimeout('showMe("choose1");',500);
				setTimeout('showMe("choose2");',500);
			}
		} else{
			currentRole = 'director';
			if(recordChoices){
				setTimeout('showMe("NextRound");',500);
				hideMe("choose0");
				hideMe("choose1");
				hideMe("choose2");
			}
		}

		if(currentStims=="images"){
			showImages();
		} else{
			setUpAudio();
		}
	}	
}

goToPreviousRound = function(){
	console.log("PrevRound");
	results += "PrevRound\n";
	if(stage>0){
		stage -= 2;
	}
	nextRound();
}


nextGame = function(){
	
//	stage = -1;
//	game += 1;
//	currentOrder = orders[game];

	showMe("NextRound");
	hideMe("NextGame");
	hideMe("resultsText");
	document.getElementById("Instructions").innerHTML = "";
}


document.onkeypress = function (e) {
	
	var x = e.which || e.keyCode;
	if(x==98){
		goToPreviousRound();
	} 

    e = e || window.event;

	if(x==113){
			showCookie();
		}

    // use e.keyCode
    if(currentRole=='matcher' & !flashOn){
	    if(e.keyCode=='37'){
	    	// left arrow
	    	cycleImages(-1);
	    } 
	    if(e.keyCode=='39'){
	    	// right arrow
	    	cycleImages(1);
	    }

		
	
	}

};



endExperiment = function(){
	clearImages();
	document.getElementById("Instructions").innerHTML = experiment_end_message;
	hideMe("NextRound");
	hideMe("NextGame");

	for(var i=1;i<5; ++ i){
		showMe("StartDirectorImages"+i);
		showMe("StartDirectorImages"+i+"b");
		showMe("StartDirectorImages"+i+"c");
		showMe("StartDirectorImages"+i+"d");
	}

	showMe("resultsText");
	document.getElementById("resultsText").value = results;
	saveCookie();
}

saveCookie = function(){
	// cookies can only be 4k characters in length, so just keep the latest version

	var currentCookie = getCookie("data");
	console.log(["currentCookie",currentCookie]);
	//var cstring = "data="+ currentCookie + "\n" +results+"; expires=Thu, 18 Dec 2088 12:00:00 UTC; "; 
	//cstring="data=asdhasdbas; expires=Thu, 18 Dec 2088 12:00:00 UTC; "; 

	
	var results2 = results.replace(/\n/g, '#');

	if(results2.length > 4050){
		results2 = results2.slice(0,4050);
	}

	var cstring = "data="+ results2+"; expires=Thu, 18 Dec 2088 12:00:00 UTC; "; 
	console.log(cstring);
	document.cookie = cstring; 
}

showCookie = function(){
	console.log(document.cookie);
	var cx = getCookie("data");
	cx = cx.replace(/#/g,"\n");
	console.log(cx);
	document.getElementById("resultsTextCookie").value = cx;
	showMe("resultsTextCookie");
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length, c.length);
        }
    }
    return "";
}

setup = function(role, stimtype, order){

	// index starts from -1
	if(role=='director'){
		role='matcher';
	} else{
		role = 'director';
	}

	// mix up order
	if(order == 1){
		orders = [orders[2],orders[3],orders[0],orders[1]]
	}
	if(order == 2){
		orders = [orders[1],orders[2],orders[3],orders[0]]
	}
	if(order == 3){
		orders = [orders[3],orders[0],orders[1],orders[2]]
	}


	for(var i=1;i<5; ++ i){
		hideMe("StartDirectorImages"+i);
		hideMe("StartDirectorImages"+i+"b");
		hideMe("StartDirectorImages"+i+"c");
		hideMe("StartDirectorImages"+i+"d");
	}
	
	currentRole = role;
	currentStims = stimtype;	
	currentOrder = orders[0];

	stage = -1;
	game = 0;

	currentlyShownImage = 0;

	showMe("NextRound");
}

//-------------
playAudio = function(file){

	var audio = document.getElementById('audioX');
    var source = document.getElementById('sourceX');
	audio.pause();
	source.src = file;
	audio.load();
	audio.play();
}


setUpAudio = function(){
	clearImages();
	if(currentRole == "director"){
		document.getElementById("image1").src = "Play.png";
		showMe('image1');
	} else{
		resetCoverImages();
		showMe('image0');
		showMe('image1');
		showMe('image2');
	}
}

imageClicked = function(n){
	var curOr= currentOrder[stage];
	var currentDistractors = curOr[1];

	if(curOr[0].length > curOr[1].length){
			var currentDistractors = curOr[0];
		}

	if(currentStims=='audio'){
		playAudio(audioFiles[currentDistractors[n]]);
	} else{
		flashImage(n, images[currentDistractors[n]])
	}
}

preloadImages = function(){
	for(var i=0;i<images.length; ++i){
		document.getElementById("image0").src = images[i];
	}
	for(var i=0; i<audioFiles.length; ++i){
		document.getElementById('sourceX').src = audioFiles[i];
	}
}

//-------


chooseMe = function(choice){

	var d = new Date();
	var n = d.getTime(); 
	var timeSinceStart = n - startTime;

	// in case we're in the middle of playing something
	clearTimeout(flashTimeoutVariable);
	flashOn = false;

	var x= currentOrder[stage];

	var currentTarget = x[0][0];
	var currentDistractors = x[1];
	if(x[0].length > x[1].length){
		var currentTarget = x[1][0];
		var currentDistractors = x[0];
	}

	var d = new Date();
	var datestring = d.toLocaleString(); 

	trialString = [game,stage,currentTarget,currentDistractors.join("_"),timeSinceStart,choice,currentDistractors[choice],datestring].join();
	results += trialString +"\n";

	nextRound();
	
}

var dx = new Date();
var startTime = dx.getTime(); 

playAudio(audioFiles[0]);

clearImages();
preloadImages();
hideMe("NextGame");
hideMe("NextRound");
hideMe("resultsText");
hideMe("choose0");
hideMe("choose1");
hideMe("choose2");