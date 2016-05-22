"use strict";

// This script is included by the audience-member client page

var syllables  = ['Fa', 'Ga', 'Ba', 'Da', 'Tha'];
var modalityIcons = {Heard: 'headphones',
                     Saw: 'eye-open'};

function roomColor(n){
    var h = Math.floor(n * 300/7);
    return "hsla(" + h + ",29%,57%,1)"
}

function thisRoom(){
    var roomNum = parseInt(window.location.pathname.substring(1));
    if (isNaN(roomNum )) {
        throw new Error ('Bad room number from page path');
    }
    return roomNum;
}


// Compute the websocket url from the page's path info
function wsUrl(){
    var pageloc = window.location;
    var wsloc = "";
    var roomnum = thisRoom();
    if (pageloc.protocol === "https:") {
        wsloc = "wss:";
    } else if (pageloc.protocol === "http:") {
        wsloc = "ws:";
    } else {
        throw new Error ('Unexpected page location protocol: ' + wsloc);
    }
    wsloc += "//" + pageloc.host +  '/api1/browser-socket' + pageloc.pathname;
    return wsloc
}


// Package a click as a message to the server
function responseMessage(modality,syllable){
    if ((modalityIcons[modality] == undefined) || !syllables.includes(syllable)){
        throw new Error ('Bad inputs to responseMessage: ' + modality + ', ' + syllable);
    };
    var msg = { respModality: modality,
                respSyllable: syllable};
    return msg;
}


function makeButtons(){

    var c = $('.content');

    for (var m in modalityIcons) {

        var d      = document.createElement('div');
        var header = document.createElement('span');
        $(d).append(header);

        header.setAttribute('class', 'glyphicon glyphicon-' + modalityIcons[m]);
        header.setAttribute('aria-hidden','true');

        syllables.forEach(function(s){

            var b = makeButton(m,s);
            $(d).append(b);

        });

        c.append(d);
    }

}

function makeButton(modality, syllable){
    var roomNum = thisRoom();
    var b = document.createElement('button');
    function postSuccess(d, s, x){
        console.log('In postSuccess');
        console.log(d);
        console.log(s);
    }
    b.className = "btn btn-default btn-lg";
    b.setAttribute('type','button');
    $(b).on('click', function() {
        var pUrl = '/api1/respond/' + roomNum;
        var d = responseMessage(modality, syllable);
        console.log("2 POSTing to: " + pUrl)
        $.ajax(pUrl,
               {type: "POST",
                headers: {"Content-Type": "application/json"},
                data: JSON.stringify(d),
                success: postSuccess
               })
        // $.post(pUrl, JSON.stringify(d), postSuccess, 'json');
        // $.post(pUrl, JSON.stringify(d));
    });
    b.setAttribute('Value', syllable);
    b.innerHTML = syllable;
    return b;
}

makeButtons();
$('.content').css('background-color', roomColor( thisRoom() ));
$('.glyphicon').css('color', roomColor( thisRoom() ));
