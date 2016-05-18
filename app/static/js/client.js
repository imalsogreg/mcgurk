"use strict";

// This script is included by the audience-member client page
// It's assumed that jquery is included earlier, and that
// all the selectors have matching dom elmeents in the
// client html file (no dom elements are created by this script)

var modalities = ['Saw','Heard'];
var syllables  = ['Fa', 'Ga', 'Ba', 'Da', 'Tha'];

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

console.log( wsUrl() );

// // Collect references to parts of the page we'll be twiddling
// function pageParts(){
//     function pagePart(selector){
//         s = $(s);
//         assert s.length > 0;
//         return s;
//     }
//     parts.sawBox = pagePart('sawbox');
//     parts.heardBox = pagePart('heardbox');
//     return parts;
// }


// // Package a click as a message to the server
// function responseMessage(modality,syllable){
//     if (!(modalities.includes(modality) && syllables.includes(syllable))) {
//         throw new Error ('Bad inputs to responseMessage');
//     };
//     var msg = {};
//     msg.tag = 'Respond';
//     msg.contents = { respModality: modality,
//                      respSyllable: syllable};
//     return msg;
// }

// Package a click as a message to the server
function responseMessage(modality,syllable){
    if (!(modalities.includes(modality) && syllables.includes(syllable))) {
        throw new Error ('Bad inputs to responseMessage');
    };
    var msg = { respModality: modality,
                respSyllable: syllable};
    return msg;
}


function makeButtons(){

    var c = $('.content');

    modalities.forEach(function(m) {

        var d = document.createElement('div');

        syllables.forEach(function(s){

            var b = makeButton(m,s);
            $(d).append(b);

        });

        c.append(d);
    });
}

function makeButton(modality, syllable){
    var roomNum = thisRoom();
    var b = document.createElement('button');
    function postSuccess(d, s, x){
        console.log('In postSuccess');
        console.log(d);
        console.log(s);
    }
    b.className = "button btn-default";
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
