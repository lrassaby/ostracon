function startGame(ostracon) {
    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext("2d");
    canvas.width = 512;
    canvas.height = 480;

    $('#canvas-holder').append(canvas);

    // Set up background img
    var bgReady = false;
    var bgImage = new Image();
    bgImage.onload = function() {
        bgReady = true;
    };
    bgImage.src = 'assets/img/background.png';

    // Set up character img
    var characterReady = false;
    var characterImage = new Image();
    characterImage.onload = function() {
        characterReady = true;
    };
    characterImage.src = 'assets/img/character.png';


    var w = window;
    requestAnimationFrame = w.requestAnimationFrame || w.webkitRequestAnimationFrame || w.msRequestAnimationFrame || w.mozRequestAnimationFrame;

    var draw = function () {
        ostracon.requestState();
        var state = ostracon.getState();

        if (bgReady) {
            ctx.drawImage(bgImage, 0, 0);
        }
        if (characterReady && state) {
            ctx.drawImage(characterImage, 30 + state.x * (canvas.width - 90), 30 + state.y * (canvas.height - 90));
        }

        requestAnimationFrame(draw);
    };

    draw();
}

function formatVote(voteText) {
    var voteObject = {
        type: "vote",
        vote: voteText,
        team: null
    };
    return voteObject;
}


function Ostracon () {
    ostracon = this;

    this.start = function() {
        if (!("WebSocket" in window)) {
            alert("Your browser doesn't support WebSockets! Try Chrome :P");
            return;
        }

        ostracon.ws = new WebSocket("ws://" + window.location.host + "/websocket");

        ostracon.ws.onopen = function () {
            console.log("Connection started...");
            ostracon.requestState();
            ostracon.open = true;
        };

        ostracon.ws.onmessage = ostracon.handleMessage;
        ostracon.ws.onclose = function () {
            ostracon.ws = {};
            ostracon.open = false;
        };
    };
    ostracon.getState = function() {
        return ostracon.state;
    };
    ostracon.pushVote= function(voteInfo) {
        if (ostracon.ws && ostracon.open) {
            ostracon.ws.send(JSON.stringify(voteInfo));
        }
    };

    ostracon.handleMessage = function(msg) {
        var parsed_message = JSON.parse(msg.data);
        switch(parsed_message.type) {
            case "stateresponse":
                ostracon.state = parsed_message.response;
                break;
            case "voteresponse":
                break;
            default:
                break;
        }
    };

    ostracon.requestState = function() {
        var requestObject = {
            type: 'statequery'
        };
        if (ostracon.ws && ostracon.open) {
            ostracon.ws.send(JSON.stringify(requestObject));
        }
    };

    ostracon.start();
}

$(document).ready(function() {
    var ostracon = new Ostracon();
    startGame(ostracon);
    makeKeystrokeHandler(ostracon);
});


function makeKeystrokeHandler(ostracon) {
    $(document).keydown(function(e) {
        switch(e.which) {
            case 37: // left
                ostracon.pushVote(formatVote("left"));
            break;

            case 38: // up
                ostracon.pushVote(formatVote("up"));
            break;

            case 39: // right
                ostracon.pushVote(formatVote("right"));
            break;

            case 40: // down
                ostracon.pushVote(formatVote("down"));
            break;

            default: return; // exit this handler for other keys
        }
        e.preventDefault(); // prevent the default action (scroll / move caret)
    });
}

