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
    characterImage.src = 'assets/img/sheldon.png';


    var w = window;
    requestAnimationFrame = w.requestAnimationFrame ||
                        w.webkitRequestAnimationFrame ||
                        w.msRequestAnimationFrame ||
                        w.mozRequestAnimationFrame;

    var gameState, myState;
    var draw = function () {
        ostracon.requestState();
        gameState = ostracon.getState();
        myState = {
            x: gameState[ostracon.myTeam+'X'],
            y: gameState[ostracon.myTeam+'Y']
        };

        if (bgReady) {
            ctx.drawImage(bgImage, 0, 0);
        }
        if (characterReady && myState) {
            ctx.drawImage(characterImage, 30 + myState.x * (canvas.width - 90),
                30 + myState.y * (canvas.height - 90));
        }

        requestAnimationFrame(draw);
    };

    draw();
}


function Ostracon () {
    ostracon = this;

    ostracon.myTeam = "mark";

    this.start = function() {
        ostracon.initWebSocket();
    };

    ostracon.initWebSocket = function() {
        if (!("WebSocket" in window)) {
            alert("Your browser doesn't support WebSockets! Try Chrome :P");
            return;
        }
        console.log("beginning connection:");
        ostracon.ws = new WebSocket("ws://" + window.location.host + "/websocket");

        ostracon.ws.onopen = function () {
            console.log("Connection started...");
            ostracon.requestState();
            ostracon.open = true;
        };
        ostracon.ws.onmessage = ostracon.handleMessage;
        ostracon.ws.onclose = ostracon.handleWSClose;

    };

    ostracon.handleWSClose = function() {
        console.log("WebSocket connection killed.");
        ostracon.open = false;
        ostracon.ws = null;
        setTimeout(function() {
            console.log("Attempting reconnect.");
            ostracon.initWebSocket();
        }, 10*1000);
    };

    ostracon.getState = function() {
        return ostracon.state;
    };

    ostracon.formatVote = function (voteText) {
        var voteObject = {
            type: "vote",
            vote: voteText,
            team: ostracon.myTeam
        };
        return voteObject;
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
                ostracon.pushVote(ostracon.formatVote("left"));
            break;

            case 38: // up
                ostracon.pushVote(ostracon.formatVote("up"));
            break;

            case 39: // right
                ostracon.pushVote(ostracon.formatVote("right"));
            break;

            case 40: // down
                ostracon.pushVote(ostracon.formatVote("down"));
            break;

            default: return; // exit this handler for other keys
        }
        e.preventDefault(); // prevent the default action (scroll / move caret)
    });
}

