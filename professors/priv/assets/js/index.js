
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

    var playerNames = ["mark", "noah", "ming", "couch", "monaco"];

    var players = {};


    function generatePlayers() {
        for (var playerIndex in playerNames) {
            var thisPlayerName = playerNames[playerIndex];
            players[thisPlayerName] = new Player(thisPlayerName);
        }
    }

    function Player(playerName) {
        var player = this;
        player.name = playerName;
        player.ready = false;
        player.image = new Image();
        player.image.onload = function() {player.ready = true;};
        player.image.src = 'assets/img/' + playerName + '.png';
    }

    var w = window;
    requestAnimationFrame = w.requestAnimationFrame ||
                        w.webkitRequestAnimationFrame ||
                        w.msRequestAnimationFrame ||
                        w.mozRequestAnimationFrame;

    var gameState, myState;
    var draw = function () {
        ostracon.requestState();
        gameState = ostracon.getState();

        if (bgReady) {
            ctx.drawImage(bgImage, 0, 0);
        }
        if (gameState) {
            var currentPlayer = "";
            var thisState = {x: 0, y: 0};
            for (var playerKey in players) {
                currentPlayer = players[playerKey];
                thisState = {
                    x: gameState[playerKey+'X'],
                    y: gameState[playerKey+'Y']
                };
                if (currentPlayer['ready']) {
                    ctx.drawImage(currentPlayer['image'],
                            30 + (thisState.x * (canvas.width - 90)),
                            30 + (thisState.y * (canvas.height - 90)));
                }
            }
        }

        requestAnimationFrame(draw);
        //setTimeout(function() {requestAnimationFrame(draw);}, 1000);

    };


    generatePlayers();
    draw();
}


function Ostracon () {
    ostracon = this;

    ostracon.myTeam = "mark";

    ostracon.setTeam = function(teamName) {
        ostracon.myTeam = teamName;
    };

    ostracon.start = function() {
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
    enablePlayerToggle(ostracon);

});


function enablePlayerToggle() {
    $('label.btn.player').on('click', function (event) {
        var newPlayer = $(event.target).children('input').prop('id');
        ostracon.setTeam(newPlayer);
    });
}

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

