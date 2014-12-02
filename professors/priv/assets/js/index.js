
function startGame(ostracon) {
    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext("2d");
    canvas.width = 1080;
    canvas.height = 480;

    $('#canvas-holder').append(canvas);

    // Set up background img
    var bgReady = false;
    var bgImage = new Image();
    bgImage.onload = function() {
        bgReady = true;
    };
    bgImage.src = 'assets/img/background.png';

    var playerNames = ["monaco", "noah", "ming", "couch", "mark"];

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
        player.score = -1;
    }

    function setPlayerScore(player, newScore) {
        player.score = newScore;
        $("span.score#"+player.name).text(newScore);
    }

    var w = window;
    requestAnimationFrame = w.requestAnimationFrame ||
                        w.webkitRequestAnimationFrame ||
                        w.msRequestAnimationFrame ||
                        w.mozRequestAnimationFrame;

    var gameState, myState;
    var printed = false;
    var draw = function () {
        ostracon.requestState();
        gameState = ostracon.getState();

        if (bgReady) {
            ctx.drawImage(bgImage, 0, 0);
        }
        if (gameState) {
            var currentPlayer;
            var thisState = {x: 0, y: 0};
            for (var playerKey in players) {
                currentPlayer = players[playerKey];

                if (playerKey != 'monaco'){
                    if (gameState[playerKey+'Score'] != currentPlayer.score) {
                        setPlayerScore(currentPlayer, gameState[playerKey+'Score']);
                    }
                }

                if (currentPlayer['ready']) {
                    ctx.drawImage(currentPlayer['image'],
                           30 + (gameState[playerKey+'X'] * (canvas.width - 90)),
                           30 + (gameState[playerKey+'Y'] * (canvas.height - 90)));
                }
            }
        }

        requestAnimationFrame(draw);
        //setTimeout(function() {requestAnimationFrame(draw);}, 1000);

    };


    generatePlayers();
    draw();
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

