var ws = {};

function send() {
    ws.send("hi!");
    console.log('sent');
}

function initGame(state_query) {
    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext("2d");
    canvas.width = 512;
    canvas.height = 480;
    $('#canvas-holder').append(canvas);

    // Set up background img
    var bgReady = false;
    var bgImage = new Image()
    bgImage.onload = function() {
        bgReady = true;
    };
    bgImage.src = 'assets/img/background.png';

    // Set up character img
    var characterReady = false;
    var characterImage = new Image()
    characterImage.onload = function() {
        characterReady = true;
    };
    characterImage.src = 'assets/img/character.png';

    // Game objects 
    var character = {
        speed: 256,
        x: 1,
        y: 2
    };
    
    // Update game objects
    var render = function(state) {
        if (bgReady) {
            ctx.drawImage(bgImage, 0, 0);
        }
        if (characterReady) {
            ctx.drawImage(characterImage, state.character.x, state.character.y);
        }
    };

    var w = window;
    requestAnimationFrame = w.requestAnimationFrame || w.webkitRequestAnimationFrame || w.msRequestAnimationFrame || w.mozRequestAnimationFrame;

    var main = function () {
        
        render(state_query());

        //request to animate again
        requestAnimationFrame(main);
    };

    main();
};

function handleKeystrokes(voteHandler) {
    $(document).keydown(function(e) {
        switch(e.which) {
            case 37: // left
                voteHandler(formatVote("left"));
            break;

            case 38: // up
                voteHandler(formatVote("up"));
            break;

            case 39: // right
                voteHandler(formatVote("right"));
            break;

            case 40: // down
                voteHandler(formatVote("down"));
            break;

            default: return; // exit this handler for other keys
    }
    e.preventDefault(); // prevent the default action (scroll / move caret)
});
}

function formatVote(voteText) {
    var voteObject = {
        type: "vote",
        vote: voteText,
        team: null
    };
    return voteObject;
}


function initWebSocket()
{
    if (!("WebSocket" in window)) {
        alert("WebSocket NOT supported by your Browser!");
        return;
    }

    ws = new WebSocket("ws://" + window.location.host + "/websocket");
    ws.onopen = function() {
        console.log('ws connected');
    };
    ws.onmessage = function (evt)
    {
        var received_msg = evt.data;
        console.log("Received: " + received_msg);
        $("<li class='msg'>" + received_msg + "</li>" ).appendTo("#messages");
    };
    ws.onclose = function()
    {
        // websocket is closed.
        console.log('close');
    };
};

function Ostracon (initState) {
    this.wsReady = false;

    this.initWebSocket = function() {
        if (!("WebSocket" in window)) {
            alert("WebSocket NOT supported by your Browser!");
            return;
        }

        this.ws = new WebSocket("ws://" + window.location.host + "/websocket");

        this.ws.onopen = function () {this.wsReady = true};
        this.ws.onmessage = this.handleMessage;
        this.ws.onclose = this.handleClose;
    };

    initWebSocket();

    this.currentState = initState;
    
    this.getState = function() {
        this.requestState();
        return this.currentState;
    };

    this.pushVote= function(voteInfo) {
        if (this.wsReady) {
            this.ws.send(JSON.stringify(voteInfo));            
        }
    };

    this.validateState = function(stateMsgObject) {
        if (stateMsgObject['type'] = 'stateresponse') {
            return true;
        }
        else {
            return false;
        }
    };

    this.handleMessage = function(msg) {
        var stateMsgObject = JSON.parse(msg);
        if (this.validateState(stateMsgObject)) {
            this.currentState = stateMsgObject['state'];
        }
    };

    this.handleClose = function() {
        this.wsReady = false;
    };

    this.requestState = function() {
        var requestObject = {
            type: 'statequery'
        }
        if (this.wsReady) {
            this.ws.send(JSON.stringify(requestObject));
        }
    }

    return this;
};

$(document).ready(function() {
    initState = {
        character: {
            x: 0,
            y: 0
        }
    };

    var ostra = Ostracon(initState);
    console.log(ostra);

    initGame(ostra.getState);
    handleKeystrokes(ostra.pushVote);

    //initOstracon();
});

