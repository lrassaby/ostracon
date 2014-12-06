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
        ostracon.ws =
            new WebSocket("ws://" + window.location.host + "/websocket");

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
