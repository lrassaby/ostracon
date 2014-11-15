var ws = {};

function send()
{
    ws.send("hi!");
    console.log('sent');
}

function open()
{
    if (!("WebSocket" in window)) {
        alert("WebSocket NOT supported by your Browser!");
        return;
    }
    console.log('open');
    ws = new WebSocket("ws://" + window.location.host + "/websocket");
    ws.onopen = function() {
        console.log('connected');
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
}

$(document).ready(function() {
    open();
});
