var ws = new Object();

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
    ws = new WebSocket("ws://"+window.location.host+"/websocket");
    ws.onopen = function() {
        console.log('connected');
    };
    ws.onmessage = function (evt)
    {
        var received_msg = evt.data;
        console.log("Received: " + received_msg);
        var txt = document.createTextNode("Got from server: " + received_msg);
        document.getElementById('messages').appendChild(txt);
    };
    ws.onclose = function()
    {
        // websocket is closed.
        console.log('close');
    };
}