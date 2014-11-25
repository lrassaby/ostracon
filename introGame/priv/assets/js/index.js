
function startGame(ostracon) {
  //totals represent total keystrokes, 
  //states represent the keystroke frequency of the most recent state
  var leftTotal = 0;
  var leftState = 0;
  var rightTotal = 0;
  var rightState = 0;
  var upTotal = 0;
  var upState = 0;
  var downTotal = 0;
  var downState = 0;
  // Load the Visualization API and the piechart package.
  google.load('visualization', '1.1', {'packages':['bar']});

  // Set a callback to run when the Google Visualization API is loaded.
  google.setOnLoadCallback(init);

  // Callback that creates and populates a data table,
  // instantiates the pie chart, passes in the data and
  // draws it.
  function init() {
    // Create the data table.
    data = new google.visualization.arrayToDataTable(tableValues())
    
    function tableValues() {
      [['Keystroke', 'Total', 'Recent State'], 
      ['Left', leftTotal, leftState], 
      ['Right', rightTotal, rightState], 
      ['Up', upTotal, upState], 
      ['Down', downTotal, downState]
    ]);

    // Set chart options
    var options = { 
      'width':900,
      chart: {
        title:'Ostracon Visualization',
        subtitle: 'total on left, last state on the right'
      }, 
      series: {
        0: {axis: 'Total'}, 
        1: {axis: 'Recent State'}
      }, 
      axes: {
        y: {
          'Total': {label: 'votes'},
          'Recent State': {side: 'right', label: 'votes'}
        }
      }
    };


    // Instantiate and draw our chart, passing in some options.
    var chart = new google.charts.Bar(document.getElementById('chart_div'));

    function drawChart() {
      chart.draw(data, options);
    }
    drawChart();

    setInterval(function(){updateOstracon()}, 4000);
    
    function updateOstracon() {
      ostracon.requestState();
      gameState = ostracon.getState();
      if (gameState) { 
        leftState = gameState[left];
        rightState = gameState[right];
        upState = gameState[up];
        downState = gameState[down];
      }
      leftTotal += leftState;
      rightTotal += rightState;
      upTotal += upState;
      downTotal += downState;


      data = google.visualization.arrayToDataTable(tableValues);

      drawChart();
    }
  }
}


function Ostracon () {
    ostracon = this;

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
            team: 'myTeam'
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

    // window.setTeam = function(teamName) {
    //     ostracon.setTeam(teamName);
    // };

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

