
function createChart(ostracon) {

  google.load("visualization", "1", {packages:["corechart"]});
  google.setOnLoadCallback(runTableLoop);

    //totals represent total keystrokes, 
  var totalsArray = [['Keystroke', 'Total'], 
                      ['Left', 10], 
                      ['Right', 1], 
                      ['Up', 1], 
                      ['Down', 9]];

  var currentData;

  function runTableLoop() {
    // Create the data table.
    currentData = google.visualization.arrayToDataTable(totalsArray);

    // Set chart options
    var options = { 
       width:900,
       title:'0 Connected',
       titleTextStyle: {fontSize: '20px'},
       pieSliceText:'value'
    };

    // Instantiate and draw our chart, passing in some options.
    var chart = new google.visualization.PieChart(document.getElementById('chart_div'));

    var chartState;

    chart.draw(currentData, options);

    function chartStateToArray (chartState) {
      leftTotal = chartState["left"];
      rightTotal = chartState["right"];
      upTotal = chartState["up"];
      downTotal = chartState["down"];
      return [['Keystroke', 'Total'], 
                      ['Left', leftTotal], 
                      ['Right', rightTotal], 
                      ['Up', upTotal], 
                      ['Down', downTotal]];
    }

    setInterval(function(){updateChart()}, 5000);

    function isStateDifference(state1, state2) {
      if ((typeof state1 == "undefined") || (typeof state2 == "undefined")) {
        return true;
      } 
      isDifferent = false;
      for (key in state1) {
        isDifferent = isDifferent || state1[key] != state2[key];
      } 
      return isDifferent;
    }

    function updateChart() {
      ostracon.requestState();
      var newChartState = ostracon.getState();
      if (newChartState && isStateDifference(newChartState, chartState)) { 
        chartState = newChartState;
        currentData = google.visualization.arrayToDataTable(chartStateToArray(chartState));
        options['title'] = chartState['connected'] + " Users Connected";
        chart.draw(currentData, options);
      }
    }
  }
}


$(document).ready(function() {
    var ostracon = new Ostracon();
    createChart(ostracon);
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
