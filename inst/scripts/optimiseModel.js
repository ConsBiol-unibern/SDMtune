// Variables rendered by Whisker
var settings = {{{ settings }}};
var data = {{{ data }}};

var scatterData = {
  datasets: [{
    label: "Training",
    pointRadius: 3,
    pointHoverRadius: 5,
    borderColor: "rgb(245, 132, 16)",
    backgroundColor: "rgba(245, 132, 16, .7)",
    borderWidth: .7,
    data: []
  }]
};
// Add Validation dataset if metric in not AICc
if (settings.metric[0] !== "AICc") {
  scatterData.datasets.push({
    label: "Validation",
    pointRadius: 3,
    pointHoverRadius: 5,
    borderColor: "rgb(75, 192, 192)",
    backgroundColor: "rgba(75, 192, 192, .7)",
    borderWidth: .7,
    data: []
  })
}

var lineData = {
  labels: settings.labels,
  datasets: [{
    label: "Training",
    pointRadius: 3,
    pointHoverRadius: 5,
    borderColor: "rgb(245, 132, 16)",
    backgroundColor: "rgba(245, 132, 16, .7)",
    fill: false,
    lineTension: 0,
    borderWidth: .7,
    borderDash: [5],
    data: [],
  }]
};
// Add Validation dataset if metric in not AICc
if (settings.metric[0] !== "AICc") {
  lineData.datasets.push({
    label: "Validation",
    pointRadius: 3,
    pointHoverRadius: 5,
    borderColor: "rgb(75, 192, 192)",
    backgroundColor: "rgba(75, 192, 192, .7)",
    fill: false,
    lineTension: 0,
    borderWidth: .7,
    borderDash: [5],
    data: [],
  })
}

var scatterOptions = {
  responsive: true,
  title: {
    display: true,
    fontFamily: "sans-serif",
    padding: 15,
    text: data.title[0]
  },
  legend: {
    position: "bottom",
    labels: {
      fontFamily: "sans-serif",
      usePointStyle: true
    }
  },
  scales: {
    yAxes: [{
      scaleLabel: {
        display: true,
        labelString: settings.metric[0]
      }
    }],
    xAxes: [{
      scaleLabel: {
        display: true,
        labelString: "model"
      },
      ticks: {
        max: settings.pop[0],
        callback: function(value) {
          if (value % 1 === 0) {
            return value;
          }
        }
      }
    }]
  },
  tooltips: {
    mode: "x",
    footerFontStyle: "normal",
    callbacks: {
      label: function(tooltipItem, data) {
        var label = data.datasets[tooltipItem.datasetIndex].label || '';
        if (label) {
          label += ": ";
        }
        label += tooltipItem.yLabel;
        return label;
      },
      footer: function(tooltipItems, data) {
        var footer = window.data.scatterFooter[tooltipItems[0].index];
        if (settings.metric[0] !== "AICc") {
          var footer = "Diff: " + (tooltipItems[0].yLabel - tooltipItems[1].yLabel).toFixed(4) + "\n" + footer;
        }
        return footer;
      }
    }
  }
};

var lineOptions = {
  responsive: true,
  title: {
    display: false
  },
  legend: {
    position: "bottom",
    labels: {
      fontFamily: "sans-serif",
      usePointStyle: true
    }
  },
  scales: {
    yAxes: [{
      scaleLabel: {
        display: true,
        labelString: settings.metric[0]
      }
    }],
    xAxes: [{
      scaleLabel: {
        display: true,
        labelString: "generation"
      }
    }]
  },
  tooltips: {
    mode: "x",
    footerFontStyle: "normal",
    callbacks: {
      title: function (tooltipItems, data) {
        return window.data.lineTitle[tooltipItems[0].index]
      },
      label: function (tooltipItem, data) {
        var label = data.datasets[tooltipItem.datasetIndex].label || "";
        if (label) {
          label += ": ";
        }
        label += tooltipItem.yLabel;
        return label;
      },
      footer: function (tooltipItems, data) {
        var footer = window.data.lineFooter[tooltipItems[0].index];
        if (settings.metric[0] !== "AICc") {
          var footer = "Diff: " + (tooltipItems[0].yLabel - tooltipItems[1].yLabel).toFixed(4) + "\n" + footer;
        }
        return footer;
      }
    }
  }
};

init = function() {
  window.chartScatter.data.datasets[0].data = data.train;
  if (settings.metric[0] !== "AICc") {
    window.chartScatter.data.datasets[1].data = data.val;
  }
  window.chartScatter.options.title.text = data.title[0];
  window.chartScatter.update();

  window.chartLine.data.datasets[0].data = data.best_train;
  if (settings.metric[0] !== "AICc") {
    window.chartLine.data.datasets[1].data = data.best_val;
  }
  window.chartLine.update();
};

update = function() {
  var refresh = setInterval(loadData, 1000);
  function loadData() {
    $.ajax({
      method: "GET",
      url: "data.json",
      cache: false
    }).done(function (json) {
      data = JSON.parse(json);
      init();

      if (data.stop[0]) {
        clearInterval(refresh);
      }

    })
  };
};

window.onload = function() {
  // Set a wider content if page is displayed in the browser
  if (window.location.href.search("[?&]viewer_pane=") === -1) {
    document.querySelector(".content").style.maxWidth="600px";
  }
  var ctx1 = document.getElementById("ctx1").getContext("2d");
  window.chartScatter = new Chart(ctx1, {
    type: "scatter",
    data: scatterData,
    options: scatterOptions,
  });
  var ctx2 = document.getElementById("ctx2").getContext("2d");
  window.chartLine = new Chart(ctx2, {
    type: "line",
    data: lineData,
    options: lineOptions,
  });
  // Init charts
  init();
  // Update in case of real time chart
  if (settings.update[0]) {
    update();
  }
};
