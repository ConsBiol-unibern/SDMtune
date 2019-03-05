// Variables rendered by Whisker
var settings = {{{ settings }}};
var data = {{{ data }}};

var barData = {
  labels: settings.labels,
	datasets: [{
		label: "Variable",
		borderWidth: .7,
		borderColor: "rgb(245, 132, 16)",
		backgroundColor: "rgba(245, 132, 16, .7)",
		data: []
	}]
};

var barOptions = {
	responsive: true,
  maintainAspectRatio: false,
	title: {
		display: true,
		fontFamily: "sans-serif",
		padding: 15,
		text: "Variable Selection"
	},
	legend: {
		position: "bottom",
		labels: {
			fontFamily: "sans-serif",
		}
	},
	scales: {
		xAxes: [{
			ticks: {
				min: 0,
				suggestedMax: 50,
        callback: function(value, index, values) {
          return value + "%";
        }
			}
		}]
	},
	tooltips: {
		footerFontStyle: "normal",
		callbacks: {
		  title: function (tooltipItems, data) {
				return ""
			},
			label: function (tooltipItem, data) {
				var label = tooltipItem.yLabel || "";
				if (label) {
					label += ": ";
				}
				label += tooltipItem.xLabel + "%";
				return label;
			}
		}
	}
};

var lineData = {
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
			type: "linear",
      scaleLabel: {
				display: true,
				labelString: "iteration",
			},
			ticks: {
			  min: 0,
        suggestedMax: 2,
        stepSize: 1
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
  },
  annotation: {
    annotations: []
  }
};

init = function() {
  window.chart1.data.datasets[0].data = data.data;

  window.chart2.data.datasets[0].data = data.train;
  if (settings.metric[0] !== "AICc") {
    window.chart2.data.datasets[1].data = data.val;
  }

  // Add first vertical line//
  if (data.drawLine1[0]) {
    window.chart2.options.annotation.annotations[0] = {
      type: "line",
      mode: "vertical",
      scaleID: "x-axis-0",
      value: "1",
      borderColor: "rgb(204, 82, 79)",
      borderDash: [2],
      label: {
        backgroundColor: "rgb(204, 82, 79)",
        content: "Set reg to " + data.reg[0],
        fontStyle: "normal",
        enabled: true,
        position: "bottom",
        yAdjust: 6
      }
    }
  }

  // Add second vertical line//
  if (data.drawLine2[0]) {
    window.chart2.options.annotation.annotations[1] = {
      type: "line",
      mode: "vertical",
      scaleID: "x-axis-0",
      value: (data.train.length - 1).toString(),
      borderColor: "rgb(204, 82, 79)",
      borderDash: [2],
      label: {
        backgroundColor: "rgb(204, 82, 79)",
        content: "Set reg back to " + data.reg[1],
        fontStyle: "normal",
        enabled: true,
        position: "top",
        yAdjust: 6
      }
    };
    window.chart2.options.scales.xAxes[0].ticks.max = data.train.length;
  }

  window.chart1.update();
  window.chart2.update();
};

update = function() {
	var refresh = setInterval(loadData, 1000);
	function loadData() {
		$.ajax({
			method: "GET",
			url: "data.json",
			cache: false
		})
		.done(function (json) {
			data = JSON.parse(json);
			init();

			if (data.stop[0]) {
				clearInterval(refresh);
			}
		})
	};
};

window.onload = function() {
  // Set row height for bar chart according to the number of bars
  document.querySelector("#chart1").style.height = (settings.labels.length * 20).toString() + "px";
  // Set a wider content if page is displayed in the browser
  if (window.location.href.search("[?&]viewer_pane=") === -1) {
    document.querySelector(".content").style.maxWidth = "600px";
  }
  var ctx1 = document.getElementById("ctx1").getContext("2d");
  window.chart1 = new Chart(ctx1, {
    type: "horizontalBar",
    data: barData,
    options: barOptions,
  });
  var ctx2 = document.getElementById("ctx2").getContext("2d");
  window.chart2 = new Chart(ctx2, {
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
