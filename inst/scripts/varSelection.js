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
		text: settings.title[0]
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
        suggestedMax: 1,
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
      }
    }
  }
};

init = function() {
  window.chart1.data.datasets[0].data = data.data;

  window.chart2.data.datasets[0].data = data.train;
  if (settings.metric[0] !== "AICc") {
    window.chart2.data.datasets[1].data = data.val;
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
  var l = settings.labels.length < 20 ? 25 : 15;
  document.querySelector("#chart1").style.height = (settings.labels.length * l).toString() + "px";
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
