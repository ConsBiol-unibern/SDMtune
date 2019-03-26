// Variables rendered by Whisker
var settings = {{{ settings }}};
var data = {{{ data }}};

var lineData = {
	datasets: [{
		label: "Training",
		pointRadius: 3,
		pointHoverRadius: 5,
		borderWidth: .7,
		borderColor: "rgb(245, 132, 16)",
		backgroundColor: "rgba(245, 132, 16, .7)",
		fill: false,
    lineTension: 0,
    borderDash: [5],
		showLine: settings.show_line[0],
		data: [],
	}]
};
// Add Validation dataset if metric in not AICc
if (settings.metric[0] !== "AICc") {
	lineData.datasets.push({
		label: "Validation",
		pointRadius: 3,
		pointHoverRadius: 5,
		borderWidth: .7,
		borderColor: "rgb(75, 192, 192)",
		backgroundColor: "rgba(75, 192, 192, .7)",
		fill: false,
    lineTension: 0,
    borderDash: [5],
		showLine: settings.show_line[0],
		data: [],
	})
}

var lineOptions = {
	responsive: true,
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
				labelString: "model",
			},
			ticks: {
				max: settings.max[0],
				min: 1,
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
				var label = data.datasets[tooltipItem.datasetIndex].label || "";
				if (label) {
					label += ": ";
				}
				label += tooltipItem.yLabel;
				return label;
			},
			title: function(tooltipItems, data) {
				return ""
			},
			footer: function(tooltipItems, data) {
				var footer = window.data.gridFooter.length > 1 ? window.data.gridFooter[tooltipItems[0].index] : "";
				if (settings.metric[0] !== "AICc") {
					var footer = "Diff: " + (tooltipItems[0].yLabel - tooltipItems[1].yLabel).toFixed(4) + "\n" + footer;
				}
				return footer;
			}
		}
	}
};

init = function() {
  window.chartLine.data.datasets[0].data = data.train;
		if (settings.metric[0] !== "AICc") {
			window.chartLine.data.datasets[1].data = data.val;
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
		})
		.done(function (json) {
			data = JSON.parse(json)
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
		document.querySelector(".content").style.maxWidth = "600px";
	}
	var ctx = document.getElementById("ctx1").getContext("2d");
	window.chartLine = new Chart(ctx, {
		type: "scatter",
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
