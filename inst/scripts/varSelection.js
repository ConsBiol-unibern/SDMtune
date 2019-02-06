// Variables rendered by Whisker
var settings = {{{ settings }}};
var data = {{{ data }}};

var barData = {
  labels: settings.labels,
	datasets: [{
		label: "Variable",
		borderWidth: .7,
		borderColor: "rgb(75, 192, 192)",
		backgroundColor: "rgba(75, 192, 192, .7)",
		data: []
	}]
};

var barOptions = {
	responsive: true,
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
			}
		}]
	},
	tooltips: {
		footerFontStyle: "normal",
		callbacks: {
			label: function (tooltipItem, data) {
				var label = tooltipItem.yLabel || "";
				if (label) {
					label += ": ";
				}
				label += tooltipItem.y + "%";
				return label;
			},
			title: function (tooltipItems, data) {
				return ""
			}
		}
	}
};

init = function() {
  window.chart1.data.datasets[0].data = data.data;
  window.chart1.update();
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
    document.querySelector(".content").style.maxWidth="600px";
  }
  var ctx1 = document.getElementById("ctx1").getContext("2d");
  window.chart1 = new Chart(ctx1, {
    type: "horizontalBar",
    data: barData,
    options: barOptions,
  });
  // Init charts
  init();
  // Update in case of real time chart
  if (settings.update[0]) {
    update();
  }
};
