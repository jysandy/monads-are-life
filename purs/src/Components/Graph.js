"use strict";

var Chart = require('node_modules/chart.js/dist/Chart');

exports.makeChart = function (element) {
  return function () {
    var chart = new Chart.Chart(element, {
      type: 'line',

      data: {
        datasets: [{
          label: 'Interest in monads over time',
          backgroundColor: 'rgb(255, 25, 25)',
          borderColor: 'rgb(255, 25, 25)',
          fill: false,
          data: [{
            x: 10,
            y: 10
          }, {
            x: 15,
            y: 15
          }, {
            x: 35,
            y: 25
          }, {
            x: 60,
            y: 55
          }]
        }]
      },

      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: {
          xAxes: [{
            display: true,
            type: 'linear',
            scaleLabel: {
              display: true,
              labelString: 'Time'
            },
            ticks: {
              display: false
            }
          }],
          yAxes: [{
            display: true,
            ticks: {
              display: false
            },
            scaleLabel: {
              display: true,
              labelString: 'Interest'
            }
          }]
        }
      }
    });
    return chart;
  };
};

exports.destroyChart = function (chart) {
  return function () {
    chart.destroy();
    return {};
  }
}
