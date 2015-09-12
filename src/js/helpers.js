"use strict";

function getDiff(idx, when, data, ident) {
    for (; idx >= 0; --idx) {
        if (moment(data[idx][0]).diff(when) < 0) {
            var last = data[data.length - 1];
            var diff = last[2] - data[idx][2];
            var elem = $(ident).text(diff.toFixed(1));
            if (diff > 0) {
                elem.addClass("text-danger");
            } else if (diff < 0) {
                elem.addClass("text-success");
            }
            return idx;
        }
    }
}


function processData(data) {
    var alpha = 0.1;
    var d = [];
    var avg = 0;
    for (var i = 0, len = data.length; i < len; i++) {
        if (i == 0) {
            avg = data[i]['weight'];
        } else {
            avg = alpha * data[i]['weight'] + (1 - alpha) * avg;
        }
        d.push([new Date(data[i]['date']), data[i]['weight'], avg])
    };
    return d;
}

function populateDashboard(data) {
    var d = processData(data);

    var height = parseInt($("#height").text());
    var last = d[d.length - 1];
    var weight = last[2]
    $("#weight").text(weight.toFixed(1));

    var bmi = weight / ((height / 100) * (height / 100));
    $("#bmi").text(bmi.toFixed(1));
    if (bmi > 30) {
        $("#bmi").addClass("text-danger");
    } else if (bmi > 25) {
        $("#bmi").addClass("text-warning");
    }

    var i = d.length - 1;
    i = getDiff(i, moment(last[0]).subtract(7, 'days'), d, "#trend-w");
    i = getDiff(i, moment(last[0]).subtract(1, 'months'), d, "#trend-m");
    getDiff(i, moment(last[0]).subtract(3, 'months'), d, "#trend-q");
}

function formatDate(val, opts, graph) {
    return moment(val).format('YYYY-MM-DD');
}

function axisFormat(x, gran) {
    moment.locale('cs', {
        monthsShort: 'led_ún_bře_dub_kvě_čer_čec_srp_zář_říj_lis_pro'.split('_')
    });
    switch (gran) {
        case Dygraph.MONTHLY:
            return moment(x).format('MMM[<br>]YYYY');
        case Dygraph.YEARLY:
            return moment(x).format('YYYY');
    }
    return moment(x).format('D[.&nbsp;]MMM[<br>]YYYY');
}

function createGraph(ident, data) {
    var d = processData(data);
    return new Dygraph(
        document.getElementById(ident),
        d, {
            showRangeSelector: true,
            xRangePad: 6,
            pointSize: 2,
            panEdgeFraction: 0.01,
            labels: ['Date', 'Weight', 'Avg'],
            labelsSeparateLines: true,
            legend: 'always',
            labelsDivWidth: 150,
            labelsDivStyles: {
                padding: '1em',
                'background-color': 'rgba(230, 230, 230, 0.65)',
            },
            gridLineColor: '#cccccc',
            colors: ['#1F77B4', '#FF7F0E'],
            series: {
                Weight: {
                    drawPoints: true,
                    strokeWidth: 0,
                },
                Avg: {
                    strokeWidth: 1.5,
                }
            },
            axes: {
                x: {
                    valueFormatter: formatDate,
                    axisLabelFormatter: axisFormat
                }
            }
        }
    );
}

function loadData(callback) {
    $.get("/data.json").success(function(data) {
        callback(data);
    });
}
