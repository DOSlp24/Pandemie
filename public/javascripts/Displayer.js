var socket;
var windowWidth = window.innerWidth;
var windowHeight = window.innerHeight * 0.8;

var margin = {
    top: this.windowHeight / 20,
    right: this.windowWidth / 15,
    bottom: this.windowHeight / 10,
    left: this.windowWidth / 10
};
var padding = 0.2;
var populationData = [];

var svgHeight = windowHeight * 0.8;
var svgWidth = windowWidth * 0.7;

$(document).ready(function () {
    createSocket();
});

function createSocket() {
    socket = new WebSocket("ws://localhost:5000/webSockets");
    socket.setTimeout;
    socket.onopen = function () {
        console.log("I just opened a socket!");
        socket.send("Send Me Data");
        setInterval(function () {
            socket.send("I am still alive");
        }, 30000);
    };
    socket.onmessage = function (message) {
        console.log(message.data);
        var data = JSON.parse(message.data);
        if (data.round == 1) {
            populationData = [data.population];
        }
        if (data.round > populationData.length) {
            populationData[populationData.length] = data.population;
        }

        d3.select(".mainSvg").remove();

        var svg = d3.select("#vizArea").append("svg")
            .attr("class", "mainSvg")
            .attr("width", svgWidth)
            .attr("height", svgHeight)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        var xScale = d3.scaleLinear()
            .range([0, svgWidth - margin.right - margin.left])
            .domain([0, populationData.length]);

        var yScale = d3.scaleLinear()
            .range([svgHeight - margin.top - margin.bottom, 0])
            .domain([0, d3.max(populationData)]);

        svg.append("g")
            .attr("transform", "translate(0, " + (svgHeight - margin.top - margin.bottom) + ")")
            .call(d3.axisBottom(xScale));

        svg.append("g")
            .call(d3.axisLeft(yScale)
                .ticks(15));

        svg.append("path")
            .datum(populationData)
            .attr("d", d3.area()
                .x(function (entry, index) {
                    return xScale(index);
                })
                .y0(function (entry) {
                    return yScale(entry);
                })
                .y1(function () {
                    return yScale(0);
                }))
            .attr("stroke", "darkgreen")
            .attr("stroke-wdith", 2)
            .attr("fill", "forestgreen");

        svg.append("path")
            .attr("d", "M 0 " + yScale(d3.max(populationData) / 2) + " L " + xScale(populationData.length - 1) + " " + yScale(d3.max(populationData) / 2))
            .attr("stroke", "firebrick")
            .attr("stroke-width", 2)
            .attr("fill", "none");


        $(".stateTableEntry").remove();
        var patTable = $("#pathogen-table-body");
        data.pathogens.forEach(function (pat) {
            patTable.append("<tr class='stateTableEntry'>" +
                "<td>" + pat.pathogen.name + "</td>" +
                "<td>" + pat.pathogen.infectivity + "</td>" +
                "<td>" + pat.pathogen.mobility + "</td>" +
                "<td>" + pat.pathogen.duration + "</td>" +
                "<td>" + pat.pathogen.lethality + "</td>" +
                "<td>" + pat.totalInfected + "</td>" +
                "<td>" + pat.infectedCities + "</td>" +
                "</tr>")
        });


    };
    socket.onclose = function () {
        console.log("Socket Closed!");
    };
}