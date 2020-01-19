String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    return target.split(search).join(replacement);
};

var socket;
var listenerSocketNumber;

var BASEPORT = 50123;

var windowWidth = window.innerWidth;
var windowHeight = window.innerHeight * 0.8;

var margin = {
    top: this.windowHeight / 20,
    right: this.windowWidth / 30,
    bottom: this.windowHeight / 10,
    left: this.windowWidth / 30
};
var padding = 0.2;
var storedData = [];
var infectionData = [];
var messageData;

var selectedPathogen = "";
var selectedRound = 0;

var globeAllreadyBuilt = false;
var showFlyers = false;

var svgHeight = windowHeight * 0.8;
var svgWidth = windowWidth * 0.4;

var cPathogens = ["NotInfected"];
var colorPalette = ["limegreen", "coral", "turquoise", "mediumorchid", "indianred", "navy", "goldenrod", "olive", "skyblue", "violet"];

$(document).ready(function () {
    awakenLinks();
    createSocket();
});

function awakenLinks() {
    $(".scrollLinkToGlobe").click(function () {
        $([document.documentElement, document.body]).animate({
            scrollTop: $("#globeVizAnchor").offset().top
        }, 500);
    });
    $(".scrollLinkToPopulation").click(function () {
        $([document.documentElement, document.body]).animate({
            scrollTop: $("#populationVizAnchor").offset().top
        }, 500);
    });
}

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
        if (message.data.toString().split(" ")[0] === "Socket") {
            listenerSocketNumber = message.data.toString().split(" ")[1];
            console.log("I am " + listenerSocketNumber);
        } else {

            messageData = JSON.parse(message.data);
            console.log(messageData);

            if (messageData.outcome !== "pending") {
                $("#StartButton").show();
            }

            //TODO auslagern
            $(".generatedOption").remove();
            messageData.pathogens.map(function (pat) {
                return pat.pathogen.name
            }).forEach(function (pat) {
                $(".pathogenDropdown").append("<option class='generatedOption'>" + pat + "</option>")
            });
            $(".pathogenDropdown").change(function () {
                selectedPathogen = $(this).children("option:selected").val();
                buildGlobeCityViz();
            });
            $("#showFlyers").click(function () {
                showFlyers = $("#showFlyers").is(":checked");
                buildGlobeCityViz();
            });

            d3.selectAll(".roundSlider")
                .attr("max", messageData.round);

            $(".roundSlider").change(function () {
                selectedRound = this.value;

                if (selectedPathogen !== "") {
                    d3.selectAll(".cityPoint")
                        .attr("fill", "black")
                        .attr("stroke-width", 1);
                }
                storedData[selectedRound].pathogens.filter(function (pat) {
                    return pat.pathogen.name === selectedPathogen;
                })[0].infectedCities.forEach(function (city) {
                    d3.select("#" + city.name.replaceAll(",", "").replaceAll(".", "").replaceAll("(", "").replaceAll(")", "").trim() + "Point")
                        .attr("fill", "red")
                        .attr("stroke-width", 2);
                });
                //buildGlobeCityViz();
            });


            if (messageData.round === 1) {
                cPathogens = ["NotInfected"];
                storedData = [messageData];
                infectionData = [{round: messageData.round - 1}];
                messageData.pathogens.forEach(function (pat) {
                    infectionData[infectionData.length - 1][pat.pathogen.name] = pat.totalInfected;
                });
                infectionData[infectionData.length - 1]["NotInfected"] = messageData.notInfected;
                fillupInfectionData();
            }
            if (messageData.round > storedData.length) {
                storedData.push(messageData);
                infectionData.push({round: messageData.round - 1});
                messageData.pathogens.forEach(function (pat) {
                    infectionData[infectionData.length - 1][pat.pathogen.name] = pat.totalInfected;
                });
                infectionData[infectionData.length - 1]["NotInfected"] = messageData.notInfected;
                fillupInfectionData();
            }

            buildPathogenTable();
            buildPopulationGraph();
            buildStackedInfectionViz();
            buildPathogenLineViz();
            if (messageData.round === 1 && messageData.points === 40) {
                buildGlobeCityViz();
            }
        }
    };

    socket.onclose = function () {
        console.log("Socket Closed!");
    };
    $("#StartButton").click(function () {
        $("#StartButton").hide();
        socket.send("New Run please")
    });
}

function fillupInfectionData() {
    messageData.pathogens.map(function (p) {
        return p.pathogen.name
    }).forEach(function (pathogenName) {
        infectionData.forEach(function (infEntry, index) {
            if (infEntry[pathogenName] === undefined) {
                infectionData[index][pathogenName] = 0;
            }
        });
    });
}

function buildPathogenTable() {
    $(".stateTableEntry").remove();
    var patTable = $("#pathogen-table-body");
    messageData.pathogens.forEach(function (pat) {
        patTable.append("<tr class='stateTableEntry'>" +
            "<td><div style='background-color: " + getColorForPathogen(pat.pathogen.name) + ";width: 30px;height: 30px'></div></td>" +
            "<td>" + pat.pathogen.name + "</td>" +
            "<td>" + pat.pathogen.infectivity + "</td>" +
            "<td>" + pat.pathogen.mobility + "</td>" +
            "<td>" + pat.pathogen.duration + "</td>" +
            "<td>" + pat.pathogen.lethality + "</td>" +
            "<td>" + pat.totalInfected + "</td>" +
            "<td>" + pat.numberInfectedCities + "</td>" +
            "</tr>")
    });
}

function buildPopulationGraph() {
    var populationData = storedData.map(function (d) {
        return d.population;
    });

    var totalWidth = $("#populationVizArea").width();
    var totalHeight = $("#populationVizArea").height();
    var width = totalWidth - margin.left - margin.right;
    var height = totalHeight - margin.top - margin.bottom;

    d3.select(".populationSvg").remove();

    var svg = d3.select("#populationVizArea").append("svg")
        .attr("class", "populationSvg")
        .attr("width", totalWidth)
        .attr("height", totalHeight)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var xScale = d3.scaleLinear()
        .range([0, width])
        .domain([0, populationData.length]);

    var yScale = d3.scaleLinear()
        .range([height, 0])
        .domain([0, d3.max(populationData)]);

    svg.append("g")
        .attr("transform", "translate(0, " + height + ")")
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
        .attr("stroke", "peru")
        .attr("stroke-wdith", 2)
        .attr("fill", "navajowhite");

    svg.append("path")
        .attr("d", "M 0 " + yScale(d3.max(populationData) / 2) + " L " + xScale(populationData.length - 1) + " " + yScale(d3.max(populationData) / 2))
        .attr("stroke", "firebrick")
        .attr("stroke-width", 2)
        .attr("fill", "none");
}

function buildPathogenLineViz() {
    var totalWidth = $("#pathogenLineVizArea").width();
    var totalHeight = $("#pathogenLineVizArea").height();
    var width = totalWidth - margin.right - margin.left;
    var height = totalHeight - margin.top - margin.bottom;

    d3.select("#pathogenLineVizSvg")
        .remove();

    var keys = messageData.pathogens.map(function (entry) {
        return entry.pathogen.name;
    });

    var color = d3.scaleOrdinal()
        .domain(keys)
        .range(['#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf']);

    var svg = d3.select("#pathogenLineVizArea")
        .append("svg")
        .attr("id", "pathogenLineVizSvg")
        .attr("width", totalWidth)
        .attr("height", totalHeight)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var xScale = d3.scaleLinear()
        .range([0, width])
        .domain([0, storedData.length]);

    var yScale = d3.scaleLinear()
        .range([height, 0])
        .domain([0, storedData[0].population]);

    svg.append("g")
        .attr("transform", "translate(0, " + height + ")")
        .call(d3.axisBottom(xScale));

    svg.append("g")
        .call(d3.axisLeft(yScale)
            .ticks(15));

    var lineGen = d3.line()
        .x(function (_, i) {
            return xScale(i)
        })
        .y(function (data) {
            return yScale(data)
        });

    var infData = {};

    for (var i in keys) {
        var pathogen = keys[i];
        var d = storedData.map(function (dataEntry) {
            var filteredForPat = dataEntry.pathogens.filter(function (pat) {
                return pat.pathogen.name === pathogen;
            });
            if (filteredForPat.length > 0) {
                return filteredForPat[0].totalInfected;
            } else {
                return 0;
            }
        });
        infData[pathogen] = d;
    }

    svg.selectAll(".pathogenLine")
        .data(keys)
        .enter()
        .append("path")
        .attr("d", function (key) {
            return lineGen(infData[key])
        })
        .attr("stroke", function (key) {
            return getColorForPathogen(key)
        })
        .attr("class", function (key) {
            return "pathogenLine pathogenViz " + key.replace(/\s/g, '') + "Viz";
        })
        .attr("stroke-width", 4)
        .attr("fill", "none")
        .on("click", triggerPathogenVizElements);
}

function buildStackedInfectionViz() {
    var totalWidth = $("#stackedInfectionVizArea").width();
    var totalHeight = $("#stackedInfectionVizArea").height();
    var width = totalWidth - margin.right - margin.left;
    var height = totalHeight - margin.top - margin.bottom;


    var xScale = d3.scaleLinear()
        .range([0, width])
        .domain([0, storedData.length]);

    var yScale = d3.scaleLinear()
        .range([height, 0])
        .domain([0, storedData[0].population]);

    var keys = messageData.pathogens.map(function (entry) {
        return entry.pathogen.name;
    });
    keys.push("NotInfected");

    var stack = d3.stack()
        .keys(keys)
        (infectionData);

    var stackedInfData = [];

    stack.forEach(function (layer, index) {
        var currentStack = [];
        var key = layer.key;
        layer.forEach(function (d, i) {
            currentStack.push({
                values: d,
                name: key,
                round: storedData[i].round
            });
        });
        stackedInfData.push(currentStack);
    });

    var area = d3.area()
        .x(function (entry) {
            return xScale(entry.round - 1);
        })
        .y0(function (entry) {
            return yScale(entry.values[0]);
        })
        .y1(function (entry) {
            return yScale(entry.values[1]);
        });

    d3.selectAll(".infectionVizSvg")
        .remove();

    var color = d3.scaleOrdinal() // Unusable cause colors change at runetime
        .domain(keys)
        .range(['#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf']);


    var svg = d3.select("#stackedInfectionVizArea")
        .append("svg")
        .attr("class", "infectionVizSvg")
        .attr("width", totalWidth)
        .attr("height", totalHeight)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    svg.append("g")
        .attr("transform", "translate(0, " + height + ")")
        .call(d3.axisBottom(xScale));

    svg.append("g")
        .call(d3.axisLeft(yScale)
            .ticks(15));

    var stackedInfectionArea = svg.selectAll(".stackedInfectionArea")
        .data(stackedInfData)
        .enter()
        .append("g")
        .attr("class", function (d) {
            return "stackedInfectionArea pathogenViz " + d[0].name.replace(/\s/g, '') + "Viz";
        })
        .on("click", function (d) {
            triggerPathogenVizElements(d[0].name)
        });
    stackedInfectionArea.append("path")
        .attr("d", function (data) {
            return area(data);
        })
        .attr("fill", function (d) {
            return getColorForPathogen(d[0].name);
        })
        .attr("stroke", "black");
}

function buildGlobeCityViz() {
    var width = windowWidth,
        height = windowHeight;

    var proj = d3.geoOrthographic()
        .translate([width / 2, height / 2])
        .clipAngle(90)
        .scale(220);

    var sky = d3.geoOrthographic()
        .translate([width / 2, height / 2])
        .clipAngle(90)
        .scale(300);

    var path = d3.geoPath().projection(proj).pointRadius(2);

    var swoosh = d3.line()
        .x(function (d) {
            return d[0]
        })
        .y(function (d) {
            return d[1]
        })
        .curve(d3.curveCardinal.tension(.0));

    var links = [],
        arcLines = [],
        points = [];

    if (globeAllreadyBuilt) {
        refresh();
    } else {
        d3.select("#globeVizArea")
            .on("mousemove", mousemove)
            .on("mouseup", mouseup);

        d3.select("#globeVizSvg")
            .remove();

        var svg = d3.select("#globeVizArea").append("svg")
            .attr("id", "globeVizSvg")
            .attr("width", width)
            .attr("height", height)
            .on("mousedown", mousedown);

        d3.json("assets/json/world-110m.json").then(function (world) {
            console.log(world);

            var ocean_fill = svg.append("defs").append("radialGradient")
                .attr("id", "ocean_fill")
                .attr("cx", "75%")
                .attr("cy", "25%");
            ocean_fill.append("stop").attr("offset", "5%").attr("stop-color", "blue");
            ocean_fill.append("stop").attr("offset", "100%").attr("stop-color", "darkblue");

            var globe_highlight = svg.append("defs").append("radialGradient")
                .attr("id", "globe_highlight")
                .attr("cx", "75%")
                .attr("cy", "25%");
            globe_highlight.append("stop")
                .attr("offset", "5%").attr("stop-color", "#ffd")
                .attr("stop-opacity", "0.6");
            globe_highlight.append("stop")
                .attr("offset", "100%").attr("stop-color", "#ba9")
                .attr("stop-opacity", "0.2");

            var globe_shading = svg.append("defs").append("radialGradient")
                .attr("id", "globe_shading")
                .attr("cx", "55%")
                .attr("cy", "45%");
            globe_shading.append("stop")
                .attr("offset", "30%").attr("stop-color", "#fff")
                .attr("stop-opacity", "0");
            globe_shading.append("stop")
                .attr("offset", "100%").attr("stop-color", "#505962")
                .attr("stop-opacity", "0.3");

            svg.append("circle")
                .attr("cx", width / 2).attr("cy", height / 2)
                .attr("r", proj.scale())
                .attr("class", "noclicks")
                .style("fill", "url(#ocean_fill)");

            /** @namespace world.objects.land */
            svg.append("path")
                .datum(topojson.feature(world, world.objects.land))
                .attr("class", "land noclicks")
                .attr("fill", "brown")
                .attr("d", path);

            svg.append("circle")
                .attr("cx", width / 2).attr("cy", height / 2)
                .attr("r", proj.scale())
                .attr("class", "noclicks")
                .style("fill", "url(#globe_highlight)");

            svg.append("circle")
                .attr("cx", width / 2).attr("cy", height / 2)
                .attr("r", proj.scale())
                .attr("class", "noclicks")
                .style("fill", "url(#globe_shading)");


            /** @namespace messageData.cities */
            messageData.cities.forEach(function (a) {
                points.push({"type": "Point", "coordinates": [a.longitude, a.latitude], "name": a.name});
                /** @namespace a.connections */
                a.connections.forEach(function (cName) {
                    var b = messageData.cities.find(function (city) {
                        return city.name === cName;
                    });
                    links.push({
                        sourceCity: a.name,
                        targetCity: b.name,
                        source: [a.longitude, a.latitude],
                        target: [b.longitude, b.latitude]
                    });
                });
            });

            svg.append("g").attr("class", "cityPoints")
                .selectAll("text").data(points)
                .enter().append("path")
                .attr("class", "cityPoint")
                .attr("id", function (city) {
                    return city.name.replaceAll(",", "").replaceAll(".", "").replaceAll("(", "").replaceAll(")", "").trim() + "Point";
                })
                .attr("opacity", 0.6)
                .attr("d", path)
                .on("mouseover", function (city, i) {
                    var mouse = d3.mouse(this);

                    svg.append("text")
                        .attr("class", "cityLable")
                        .attr("x", mouse[0] - 15)
                        .attr("y", mouse[1] - 30)
                        .text(city.name);
                })
                .on("mouseout", function () {
                    d3.selectAll(".cityLable")
                        .remove();
                });

            links.forEach(function (e) {
                var feature = {
                    "type": "Feature",
                    "geometry": {"type": "LineString", "coordinates": [e.source, e.target]}
                };
                arcLines.push(feature)
            });

            svg.append("g").attr("class", "flyers")
                .selectAll("path").data(links)
                .enter().append("path")
                .attr("class", function (link) {
                    return "flyer noclicks flyer" + link.sourceCity.trim() + " flyer" + link.targetCity.trim();
                })
                .attr("fill", "none")
                .attr("stroke", "darkred")
                .attr("opacity", 0)
                .attr("stroke-linejoin", "round")
                .attr("stroke-width", 1)
                .attr("d", function (d) {
                    return swoosh(flying_arc(d))
                });
            globeAllreadyBuilt = true;
            refresh();
        });
        // modified from http://bl.ocks.org/1392560
        var m0, o0;

        function mousedown() {
            m0 = [d3.event.pageX, d3.event.pageY];
            o0 = proj.rotate();
            d3.event.preventDefault();
        }

        function mousemove() {
            if (m0) {
                var m1 = [d3.event.pageX, d3.event.pageY]
                    , o1 = [o0[0] + (m1[0] - m0[0]) / 6, o0[1] + (m0[1] - m1[1]) / 6];
                o1[1] = o1[1] > 30 ? 30 :
                    o1[1] < -30 ? -30 :
                        o1[1];
                proj.rotate(o1);
                sky.rotate(o1);
                refresh();
            }
        }

        function mouseup() {
            if (m0) {
                mousemove();
                m0 = null;
            }
        }
    }

    function flying_arc(pts) {
        var source = pts.source,
            target = pts.target;

        var mid = location_along_arc(source, target, .5);
        return [proj(source),
            sky(mid),
            proj(target)];
    }


    function refresh() {
        d3.selectAll(".land").attr("d", path);
        d3.selectAll(".cityPoint").attr("d", path);

        d3.selectAll(".arc").attr("d", path)
            .attr("opacity", function (d) {
                return fade_at_edge(d)
            });

        d3.selectAll(".flyer")
            .attr("opacity", 0);
        d3.selectAll(".cityPoint")
            .attr("fill", "black");

        var selectedPathogenData = storedData[selectedRound].pathogens.filter(function (pat) {
            return pat.pathogen.name === selectedPathogen;
        })[0];
        if (selectedPathogenData !== undefined) {
            selectedPathogenData.infectedCities.forEach(function (city) {
                d3.select("#" + city.name.replaceAll(",", "").replaceAll(".", "").replaceAll("(", "").replaceAll(")", "").trim() + "Point")
                    .attr("fill", "red");

                if (showFlyers) {
                    d3.selectAll(".flyer" + city.name.replaceAll(",", "").replaceAll(".", "").replaceAll("(", "").replaceAll(")", "").trim())
                        .attr("d", function (d) {
                            return swoosh(flying_arc(d))
                        })
                        .attr("opacity", function (d) {
                            return fade_at_edge(d)
                        });
                }
            });
        }

    }

    function fade_at_edge(d) {
        var centerPos = proj.invert([width / 2, height / 2]),
            start, end;
        if (d.source) {
            start = d.source;
            end = d.target;
        } else {
            start = d.geometry.coordinates[0];
            end = d.geometry.coordinates[1];
        }

        var start_dist = 1.57 - d3.geoDistance(start, centerPos),
            end_dist = 1.57 - d3.geoDistance(end, centerPos);

        var fade = d3.scaleLinear().domain([-.1, 0]).range([0, .1]);
        var dist = start_dist < end_dist ? start_dist : end_dist;

        return fade(dist)
    }

    function location_along_arc(start, end, loc) {
        var interpolator = d3.geoInterpolate(start, end);
        return interpolator(loc)
    }

}

function getColorForPathogen(key) {
    if (!cPathogens.includes(key)) {
        cPathogens[cPathogens.length] = key;
    }
    return colorPalette[cPathogens.indexOf(key)];
}

function triggerPathogenVizElements(key) {
    if (selectedPathogen === key) {
        selectedPathogen = "";
        d3.selectAll(".pathogenViz")
            .attr("opacity", 1);
    } else {
        selectedPathogen = key;

        d3.selectAll(".pathogenViz")
            .attr("opacity", .2);
        d3.selectAll("." + key.replace(/\s/g, '') + "Viz")
            .attr("opacity", 1);
    }
}