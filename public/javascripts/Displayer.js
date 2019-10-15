$(document).ready(function () {
    createSocket();
});

var socket;

function createSocket() {
    socket = new WebSocket("ws://localhost:5000/webSockets");
    socket.setTimeout;
    socket.onopen = function () {
        console.log("I just opened a socket!");
        socket.send("Send Me Data");
    };
    socket.onmessage = function (message) {
        console.log(message.data);
        $(".stateText").remove();
        var textArea = $("#textArea");
        message.data.split("\n").forEach(function (line) {
            textArea.append("<h3 class='stateText'>" + line + "</h3>");
        })
    };
    socket.onclose = function () {
        console.log("Socket Closed!");
    };
}