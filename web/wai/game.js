
function Client(socket) {
    socket.onopen = function () {
        console.log("opened web socket")
    }
    socket.onclose = function () {
        console.log("closed web socket")
    }
    socket.onerror = function (event) {
        console.log(event)
    }
    socket.onmessage = function (event) {
      console.log(event)
      console.log(event.data)
    }
}

$(function(){
    socket = new WebSocket("ws://localhost:8000/")
    client = new Client(socket);
})

