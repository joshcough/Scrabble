var playerName;

function Client(socket) {
    socket.onopen = function () {
      // TODO: need a real way for players to enter their name
      socket.send("playerNameHere!");
    }
    socket.onclose = function () {
      console.log("closed web socket")
    }
    socket.onerror = function (event) {
      console.log(event)
    }
    socket.onmessage = function (event) {
      // TODO: here is where magic must happen.
      // we must have gotten a new game, or an error message
      // check, and if its a new game, update the ui, and
      // if its this players turn, let them enter a word.
      // (check if it's this players turn by their name...
      // and id...which we still don't have yet. for not thats okay.
      console.log(event.data)
    }
}

$(function(){
    socket = new WebSocket("ws://localhost:8000/")
    client = new Client(socket);
})

