var init = function() {
  var sock = new WebSocket("ws://0.0.0.0:8080");
  sock.onopen = function(event) {
    console.log('open');
    sock.send("hi up there?")
  };
  sock.onmessage = function(event) {
    //var obj = JSON.parse(event.data);
    var obj = event.data;
    console.log(obj);
    return;
  }
}
