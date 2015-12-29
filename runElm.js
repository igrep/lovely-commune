/*global Elm, webkitSpeechRecognition*/

var main = Elm.fullscreen(Elm.Main, { precureLoveLink: false });

// Ref: https://developers.google.com/web/updates/2013/01/Voice-Driven-Web-Apps-Introduction-to-the-Web-Speech-API
var recognition = new webkitSpeechRecognition();
recognition.continuous = true;
recognition.interimResults = false;
recognition.lang = "ja-JP";

var result = "";
recognition.onresult = function(event){
  for (var i = event.resultIndex; i < event.results.length; ++i){
    if (event.results[i].isFinal){
      result += event.results[i][0].transcript;
    }
  }

  if (/プリキュア\s*ラブリンク/.test(result)){
    main.ports.precureLoveLink.send(true);
    recognition.stop();
  }
};
recognition.start();
