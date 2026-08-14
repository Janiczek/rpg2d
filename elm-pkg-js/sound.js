/* elm-pkg-js
port playSound : String -> Cmd msg
*/

const sounds = {
  bounce: new Audio("assets/sounds/pretzl-rubberballbouncing-251948.mp3"),
};

exports.init = async function (app) {
  app.ports.playSound.subscribe(function (sound) {
    if (sound in sounds) {
      sounds[sound].play();
    }
  });
};
