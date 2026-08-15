/* elm-pkg-js
port playSound : String -> Cmd msg
*/

const sounds = {
  bounce: new Audio("assets/sounds/pretzl-rubberballbouncing-251948.mp3"),
};

exports.init = async function (app) {
  app.ports.playSound.subscribe(async function (soundName) {
    if (soundName in sounds) {
      const sound = sounds[soundName];
      if (sound.paused) {
        sound.play();
      } else {
        // already playing, just seek it to beginning
        sound.currentTime = 0;
      }
    }
  });
};
