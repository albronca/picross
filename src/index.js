import { Elm } from './Main.elm';

var mountNode = document.getElementById('elm');

Elm.Main.init({
  node: mountNode,
  flags: {
    width: window.innerWidth,
    height: window.innerHeight
  }
});
