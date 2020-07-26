// import './reset.css';
import './tachyons.min.css';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import down_arrow from '../images/down_arrow.svg';
import dry from '../images/dry.svg';
import inside from '../images/inside.svg';
import opening_window from '../images/opening_window.svg';
import outside from '../images/outside.svg';
import wet from '../images/wet.svg';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    down_arrow
    , dry
    , inside
    , opening_window
    , outside
    , wet
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
