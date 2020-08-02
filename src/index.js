import './tachyons.min.css';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import down_arrow from '../images/down_arrow.svg';
import dry from '../images/dry.svg';
import inside from '../images/inside.svg';
import outside from '../images/outside.svg';
import wet from '../images/wet.svg';
import left_right_arrow from '../images/left_right_arrow.svg';
import question_mark from '../images/question_mark.svg';
import german from '../images/german.svg';
import english from '../images/english.svg';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    down_arrow
    , dry
    , inside
    , outside
    , wet
    , left_right_arrow
    , question_mark
    , german
    , english
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
