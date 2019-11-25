import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root")
});

const sendNotionToken = () => {
  if (chrome && chrome.cookies) {
    chrome.cookies.get(
      { url: "https://www.notion.so/", name: "token_v2" },
      function(cookie) {
        if (cookie) {
          app.ports.notionToken.send(cookie.value);
        } else {
          app.ports.notionToken.send(null);
        }
      }
    );
  } else {
    app.ports.notionToken.send(null);
  }
};

app.ports.askToken.subscribe(sendNotionToken);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
