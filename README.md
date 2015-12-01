# CardsAgainstOcaml
Cards Against Humanity: In OCaml

## Members:
[Austin Liu (arl96)](https://github.com/aliu139)
[Matt Li (ml927)](https://github.com/MattLi96)
[Jared Wong (jmw535)](https://github.com/techlover10)
[Charley (Yuxin) Chen (yc769)](https://github.com/charleycyx)

## Introduction

[Cards Against Humanity](https://cardsagainsthumanity.com/) is a card game (based on the popular family card game “Apples to Apples”) that pushes the bounds of friendship and political correctness through the incorrect and explicit juxtaposition of frankly unrelated objects in a very unusual (and often obscene) context. In other words, Cards Against Humanity is the game you don’t want your mother to know you play.

## How to Start

First navigate into the folder CardsAgainstOcaml.

### Server

In terminal run:

* cs3110 compile server.ml
* cs3110 run server.ml

This should start up the server.

### Client/GUI

In a new terminal (other than the one running server), run:

* cs3110 compile gui.ml
* cs3110 run gui.ml

This should launch the GUI. From the GUI, type in the ip address of the server
you want to connect to. Press the connect button. If you get the message
"you have connected", then feel free to start the game when all players are ready.

### Bots

To start up a bot, from the same machine that is running the server, run:

* cs3110 compile bot.ml
* cs3110 run bot.ml --[name of json trainer file]

## Self-Modification
### Using Different Decks

### Changing Bot Playstyle
