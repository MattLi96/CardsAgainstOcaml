# CardsAgainstOcaml
Cards Against Humanity: In OCaml. Need I say anymore?

## Members:
* [Austin Liu (arl96)](https://github.com/aliu139)
* [Matt Li (ml927)](https://github.com/MattLi96)
* [Jared Wong (jmw535)](https://github.com/techlover10)
* [Charley (Yuxin) Chen (yc769)](https://github.com/charleycyx)

## Introduction

[Cards Against Humanity](https://cardsagainsthumanity.com/) is a card
game (based on the popular family card game “Apples to Apples”) that pushes
the bounds of friendship and political correctness through the incorrect and
explicit juxtaposition of frankly unrelated objects in a very unusual
(and often obscene) context. In other words, Cards Against Humanity
is the game you don’t want your mother to know you play.

## How to Start

First navigate into the folder CardsAgainstOcaml.

### Server

In terminal run:

* cs3110 compile server.ml
* cs3110 run server.ml

This should start up the server. If properly started, you should see a message
telling you what port the server was started on (default to 8080). If 8080 is
in use already, then this will not work.

### Client/GUI

In a new terminal (other than the one running server), run:

* cs3110 compile gui.ml
* cs3110 run gui.ml

This should launch the GUI. From the GUI, type in the ip address of the server
you want to connect to. Press the "Click to connect to server!" button.
If you get the message "You are now connected",
then feel free to start the game when all players are ready.

### Bots

To start up a bot, from the same machine that is running the server, run:

* cs3110 compile bot.ml
* cs3110 run bot.ml --[name of json trainer file]

## Self-Modification
### Using Different Decks
If you would like to play with different decks, you can modify the white.json
and black.json files within the folder. This will modify the decks that you
play with. [WARNING] This will break the bot. The bot is only trained to play
with the base. If you add expansions or modify the deck in anyway, DO NOT START
A BOT.

### Changing Bot Playstyle
If you would like to change how the bot is trained, modify the file called
trainer.json. To do so, you can play around with the Python script we used to
originally train the bot, which is found
[here](https://github.com/aliu139/CAH-Trainer)
