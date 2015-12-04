# CardsAgainstOcaml

![CAO](https://github.com/MattLi96/CardsAgainstOcaml/blob/master/res/logo.png)

Cards Against Humanity: **In OCaml.** Need I say anymore?

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

### Quickstart
If you just want to get the basic game running quickly without understanding why:
* `cs3110 compile server.ml`
* `cs3110 compile gui.ml`
* `cs3110 run server.ml` 
* (In a new terminal) `cs3110 run gui.ml` (Do this one time for each player)

When there are multiple people connected to the server, start the game. 

Also, make sure that you have all the dependencies needed such as (but are not limited to)

* CoHTTP 0.19.2
* CoHTTP Async 0.19.2
* YoJSON

### Server

In terminal run:

* `cs3110 compile server.ml`
* `cs3110 run server.ml`

This should start up the server. If properly started, you should see a message
telling you what port the server was started on (default to 8080). If 8080 is
in use already, then this will not work. The server MUST be started before 
anyone can connnect or join the game. 

### Client/GUI

In a new terminal (other than the one running server), run:

* `cs3110 compile gui.ml`
* `cs3110 run gui.ml`

This should launch the GUI. From the GUI, type in the ip address of the server
you want to connect to (this defaults to localhost). Press the "Click to connect to server!" button.
If you get the message "You are now connected",
then feel free to start the game when all players are ready.

### Bots

To start up a bot, from the same machine that is running the server, run:

* `cs3110 compile bot.ml`
* `cs3110 run bot.ml --[name of json trainer file]`

You can also run `botgui.ml` in order to start a bot:

* `cs3110 compile botgui.ml`
* `cs3110 run botgui.ml`

Enter the parameters necessary.

## How to Play
If you are a player, then simply click on the white card that you feel would best complete or answer the black card. If you are the judge, wait for players to submit their responses; then click the answer you feel best responds to the black card (or is the sickest). Scores are noted in the top right, and if you would like to see old winners, click the button and a log of old winners will be shown.

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
