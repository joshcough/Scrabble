# Scrabble

[![Build Status](https://travis-ci.org/joshcough/Scrabble.png?branch=master)](https://travis-ci.org/joshcough/Scrabble)
[![Coverage Status](https://coveralls.io/repos/github/joshcough/Scrabble/badge.svg?branch=master)](https://coveralls.io/github/joshcough/Scrabble?branch=master)

This is a Haskell implementation of Scrabble.

Todo: GUI, Network game server, AI engine.

### Getting started

First things first, install stack: http://docs.haskellstack.org/en/stable/README.html#how-to-install

And then jump up on the repl:

    $ stack ghci

### Place some words on the board:

    *Scrabble> let hell  = ("HELL", Vertical,   (7,7))
               let has   = ("HAS",  Horizontal, (5,6))
               let as    = ("AS",   Horizontal, (5,7))
               let (b,s) = quickPut [hell, has, as]
    *Scrabble> printBoard True b
    ______________________________________________
    |3W|  |  |2L|  |  |  |3W|  |  |  |2L|  |  |3W|
    |  |2W|  |  |  |3L|  |  |  |3L|  |  |  |2W|  |
    |  |  |2W|  |  |  |2L|  |2L|  |  |  |2W|  |  |
    |2L|  |  |2W|  |  |  |2L|  |  |  |2W|  |  |2L|
    |  |  |  |  |2W|  |  |  |  |  |2W|  |  |  |  |
    |  |3L|  |  |  |3L|  |  |  |3L|  |  |  |3L|  |
    |  |  |2L|  |  | H| A| S|2L|  |  |  |2L|  |  |
    |3W|  |  |2L|  | A| S| H|  |  |  |2L|  |  |3W|
    |  |  |2L|  |  |  |2L| E|2L|  |  |  |2L|  |  |
    |  |3L|  |  |  |3L|  | L|  |3L|  |  |  |3L|  |
    |  |  |  |  |2W|  |  | L|  |  |2W|  |  |  |  |
    |2L|  |  |2W|  |  |  |2L|  |  |  |2W|  |  |2L|
    |  |  |2W|  |  |  |2L|  |2L|  |  |  |2W|  |  |
    |  |2W|  |  |  |3L|  |  |  |3L|  |  |  |2W|  |
    |3W|  |  |2L|  |  |  |3W|  |  |  |2L|  |  |3W|
    ----------------------------------------------
    *Scrabble> s
    [14,14,13]

### Play the game

Start a new game:

    *Scrabble> start [human "Josh", human "Stephen", human "Andrew"]

Place the word 'rest' vertically at position (7,7). You can only place things in your rack.

    (place REST V (7 7))

### Test the search engine

    *Scrabble> cheat (matchAll [containsAny "z", containsLetterAtPos 'w' 4])
    ["buzzwig","buzzwigs","buzzword","buzzwords","unbowdlerized","zugzwang","zugzwanged","zugzwanging","zugzwangs"]

    *Scrabble> cheat (matchAll [containsAny "x", containsLetterAtPos 'b' 4, endsWith 'g'])
    ["exhibiting","exorbitating","extubating","kickboxing","soapboxing"]

### What code is here

```
$ tree .
.
├── src
│   ├── Scrabble
│   │   ├── Bag.hs
│   │   ├── Board.hs
│   │   ├── Commands
│   │   │   ├── AST.hs
│   │   │   ├── Interpreter.hs
│   │   │   └── SExpr.hs
│   │   ├── Dictionary.hs
│   │   ├── Game.hs
│   │   ├── ListBoard.hs
│   │   ├── Matrix.hs
│   │   ├── Play.hs
│   │   ├── Position.hs
│   │   ├── ReplHelpers.hs
│   │   └── Search.hs
│   └── Scrabble.hs
├── stack-version-info.txt
├── stack.yaml
└── test
    ├── Main.hs
    ├── Scrabble
    │   ├── BagTests.hs
    │   ├── BoardTests.hs
    │   ├── GameTests.hs
    │   ├── ScrabbleArbitrary.hs
    │   ├── ScrabbleTests.hs
    │   └── SearchTests.hs
    └── TestHelpers.hs
```

Info about code in the main src tree:

* Scrabble.hs             - Main entry point
* Scrabble/Bag.hs         - Tile and Bag representation
* Scrabble/Board.hs       - Board representation
* Scrabble/Dictionary.hs  - Letter and Dictionary representation
* Scrabble/Game.hs        - Player and Game state representation
* Scrabble/ListBoard.hs   - A list based representation of the board
* Scrabble/Matrix.hs      - Generic matrix code used to represent the board
* Scrabble/Play.hs        - Code for placing tiles onto the board, validation, scoring, etc.
* Scrabble/Position.hs    - Simple code to represent (x,y) coordinates.
* Scrabble/ReplHelpers.hs - Some quick testing helper functions for use on the REPL.
* Scrabble/Search.hs      - Code for searching the dictionary

Code for inputting commands:

* Scrabble/Commands/AST.hs         - AST for player input (and parsing)
* Scrabble/Commands/Interpreter.hs - Interpreter for player input
* Scrabble/Commands/SExpr.hs       - Generic sexpr parser

