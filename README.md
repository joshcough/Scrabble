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
├── Scrabble
│   ├── Bag.hs             -- Tile and Bag representation
│   ├── Board.hs           -- Board representation
│   ├── Commands           -- Code for inputting game commands
│   │   ├── AST.hs         -- AST for player input (and parsing)
│   │   ├── Interpreter.hs -- Interpreter for player input
│   │   └── SExpr.hs       -- Generic sexpr parser
│   ├── Dictionary.hs      -- Letter and Dictionary representation
│   ├── Game.hs            -- Player and Game state representation
│   ├── ListBoard.hs       -- A list based representation of the board
│   ├── Matrix.hs          -- Generic matrix code used to represent the board
│   ├── Move               -- Making a move, validation, scoring, etc
│   │   ├── Move.hs        -- Move representation and functions to make moves
│   │   ├── MoveHelpers.hs -- Code used by both scoring and validation
│   │   ├── Scoring.hs     -- Calculates score for a move
│   │   ├── Validation.hs  -- Checks to see if a move is valid
│   │   └── WordPut.hs     -- Representation of tiles put on the board in a turn
│   ├── Position.hs        -- Simple code to represent (x,y) coordinates
│   ├── ReplHelpers.hs     -- Some quick testing helper functions for use on the REPL
│   └── Search.hs          -- Code for searching the dictionary
└── Scrabble.hs            -- Main entry point
```

