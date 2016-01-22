# Scrabble

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

### What's here

```
$ tree .
.
├── scrabble.cabal
├── src
│   ├── Scrabble
│   │   ├── Bag.hs
│   │   ├── Board.hs
│   │   ├── Commands
│   │   │   ├── AST.hs
│   │   │   ├── Interpreter.hs
│   │   │   └── SExpr.hs
│   │   ├── Game.hs
│   │   ├── ListBoard.hs
│   │   ├── Matrix.hs
│   │   ├── Position.hs
│   │   ├── Search.hs
│   │   └── Types.hs
│   └── Scrabble.hs
└── test
    ├── Main.hs
    ├── ScrabbleTests.hs
    └── UnitTests
        └── TestHelpers.hs
```

Info about code in the main src tree:

* Scrabble.hs           - the main entry point
* Scrabble/Bag.hs       - code for the bag and tiles
* Scrabble/Board.hs     - code for the board and placing tiles on it
* Scrabble/Commands/AST.hs         - AST for player input (and parsing)
* Scrabble/Commands/Interpreter.hs - interpreter for player input
* Scrabble/Commands/SExpr.hs       - generic sexpr parser
* Scrabble/Game.hs      - code for managing game state
* Scrabble/ListBoard.hs - A list based representation of the board.
* Scrabble/Matrix.hs    - Generic matrix code used to represent the board.
* Scrabble/Position.hs  - Simple code to represent (x,y) coordinates.
* Scrabble/Search.hs    - code for searching the dictionary
* Scrabble/Types.hs     - basic types needed for all of Scrabble.

