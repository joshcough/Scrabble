# Scrabble

## Place some words on the board:

    $ cd Haskell
    $ cabal repl
    *Scrabble> let (b,s) = quickPut [("HELL", Vertical, (7,7)), ("HAS", Horizontal, (5,6)), ("AH", Horizontal, (5,7))]
    *Scrabble> printListBoard True b
    *Scrabble> s

## Test the search engine:

    *Scrabble> cheat (matchAll [containsAny "z", containsLetterAtPos 'w' 4])
    ["buzzwig","buzzwigs","buzzword","buzzwords","unbowdlerized","zugzwang","zugzwanged","zugzwanging","zugzwangs"]

    *Scrabble> cheat (matchAll [containsAny "x", containsLetterAtPos 'b' 4, endsWith 'g'])
    ["exhibiting","exorbitating","extubating","kickboxing","soapboxing"]
