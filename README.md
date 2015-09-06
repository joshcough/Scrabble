# Scrabble

    $ cabal repl
    *Scrabble> cheat (Scrabble.Search.all [containsAny "z", containsLetterAtPos 'w' 4])
    ["buzzwig","buzzwigs","buzzword","buzzwords","unbowdlerized","zugzwang","zugzwanged","zugzwanging","zugzwangs"]
