Hunt the Wumpus
===============
A Haskell verion of the classic computer
[game](http://www.atariarchives.org/morebasicgames/showpage.php?page=178).

Build
-----

    cabal configure
    cabal build

Build documentation
-------------------
You need to have [pandoc](http://johnmacfarlane.net/pandoc/index.html)
installed. Then issue:

    pandoc -f markdown+lhs --no-highlight -o Wumpus.pdf Wumpus.lhs
