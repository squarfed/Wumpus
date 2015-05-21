pandoc -s -f markdown+lhs \
       --no-wrap \
       -t latex+lhs \
       -V documentclass=memoir -V classoption=10pt \
       -V classoption=a4paper \
       -H header.tex --listings \
       -o Wumpus.pdf \
       Wumpus.lhs
