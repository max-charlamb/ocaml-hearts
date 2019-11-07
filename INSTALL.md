
# Install Dependencies
The only package we use is ANSITerminal.
```
opam install ANSITerminal
```

# Run the program
```
make play
```

# Program Commands
Type ```start``` to begin. You will be shown a deck on the upper left corner. The first two symbols are the rank and suite. The third is the position in your hand. Use ```play [n]``` where n is the card position you want to play. 

# Current Bugs
Currently the program will crash if you try to input anything or move the cursor while the game is being displayed.