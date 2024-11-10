# Asteroids

## Getting Started
```console
cabal run
```

Some custom levels can be found within the \Levels folder.

## Creating a custom level
To create your own custom level you should use the PlaceHolderEmptyLvl.txt located in the \Levels folder as a template for your new level.

To automatically run the level, the level should be renamed to fit the following description:
Lvl{Number/Text}.txt

Furthermore, it's advised to not place any asteroids or missiles within the square representing the screen as you might load the asteroid on top of your spaceship resulting in an instant gameOver.

Asteroids may be generated using the 'A' symbol.
Missiles may be generated using the 'M' symbol.

Both capital and lower symbols work.