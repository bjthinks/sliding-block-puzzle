# Sliding Block Puzzle (AKA 15 Puzzle)

This is a tty version of the 15 puzzle using Unicode box drawing characters to render the movable squares.
You must run this in a Unicode-aware terminal program, which includes any recent terminal emulator on Linux.

This does not work on Windows for multiple minor reasons, including a lack of Unicode support(!).

Please build with:
```
sudo apt install libsdl2-mixer-dev
cabal run
```
If sound causes problems, you can disable it with:
```
cabal run --flags=-sound
```
