# Andy's Baseball (SML/OpenGL)

This folder contains a Standard ML translation of `Best/BASEBALL.BAS`.

`baseball.sml` keeps the original two-player flow:

- visitors bat first, then home bats;
- pitch keys are `8` for slow, `5` for medium, and `2` for fast;
- `Space` swings;
- `B` bunts/forces a grounder;
- on ground balls the defense throws with `6` to first, `8` to second, `4` to third, or `2` home;
- scoring, outs, strikes, runners, home runs, popouts, grounders, and inning changes mirror the QBASIC program.

The code is written as a functor over `OPENGL_SURFACE` so it can be connected to the OpenGL binding available in a given SML installation, such as an SML/NJ GLUT wrapper or an MLton SDL2/OpenGL binding.  The rendering layer deliberately uses only a small set of OpenGL-style primitives (`clear`, `color`, `line`, `rect`, `circle`, `text`, `present`, and keyboard polling), making the game logic portable across SML OpenGL packages.
