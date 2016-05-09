# Cepler

Solar system simulator using CEPL (https://github.com/cbaggers/cepl)

Physics (including VSOP87) from TANSTAAFL (http://wiki.github.com/nforrester/tanstaafl)

OpenGL, OpenAL and ALUT, SDL2, freetype2, and libdevil are required.
To install libdevil on debian: apt-get install libdevil1c2

Made for the Spring 2016 Lisp Game Jam (https://itch.io/jam/spring-2016-lisp-game-jam)

# How to play the game

You start out in overhead mode. You can only use cameras 4 and 5, but you can zoom out and move left/right.
Clicking the mouse will start pluto off near where the moon should be.
Click in the direction you want pluto to go. Pluto will be much faster if you increase the step by pressing q.
Press e to slow down if it gets too fast.
If you run out of fuel you can press x to remove the current pluto, then click the mouse again and start over.
Or, you can press backspace to cheat and get more fuel.
Try to hit each planet in the solar system.

Press escape to switch to normal mode.
When controlling Pluto in normal mode, use the mouse to aim, and the w/s/a/d keys to accelerate forward/backwards/left/right.
You can follow other planets by pressing tab, but you cannot control them.

# How to change the screen resolution

For example: cepler -r 1024x768
Or, from lisp:
(cepler:start-game :width 1024 :height 768)