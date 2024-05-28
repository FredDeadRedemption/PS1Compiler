Welcome to the language PSych!

Before starting your adventure, ensure that you have downloaded the Ounit2 test library with the command:
"opam install Ounit2"

Moreover, you need a PSX emulator. Here we reccomend the psx-redux emulator that can be downloaded on this github:
"https://github.com/grumpycoders/pcsx-redux/"

Here you can load the .ps-exe file and look at how your own game runs.
This is done by pressing File->Load binary.
Then navigate to the compiled game -> PS1Compiler/compile/output/
One the game is loaded, press Emulation->Start emulation.

To compile the psx program you have made, navigate to the PS1compiler directory in an Ocaml evnirorment and write this (make sure to have python installed):

"python compile_all.py "filename"

To run the tests you can write:
For the integration test you need to navitage to the PS1compiler directory and write:

"python integration_test.py"

For the other tests, you can write:

"make test"

Have fun exploring the language 😄

There is a cool pong game included for you to try in compile/output

O
o  
 .
<>< 'lil fisk
