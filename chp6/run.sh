#!/bin/bash

# -pa will add a directory to the code path.
# The code path is a list of all directories which modules are loaded from sequentially
# -run will run module and optionally a function taking 0+ arguments

erl -pa ./ebin -noshell -run hhfuns main
