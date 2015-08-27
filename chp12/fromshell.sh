#!/bin/bash
# Based on the results of this test, I must assume that process started in this manner are system processes and trap exits.
# When fun2 is run using the -s method, it successfully traps the message from fun1
# When fun2 is spawned from fun3, it fails to receive the exit message from fun 1
erl -pa ./ebin -noshell -s fromshell fun2 -s fromshell fun3 # -s init stop
