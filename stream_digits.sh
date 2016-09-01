#!/bin/bash

DOCKER=${DOCKER:-docker}

$DOCKER run --rm -it -v "$PWD":/home erlang escript stream_digits.erl $@
