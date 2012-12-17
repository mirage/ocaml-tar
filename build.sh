#!/bin/bash

oasis setup
ocaml setup.ml -configure
make
make install
