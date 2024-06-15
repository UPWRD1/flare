#!/bin/sh
himecc -t rust lang.gram
mv lang.rs /workspaces/allegro/allegro/src/root/passes/frontend
mv lang_lexer.bin /workspaces/allegro/allegro/src/root/passes/frontend
mv lang_parser.bin /workspaces/allegro/allegro/src/root/passes/frontend