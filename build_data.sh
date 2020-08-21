#!/usr/bin/env sh
set -e

cd engine_data_create
lazbuild EngineDataCreate.lpi
bin/engine_data_create ../games/*.pgn > ../data/positions.data
lzma ../data/positions.data
