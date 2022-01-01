#!/usr/bin/env python3
# Copyright (c) 2022 Alexander Kernozhitsky
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

from chess import Board, Move
import sys
import argparse
from sgs_common import GameReader, GameCommand, BoardCommand, MovesCommand

try:
    from tqdm import tqdm
except ImportError:
    def tqdm(iterable):
        return iterable

DESCRIPTION = '''\
Rewrites a file in SoFGameSet format as a set of boards (i.e. without using
"start" and "move" commands). Basically, this means downgrading the SoFGameSet
file to dataset format used by BattleField v0.9.11 (and earlier).
'''

parser = argparse.ArgumentParser(description=DESCRIPTION)
parser.add_argument(
    '-o', '--output', help='output file', action='store',
    type=argparse.FileType('w'), default=sys.stdout)
args = parser.parse_args()
out_file = args.output

for game in tqdm(GameReader(sys.stdin)):
    out_file.write(str(game[0]) + '\n')
    fen = None
    for cmd in game[1:]:
        if isinstance(cmd, BoardCommand):
            fen = cmd.fen
            cmd.is_starting = False
            out_file.write(str(cmd) + '\n')
        elif isinstance(cmd, MovesCommand):
            if fen is None:
                raise RuntimeError(
                    'Command "moves" is not preceded by "board" or "start"')
            board = Board(fen)
            for move in cmd.moves:
                move = board.parse_uci(move)
                board.push(move)
                fen = board.fen(en_passant='fen')
                out_cmd = BoardCommand(fen)
                out_cmd.is_starting = False
                out_file.write(str(out_cmd) + '\n')
        else:
            # Skip unknown command
            pass
    out_file.write('\n')
