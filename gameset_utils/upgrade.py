#!/usr/bin/env python3
# Copyright (c) 2021-2022 Alexander Kernozhitsky
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
Upgrades dataset format generated by BattleField v0.9.11 (and earlier) to
canonical SoFGameSet, which can be used to generate datasets for SoFCheck.
Note that starting from BattleField v0.9.12, this script is not needed to
perform conversion, as BattleField is able to generate canonical SoFGameSet
itself.
'''

parser = argparse.ArgumentParser(description=DESCRIPTION)
parser.add_argument(
    '-o', '--output', help='output file', action='store',
    type=argparse.FileType('w'), default=sys.stdout)
args = parser.parse_args()
out_file = args.output

for game in tqdm(GameReader(sys.stdin)):
    out_file.write(str(game[0]) + '\n')
    # Filter unused commands
    game = [cmd for cmd in game
            if isinstance(cmd, (BoardCommand, MovesCommand))]
    if len(game) < 1:
        raise RuntimeError('Game is too small')
    if isinstance(game[0], BoardCommand):
        fen = game[0].fen
    else:
        raise RuntimeError('Second line in the game must be a board')
    out_file.write(str(game[0]) + '\n')
    board = Board(fen)
    moves = []
    for cmd in game[1:]:
        assert not isinstance(cmd, GameCommand)
        if isinstance(cmd, BoardCommand):
            new_fen = cmd.fen
            new_board = Board(new_fen)
            legal_moves = list(board.legal_moves)
            chosen_move = None
            for move in legal_moves:
                board.push(move)
                cur_fen = board.fen(en_passant='fen')
                board.pop()
                if new_fen == cur_fen:
                    chosen_move = move
                    break
            if chosen_move is None:
                fen = board.fen(en_passant='fen')
                msg = f'No suitable move found to go from {fen} to {new_fen}'
                raise RuntimeError(msg)
            moves.append(chosen_move.uci())
            board.push(chosen_move)
        elif isinstance(cmd, MovesCommand):
            for move in cmd.moves:
                board.push(board.parse_uci(move))
                moves.append(move)
        else:
            assert False
    out_file.write(str(MovesCommand(moves)) + '\n\n')
