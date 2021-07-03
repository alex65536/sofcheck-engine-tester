from __future__ import annotations

import chess
from chess import Board, Move
import random
from openings_graham import OPENINGS


class AbstractBook:
    def fill(self, board: Board):
        raise NotImplementedError()

    def assign(self, src: AbstractBook):
        pass

    def clone(self) -> AbstractBook:
        result = type(self)()
        result.assign(self)
        return result


class RandomBook(AbstractBook):
    def _do_fill(self, board: Board, id: int):
        raise NotImplementedError()

    def fill(self, board: Board):
        assert self._max_count > 0, '_max_count must be specified!'
        self._do_fill(board, self.__random.randrange(self._max_count))

    def __init__(self):
        self.__random = random.Random()
        self._max_count = 0


class DefaultBook(RandomBook):
    def _do_fill(self, board: Board, id: int):
        opening = OPENINGS[id]
        board.set_fen(chess.STARTING_FEN)
        for move_str in opening.split(' '):
            move = board.parse_san(move_str)
            board.push(move)

    def __init__(self):
        super().__init__()
        self._max_count = len(OPENINGS)


# FIXME: write this class
class FenListBook(RandomBook):
    pass
