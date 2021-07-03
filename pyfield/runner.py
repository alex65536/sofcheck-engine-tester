import chess
from chess import Board, Move, Color
import chess.engine
from chess.engine import SimpleEngine, Limit, PovScore, Score, Cp
from typing import Optional, Union, List, Tuple, Dict
import warnings
import sys

import openings
import util
from util import GameWinner, MatchWinner


class EngineOptions:
    def __init__(self, time: Optional[int] = None,
                 depth: Optional[int] = None,
                 score_threshold: Optional[int] = None):
        assert (time is None) ^ (depth is None)
        self.time = time
        self.depth = depth
        self.score_threshold = score_threshold

    def get_limit(self) -> Limit:
        if self.time is not None:
            return Limit(time=self.time / 1000.0)
        if self.depth is not None:
            return Limit(depth=self.depth)
        assert False, 'Either time or depth must be set'


class UciEngineFactory:
    def __init__(self, command: Union[str, List[str]]):
        self.command = command

    def create(self) -> Tuple[SimpleEngine, str]:
        engine = chess.engine.SimpleEngine.popen_uci(self.command, timeout=1.0)
        engine_name = engine.id['name']
        engine_cmd = self.command if isinstance(self.command, str) \
            else ' '.join(self.command)
        name = f'{engine_name} at {engine_cmd}'
        return engine, name


class WinnerPredictor:
    __predictions: Dict[Color, Score]
    __threshold: Optional[int]

    def __init__(self, threshold: Optional[int]):
        self.__predictions = {
            chess.WHITE: Cp(0),
            chess.BLACK: Cp(0)
        }
        self.__threshold = threshold

    def add_score(self, color: Color, score: Score):
        self.__predictions[color] = score

    def __do_predict(self, score: Score) -> GameWinner:
        threshold = self.__threshold
        if score.is_mate():
            mate = score.mate()
            return GameWinner.WHITE if mate >= 0 else GameWinner.BLACK
        if threshold is None:
            return GameWinner.UNKNOWN
        assert isinstance(score, Cp)
        value = score.cp
        if value >= threshold:
            return GameWinner.WHITE
        if value <= -threshold:
            return GameWinner.BLACK
        return GameWinner.UNKNOWN

    def predict(self) -> GameWinner:
        white_prediction = self.__do_predict(self.__predictions[chess.WHITE])
        black_prediction = self.__do_predict(self.__predictions[chess.BLACK])
        if white_prediction == black_prediction:
            return white_prediction
        return GameWinner.UNKNOWN


def _outcome_to_game_winner(outcome: chess.Outcome) -> GameWinner:
    if not outcome.winner:
        return GameWinner.DRAW
    return GameWinner.WHITE if outcome.winner == chess.WHITE \
        else GameWinner.BLACK


def _game_to_match_winner(winner: GameWinner,
                          switch_sides: bool) -> MatchWinner:
    assert winner != GameWinner.UNKNOWN
    if winner == GameWinner.DRAW:
        return MatchWinner.DRAW
    if winner == GameWinner.WHITE:
        return MatchWinner.SECOND if switch_sides else MatchWinner.FIRST
    if winner == GameWinner.BLACK:
        return MatchWinner.FIRST if switch_sides else MatchWinner.SECOND
    assert False, 'Not all the cases were considered'


# FIXME: move it somewhere
class ScoredGameNotation:
    def __init__(self, board: Board, scores: List[Score],
                 names: Dict[Color, str], winner: GameWinner):
        self.board = board
        self.scores = scores
        self.names = names
        self.winner = winner


class InvalidMoveError(Exception):
    pass


class EngineRunner:
    __factories: List[UciEngineFactory]
    __games: List[ScoredGameNotation]
    __engines: List[Optional[SimpleEngine]]
    __engine_names: List[str]
    __book: openings.AbstractBook

    def __init__(self, first_factory: UciEngineFactory,
                 second_factory: UciEngineFactory,
                 book: openings.AbstractBook):
        self.__factories = [first_factory, second_factory]
        self.__book = book
        self.__games = []
        self.__engines = [None, None]
        self.__engine_names = ['', '']
        for idx in range(len(self.__engines)):
            self.__open_engine(idx)

    def __close_engine(self, idx: int):
        engine = self.__engines[idx]
        if engine is None:
            return
        try:
            try:
                engine.quit()
            finally:
                engine.close()
        except Exception as exc:
            warnings.warn(exc)
        self.__engines[idx] = None

    def __open_engine(self, idx: int):
        self.__close_engine(idx)
        self.__engines[idx], self.__engine_names[idx] = \
            self.__factories[idx].create()

    def __ensure_engine(self, idx: int):
        if self.__engines[idx] is None:
            self.__open_engine(idx)

    def close(self):
        for idx in range(len(self.__engines)):
            self.__close_engine(idx)

    def games(self):
        return self.__games

    def __handle_engine_error(self, idx: int, exc: Exception):
        name = self.__engine_names[idx]
        if isinstance(exc, chess.engine.EngineError):
            sys.stderr.write(f'Engine {name} misbehaved :(\n')
        elif isinstance(exc, chess.engine.EngineTerminatedError):
            sys.stderr.write(f'Engine {name} died :(\n')
        elif isinstance(exc, InvalidMoveError):
            sys.stderr.write(f'Engine {name} played an invalid move :(\n')
        else:
            raise exc

    def play(self, options: EngineOptions, switch_sides: bool) -> MatchWinner:
        # Stub class which is used as game ID for the chess engine
        class Game:
            pass

        board = Board()
        self.__book.fill(board)
        scores = [Cp(0) for _ in board.move_stack]
        game = Game()
        ids_color = {
            chess.WHITE: 1 if switch_sides else 0,
            chess.BLACK: 0 if switch_sides else 1
        }
        side_names = {
            color: self.__engine_names[ids_color[color]]
            for color in [chess.WHITE, chess.BLACK]
        }
        predictor = WinnerPredictor(options.score_threshold)
        winner = None

        while True:
            # FIXME : claim_draw=True is slow, maybe set to False and write a
            # hash-table for three-fold repetition ourself?
            outcome = board.outcome(claim_draw=True)
            if outcome:
                winner = _outcome_to_game_winner(outcome)
                break

            color = board.turn
            idx = ids_color[color]

            try:
                self.__ensure_engine(idx)
                engine: SimpleEngine = self.__engines[idx]
                play = engine.play(board, options.get_limit(), game=game,
                                   info=chess.engine.INFO_SCORE)
                if not board.is_legal(play.move):
                    raise InvalidMoveError()
            except Exception as exc:
                self.__close_engine(idx)
                winner = GameWinner.BLACK if board.turn == chess.WHITE \
                    else GameWinner.WHITE
                self.__handle_engine_error(idx, exc)
                break

            board.push(play.move)
            scores.append(play.info['score'].white())
            predictor.add_score(color, scores[-1])
            winner = predictor.predict()
            if winner != GameWinner.UNKNOWN:
                break

        self.__games.append(
            ScoredGameNotation(board, scores, side_names, winner))
        return _game_to_match_winner(winner, switch_sides)
