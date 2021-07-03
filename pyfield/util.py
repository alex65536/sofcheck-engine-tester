from enum import Enum


class GameWinner(Enum):
    WHITE = '1-0'
    DRAW = '1/2-1/2'
    BLACK = '0-1'
    UNKNOWN = '*'


class MatchWinner(Enum):
    FIRST = +1
    DRAW = 0
    SECOND = -1


def _score_to_str_impl(value: int) -> str:
    suffix = '.0' if value % 2 == 0 else '.5'
    return str(value // 2) + suffix


def score_pair_to_str(first: int, draw: int, second: int) -> str:
    return _score_to_str_impl(first * 2 + draw) + ':' + \
           _score_to_str_impl(second * 2 + draw)


def human_time(secs: float):
    if secs < 60:
        return '{:.2f} sec'.format(secs)
    if secs < 3600:
        return '{:.2f} min'.format(secs / 60)
    if secs < 3600 * 24:
        return '{:.2f} hours'.format(secs / 3600)
    return '{:2f} days'.format(secs / (3600 * 24))
