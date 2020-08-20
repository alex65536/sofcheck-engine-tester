import lzma

class ScoreSimple:
    @property
    def kind(self):
        return "simple"

    @property
    def value(self):
        return self.__value

    def __lt__(self, other):
        return self._to_tuple() < other._to_tuple()
    
    def __le__(self, other):
        return self._to_tuple() <= other._to_tuple()

    def __gt__(self, other):
        return self._to_tuple() > other._to_tuple()

    def __ge__(self, other):
        return self._to_tuple() >= other._to_tuple()

    def __eq__(self, other):
        return self._to_tuple() == other._to_tuple()

    def __ne__(self, other):
        return self._to_tuple() != other._to_tuple()

    def __str__(self):
        return 'score ' + str(self.__value)

    def __repr__(self):
        return 'Score(' + str(self.__value) + ')'

    def __init__(self, value):
        self.__value = value

    def inv(self):
        return ScoreSimple(-self.__value)

    def _to_tuple(self):
        return (0, self.__value)


class ScoreMate:
    @property
    def kind(self):
        return "mate"

    @property
    def mate(self):
        return self.__mate

    def __lt__(self, other):
        return self._to_tuple() < other._to_tuple()
    
    def __le__(self, other):
        return self._to_tuple() <= other._to_tuple()

    def __gt__(self, other):
        return self._to_tuple() > other._to_tuple()

    def __ge__(self, other):
        return self._to_tuple() >= other._to_tuple()

    def __eq__(self, other):
        return self._to_tuple() == other._to_tuple()

    def __ne__(self, other):
        return self._to_tuple() != other._to_tuple()

    def __str__(self):
        return 'mate ' + str(self.__mate)

    def __repr__(self):
        return 'Mate(' + str(self.__value) + ')'

    def __init__(self, mate):
        self.__mate = mate

    def inv(self):
        return ScoreMate(-self.__mate)

    def _to_tuple(self):
        if self.__mate <= 0:
            return (-1, self.__mate)
        else:
            return (1, self.__mate)


class Move:
    def add_depth(self, depth, score):
        while len(self.__scores) <= depth:
            self.__scores.append(None)
        self.__scores[depth] = score

    def max_depth(self):
        return len(self.__scores) - 1

    def score(self):
        return self.__scores[-1] if self.__scores else None

    def __str__(self):
        result = 'move ' + self.move_str + '\n'
        for depth, score in enumerate(self.__scores):
            if self.__scores is None:
                continue
            result += 'depth ' + str(depth) + ' ' + str(score) + '\n'
        result += 'end move\n'
        return result

    def score_depth(self, depth):
        if depth < 0:
            return None
        if depth < len(self.__scores):
            return self.__scores[depth]
        return self.score()

    @property
    def move_str(self):
        return self.__move_str

    def __init__(self, move_str):
        self.__move_str = move_str
        self.__scores = []


class Position:
    @property
    def fen(self):
        return self.__fen

    def move(self, move_str):
        return self.__moves[move_str]

    def add_move(self, move):
        self.__moves[move.move_str] = move

    def best_score(self):
        return max((move.score() for move in self))
    
    def best_score_depth(self, depth):
        return max((move.score_depth(depth) for move in self))

    def __str__(self):
        result = 'fen ' + self.fen + '\n'
        for move in self:
            result += str(move)
        result += 'end fen\n'
        return result

    def __iter__(self):
        return iter(self.__moves.values())

    def __init__(self, fen):
        self.__fen = fen
        self.__moves = {}


class PositionParseError(Exception):
    pass


def read_next_position(fd):
    line = fd.readline().strip()
    if not line:
        return None
    cmd, fen = line.split(' ', 1)
    if cmd != 'fen':
        raise PositionParseError('"fen" token expected')
    result = Position(fen)
    color = fen.split(' ')[1]
    while True:
        line = fd.readline().strip()
        if not line:
            raise PositionParseError('"end fen" expected')
        if line == 'end fen':
            break
        cmd, move_str = line.split(' ', 1)
        if cmd != 'move':
            raise PositionParseError('"move" token expected')
        move = Move(move_str)
        while True:
            line = fd.readline().strip()
            if line == 'end move':
                break
            cmd, depth, kind, value = line.split(' ', 3)
            if cmd != 'depth':
                raise PositionParseError('"depth" token expected')
            depth = int(depth)
            value = int(value)
            if kind == 'score':
                score = ScoreSimple(value)
            elif kind == 'mate':
                score = ScoreMate(value)
            else:
                raise PositionParseError('"score" or "mate" token expected')
            if color == 'b':
                score = score.inv()
            move.add_depth(depth, score)
        result.add_move(move)
    return result


def read_all_positions(fd, max_count=0):
    result = []
    while True:
        position = read_next_position(fd)
        if position is None:
            break
        result.append(position)
        if max_count != 0 and len(result) == max_count:
            break
    return result


def load_from_file(file_name, max_count=0):
    return read_all_positions(lzma.open(file_name, 'rt'), max_count)
