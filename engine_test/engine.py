import subprocess
from subprocess import PIPE
from position import ScoreSimple, ScoreMate


class EngineError(Exception):
    pass


class EngineResults:
    @property
    def depth(self):
        return self.__depth

    @property
    def move(self):
        return self.__move

    @property
    def score(self):
        return self.__score

    def __repr__(self):
        return 'EngineResults(depth={}, move={}, score={})'.format(
            self.depth, self.move, repr(self.score))

    def __init__(self, depth, move, score):
        self.__depth = depth
        self.__move = move
        self.__score = score


class Engine:
    def __read_line(self):
        return self.__subprocess.stdout.readline().strip()

    def __write_line(self, line):
        self.__subprocess.stdin.write(line + '\n')
        self.__subprocess.stdin.flush()

    def __handle_engine(self):
        depth = 0
        score = None
        while True:
            line = self.__read_line().split()
            if line[0] == 'bestmove':
                return EngineResults(depth, line[1], score)
            if line[0] == 'info':
                idx = 1
                while idx < len(line):
                    if line[idx] == 'depth':
                        depth = int(line[idx + 1])
                        idx += 2
                        continue
                    if line[idx] == 'score':
                        if line[idx + 1] == 'cp':
                            score = ScoreSimple(int(line[idx + 2]))
                            idx += 3
                            continue
                        if line[idx + 1] == 'mate':
                            score = ScoreMate(int(line[idx + 2]))
                            idx += 3
                            continue
                    idx += 1

    def __init__(self, exe_name, args=[]):
        self.__subprocess = subprocess.Popen(
            [exe_name] + args, bufsize=1, stdin=PIPE, stdout=PIPE,
            universal_newlines=True)
        self.__write_line('uci')
        while True:
            if self.__read_line() == 'uciok':
                break
        self.set_position()

    def set_position(self, fen=None):
        if not fen:
            self.__write_line('position startpos')
            return
        self.__write_line('position fen ' + fen)

    def terminate(self):
        self.__write_line('quit')
        self.__subprocess.wait()

    def run_fixed_time(self, time):
        self.__write_line('go movetime ' + str(time))
        return self.__handle_engine()

    def run_fixed_depth(self, depth):
        self.__write_line('go depth ' + str(depth))
        return self.__handle_engine()
