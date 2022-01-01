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

from chess import Board


class CommandFilter:
    def __init__(self, in_stream):
        self.__lines = iter(in_stream)

    def __iter__(self):
        return self

    def __next__(self):
        while True:
            line = next(self.__lines)
            normalized = line.strip()
            if not normalized or normalized[0] == '#':
                continue
            return normalized


def norm_fen(fen):
    return Board(fen).fen(en_passant='fen')


class Command:
    pass


class GameCommand(Command):
    def __init__(self, winner, label):
        self.winner = winner
        self.label = label

    def __str__(self):
        return f'game {self.winner} {self.label}'


class TitleCommand(Command):
    def __init__(self, title):
        self.title = title

    def __str__(self):
        return f'title {self.title}'


class BoardCommand(Command):
    def __init__(self, fen=Board.starting_fen):
        self.fen = norm_fen(fen)
        self.is_starting = (self.fen == norm_fen(Board.starting_fen))

    def __str__(self):
        if self.is_starting:
            return 'start'
        return f'board {self.fen}'


class MovesCommand(Command):
    def __init__(self, moves):
        self.moves = moves

    def __str__(self):
        move_str = ' '.join(self.moves)
        return f'moves {move_str}'


class GameReader:
    def __init__(self, in_stream):
        self.__lines = CommandFilter(in_stream)
        try:
            self.__last_game = next(self.__lines)
            if not self.__last_game.startswith('game '):
                raise RuntimeError('File must start with "game" command')
        except StopIteration:
            self.__last_game = None

    def __iter__(self):
        return self

    def __parse(self, line):
        line_split = line.strip().split(maxsplit=1)
        name = line_split[0]
        body = '' if len(line_split) == 1 else line_split[1]
        if name == 'game':
            args = body.split()
            if len(args) < 2:
                raise RuntimeError('Command "game" must contain "winner" and "label"')
            if args[0] not in ['W', 'B', 'D', '?']:
                raise RuntimeError('Invalid winner')
            return GameCommand(args[0], args[1])
        if name == 'title':
            return TitleCommand(body)
        if name == 'board':
            return BoardCommand(body)
        if name == 'start':
            if len(body) != 0:
                raise RuntimeError('Command "start" must have empty body')
            return BoardCommand()
        if name == 'moves':
            return MovesCommand(body.split())
        # Unknown command, ignore it
        return None

    def __parse_and_add(self, cmd_list, line):
        cmd = self.__parse(line)
        if cmd is not None:
            cmd_list.append(cmd)

    def __next__(self):
        if self.__last_game is None:
            raise StopIteration()
        lines = []
        self.__parse_and_add(lines, self.__last_game)
        while True:
            try:
                line = next(self.__lines)
                if line.startswith('game '):
                    self.__last_game = line
                    break
                self.__parse_and_add(lines, line)
            except StopIteration:
                self.__last_game = None
                break
        return lines
