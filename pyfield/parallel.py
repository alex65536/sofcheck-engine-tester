from __future__ import annotations

from threading import Thread, Lock
from typing import Any, Union, List, Tuple, Dict
import os
import chess
from chess import Color
import sys
import traceback
import time

from runner import EngineRunner, EngineOptions, UciEngineFactory
import util
from util import MatchWinner
from openings import AbstractBook, DefaultBook


class AbstractProgress:
    def step(self, src: Any):
        raise NotImplementedError()


class ParallelRunnerProgress(AbstractProgress):
    def __init__(self, total: int):
        self.__total = total
        self.__start_time = time.time()
        self.__count = 0

    def step(self, src: Any):
        self.__count += 1
        cur_time = time.time() - self.__start_time
        predict_time = cur_time / self.__count * self.__total
        rr: ParallelRunner = src
        sys.stderr.write(
            '{}/{} games completed ({}/{}), score = {}\n'.format(
                self.__count, self.__total, util.human_time(cur_time),
                util.human_time(predict_time),
                util.score_pair_to_str(
                    rr.first_wins, rr.draws, rr.second_wins)))


class ParallelRunner:
    __progress: AbstractProgress
    __options: EngineOptions
    __games: int
    __book: AbstractBook
    __results: Dict[MatchWinner, int]
    __runners: List[EngineRunner]
    __lock: Lock
    __threads: List[Thread]

    @property
    def draws(self) -> int:
        return self.__results[MatchWinner.DRAW]

    @property
    def first_wins(self) -> int:
        return self.__results[MatchWinner.FIRST]

    @property
    def second_wins(self) -> int:
        return self.__results[MatchWinner.SECOND]

    @property
    def progress(self) -> AbstractProgress:
        return self.__progress

    def __init__(self, games: int, first_cmd: Union[str, List[str]],
                 second_cmd: Union[str, List(str)],
                 options: EngineOptions, jobs: int = 0,
                 book: AbstractBook = None,
                 progress: AbstractProgress = None):
        if jobs == 0:
            jobs = os.cpu_count()
        if book is None:
            book = DefaultBook()

        first_factory = UciEngineFactory(first_cmd)
        second_factory = UciEngineFactory(second_cmd)

        self.__progress = progress
        self.__options = options
        self.__games = games
        self.__book = book
        self.__results = {winner: 0 for winner in MatchWinner}
        self.__runners = [
            EngineRunner(first_factory, second_factory, book.clone())
            for _ in range(jobs)
        ]
        self.__lock = Lock()
        self.__threads = [
            Thread(target=lambda idx: self.__thread_func(idx),
                   args=(idx,),
                   name=f'ParallelRunner-{idx}')
            for idx in range(jobs)
        ]

        for thread in self.__threads:
            thread.start()

    def __run_game(self, idx: int, switch_sides: bool):
        winner = self.__runners[idx].play(self.__options, switch_sides)
        with self.__lock:
            self.__results[winner] += 1
            if self.__progress is not None:
                self.__progress.step(self)

    def __thread_func(self, idx: int):
        try:
            while True:
                with self.__lock:
                    must_stop = True
                    if self.__games != 0:
                        must_stop = False
                        switch_sides = self.__games % 2 == 0
                        self.__games -= 1
                if must_stop:
                    break
                self.__run_game(idx, switch_sides)
        except Exception:
            with self.__lock:
                sys.stderr.write('Thread finished with exception:\n')
                traceback.print_exc(file=sys.stderr)
                sys.stderr.write('Terminating now.\n')
                # FIXME: exit the entire program, not only the current thread
                sys.exit(1)

    def join(self):
        for thread in self.__threads:
            thread.join()
        for runner in self.__runners:
            runner.close()

    def save_as_pgn(self, stream):
        # FIXME: implement it
        raise NotImplementedError()

    def save_as_dataset(self, stream):
        # FIXME: implement it
        raise NotImplementedError()
