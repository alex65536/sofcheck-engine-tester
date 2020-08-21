from common.position import Position
from common.engine import Engine
from threading import Thread
from queue import Queue, Empty
import os
import sys
import traceback


class EngineRunner:
    def __init__(self, exe_name, args=[], time=None, depth=None, threads=None,
                 observer=None, cmp_full=False):
        assert time is None or depth is None
        if threads is None:
            threads = os.cpu_count()
        self.__threads = []
        self.__results = []
        self.__weird = []
        self.__time = time
        self.__depth = depth
        self.__observer = observer
        self.__cmp_full = cmp_full
        self.__queue = Queue()
        self.__has_exception = False
        for _ in range(threads):
            thread = Thread(target=self.__thread_proc,
                            args=[Engine(exe_name, args)])
            thread.start()
            self.__threads.append(thread)

    def join(self):
        for _ in self.__threads:
            self.__queue.put(None)
        for thread in self.__threads:
            thread.join()
        if self.__has_exception:
            raise RuntimeError('Some threads finished with exception')

    def __thread_proc(self, engine):
        try:
            try:
                while True:
                    position = self.__queue.get()
                    if position is None:
                        break
                    self.__eval_position(engine, position)
                    self.__observer.observe(position)
            finally:
                engine.terminate()
        except:
            traceback.print_exc(file=sys.stderr)
            self.__has_exception = True

    def add(self, position):
        self.__queue.put(position)

    def __eval_position(self, engine, position):
        if position.best_score().kind != 'simple':
            return
        engine.set_position(position.fen)
        if self.__time is not None:
            res = engine.run_fixed_time(self.__time)
        elif self.__depth is not None:
            res = engine.run_fixed_depth(self.__depth)
        if self.__cmp_full:
            best_score = position.best_score().value
            calculated_score = position.move(res.move).score()
        else:
            depth = max(1, res.depth - 1)
            best_score = position.best_score_depth(depth).value
            calculated_score = position.move(res.move).score_depth(depth)
        if calculated_score.kind == "simple":
            calculated_score = calculated_score.value
        else:
            calculated_score = -30000
        if res.score.kind != 'simple':
            self.__weird.append({
                'fen': position.fen,
                'score': res.score,
                'move': res.move,
                'best_score': res.best_score})
            return
        self.__results.append({
            'fen': position.fen,
            'score': res.score,
            'move': res.move,
            'best_score': best_score,
            'calc_score': calculated_score,
            'error': best_score - calculated_score})

    def print_stats(self):
        self.join()
        for item in self.__weird:
            print('We say mate when it\'s absent:', item)
        if not self.__results:
            print('No stats to display.')
            return
        sum_error = 0.0
        greater_error = {10: 0, 20: 0, 30: 0, 40: 0, 50: 0, 75: 0, 100: 0,
                         150: 0, 200: 0, 300: 0}
        for item in self.__results:
            error = item['error']
            sum_error += error
            for key in greater_error.keys():
                if error <= key:
                    greater_error[key] += 1
        avg_error = sum_error / len(self.__results)
        print('Average error: {:.3f}'.format(avg_error))
        for key, value in greater_error.items():
            print('Error <= {}: {:.3f}%'.format(
                key, value / len(self.__results) * 100.0))
