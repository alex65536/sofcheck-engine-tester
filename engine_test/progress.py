import time
import sys
import util
from threading import Lock


class Progress:
    def __init__(self, total, each=1):
        self.__time = time.monotonic()
        self.__total = total
        self.__each = each
        self.__count = 0
        self.__lock = Lock()
    
    def observe(self, *args):
        with self.__lock:
            self.__count += 1
            if self.__count % self.__each == 0:
                self.__report()

    def __report(self):
        cur_time = time.monotonic() - self.__time
        total_time = cur_time / self.__count * self.__total
        print('Progress: {}/{} ({}/{})'.format(self.__count, self.__total,
            util.human_time(cur_time), util.human_time(total_time)))
