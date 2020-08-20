import argparse
import position
from runner import EngineRunner
from progress import Progress
import os
import sys

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('engine_cmd', type=str, nargs='+',
                        help='engine command line')
    parser.add_argument('-i', '--data', type=str,
                        help='file with positions (in .data.lzma format)')
    parser.add_argument('-p', '--max_positions', type=int, default=None,
                        help='use no more than MAX_POSITIONS first positions\
                            (default: process all positions)')
    parser.add_argument('-j', '--jobs', type=int, default=os.cpu_count(),
                        help='number of jobs to execute\
                            (default: number of CPU cores)')
    parser.add_argument('-t', '--time', type=int, default=None,
                        help='run on fixed time TIME')
    parser.add_argument('-d', '--depth', type=int, default=None,
                        help='run on fixed depth DEPTH, conflicts with --time')

    args = parser.parse_args()
    if args.time is None and args.depth is None:
        args.time = 1000

    p = position.load_from_file(args.data, args.max_positions)
    sys.stderr.write('Data loaded successfully.\n')
    r = EngineRunner(args.engine_cmd[0], args.engine_cmd[1:], time=args.time,
                     depth=args.depth, threads=args.jobs,
                     observer=Progress(len(p), args.jobs))
    for position in p:
        r.add(position)
    r.print_stats()
