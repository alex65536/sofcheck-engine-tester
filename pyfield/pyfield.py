import argparse
import sys
from colorama import init, Fore, Back, Style
from math import sqrt, erf, log10

from parallel import ParallelRunnerProgress, ParallelRunner
from runner import EngineOptions
import util

P0_9 = 1.64485362695147
P0_95 = 1.95996398454005
P0_97 = 2.17009037758456
P0_99 = 2.57582930354890


def prob_check(p: float, len: float, win: int, draw: int, count: int):
    prob = (2 * win + draw) / (2 * count)
    window = len * (sqrt(prob * (1 - prob)) / sqrt(count))
    left = prob - window
    right = prob + window
    print(f'  p = {p:.2f}: ', end='')
    if left >= 0.5:
        print(f'{Fore.GREEN}{Style.BRIGHT}First wins{Style.RESET_ALL}')
    elif right <= 0.5:
        print(f'{Fore.RED}{Style.BRIGHT}Second wins{Style.RESET_ALL}')
    else:
        print(f'{Style.BRIGHT}Unclear{Style.RESET_ALL}')


def print_los(win: int, lose: int):
    print(f'LOS = {Style.BRIGHT}', end='')
    if win + lose == 0:
        print(f'{Fore.WHITE}N/A{Style.RESET_ALL}')
        return
    value = 0.5 * (1.0 + erf((win - lose) / sqrt(2 * (win + lose))))
    if value < 0.1:
        print(Fore.RED, end='')
    elif value <= 0.9:
        print(Fore.YELLOW, end='')
    else:
        print(Fore.GREEN, end='')
    print(f'{value:.2f}{Style.RESET_ALL}')


def print_elo_difference(win: int, draw: int, lose: int):
    print('Elo difference = ', end='')
    if draw == 0 and lose == 0:
        print(f'{Style.BRIGHT}{Color.YELLOW}oo{Style.RESET_ALL}')
        return
    if draw == 0 and win == 0:
        print(f'{Style.BRIGHT}{Color.YELLOW}oo{Style.RESET_ALL}')
        return
    win_rate = (win + 0.5 * draw) / (win + draw + lose)
    elo_dif = -log10(1.0 / win_rate - 1.0) * 400.0
    print(f'{elo_dif:.2f}')


init()

parse = argparse.ArgumentParser(
    description='A tool to run micro-matches between chess engines. ' +
                'This is roughly the original Battlefield utility ' +
                'rewritten to Python.')
parse.add_argument('-v', '--version', action='version',
                   version='PyField (experimental version)')
parse.add_argument('-q', '--quiet', action='store_true',
                   help='do not show progress')
parse.add_argument('-j', '--jobs', action='store', type=int, default=0,
                   help='specify number of games to run simultaneoulsly')
# FIXME: support -o PGN_FILE
# FIXME: support -r FILE
parse.add_argument('-g', '--games', action='store', type=int, required=True,
                   help='number of games to run')
limits_group = parse.add_mutually_exclusive_group(required=True)
limits_group.add_argument('-d', '--depth', action='store', type=int,
                          help='run engines on fixed depth')
limits_group.add_argument(
    '-t', '--time', action='store', type=int,
    help='run engines on fixed time (in milliseconds) per move')
# FIXME: support -f FEN_FILE
parse.add_argument('-s', '--score', action='store', type=int,
                   help='terminate the game after both sides agree that the ' +
                        'score is larger than SCORE centipawns for the same ' +
                        'side')
parse.add_argument('engine1', action='store', type=str)
parse.add_argument('engine2', action='store', type=str)

args = parse.parse_args()

if args.games <= 0:
    sys.stderr.write('GAMES must be positive')
    sys.exit(1)
if args.jobs < 0:
    sys.stderr.write('JOBS must be non-negative')
    sys.exit(1)
if args.time is not None and args.time <= 0:
    sys.stderr.write('TIME must be positive')
    sys.exit(1)
if args.depth is not None and args.depth <= 0:
    sys.stderr.write('DEPTH must be positive')
    sys.exit(1)
if args.score is not None and args.score < 0:
    sys.stderr.write('SCORE must be non-negative')
    sys.exit(1)

progress = None if args.quiet else ParallelRunnerProgress(args.games)
book = None
runner = ParallelRunner(args.games, args.engine1, args.engine2,
                        EngineOptions(args.time, args.depth, args.score),
                        jobs=args.jobs, book=book, progress=progress)
runner.join()

print(f'Wins: {runner.first_wins}, Loses: {runner.second_wins}, ' +
      f'Draws: {runner.draws}')
print(f'{Style.BRIGHT}Confidence interval:{Style.RESET_ALL}')
prob_check(0.9, P0_9, runner.first_wins, runner.draws, args.games)
prob_check(0.95, P0_95, runner.first_wins, runner.draws, args.games)
prob_check(0.97, P0_97, runner.first_wins, runner.draws, args.games)
prob_check(0.99, P0_99, runner.first_wins, runner.draws, args.games)
print(f'{Style.BRIGHT}Other stats:{Style.RESET_ALL}')
print_los(runner.first_wins, runner.second_wins)
print_elo_difference(runner.first_wins, runner.draws, runner.second_wins)
