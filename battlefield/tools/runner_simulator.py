#!/usr/bin/env python3
# Copyright (c) 2021 Alexander Kernozhitsky
#
# SoFCheck is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SoFCheck is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SoFCheck.  If not, see <https://www.gnu.org/licenses/>.

# This script simulates BattleField's TParallelRunner. It assumes that the time
# of engine match is a normal random variable with mu = 1.0. Then, it just
# simulates running `things` matches in `jobs` threads to compute the expected
# time.
#
# This script is needed to tune the time estimation function for BattleField.
# One might try various functions and see how good they are, based on the
# simulation results.
#
# It is recommended to run this script under PyPy, so it will work faster.
import heapq
import random
import argparse

parser = argparse.ArgumentParser(
    description='BattleField\'s TParallelRunner simulator')
parser.add_argument('-j', '--jobs', help='number of jobs', action='store',
                    type=int, required=True)
parser.add_argument('-t', '--things', help='number of matches', action='store',
                    type=int, required=True)
parser.add_argument('-s', '--sims',
                    help='number of simulations (default: 100\'000)',
                    action='store', type=int, default=100_000)
parser.add_argument('-g', '--sigma',
                    help='sigma of normal distribution (default: 0.25)',
                    action='store', type=float, default=0.25)
args = parser.parse_args()

jobs = args.jobs
things = args.things
iters = args.sims
sigma = args.sigma

assert jobs > 0, 'jobs must be positive'
assert things > 0, 'things must be positive'
assert iters > 0, 'iters must be positive'
assert sigma > 0.0, 'sigma must be positive'

sum_time = 0.0

for i in range(iters):
    heap = []
    time = 0.0
    for _ in range(things):
        if len(heap) == jobs:
            time = heapq.heappop(heap)
        heapq.heappush(heap, time + max(0.0, random.gauss(1.0, sigma)))
    time = max(heap + [time])
    sum_time += time

got_time = sum_time / iters
approx_time = things / jobs + (1.0 - 2.0 / (jobs + 1)) * \
    max(0.5, 1.0 - things / (2.0 * jobs))
print('Completed time:', got_time)
print('Estimated time:', approx_time)
print('Percentage:', str(got_time / approx_time * 100) + '%')
