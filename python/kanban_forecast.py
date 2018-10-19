#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import random
import sys

if (len(sys.argv) != 4):
    print('usage: ' + sys.argv[0] + ' DEADLINE_IN_DAYS DEVELOPERS_COUNT STORY_COUNT')
    sys.exit(2)

DEADLINE_IN_DAYS = int(sys.argv[1])
DEVELOPERS_COUNT = float(sys.argv[2])
STORY_COUNT = int(sys.argv[3])

# lead times for all recent done stories, in days
LEAD_TIMES_INPUT = """
1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 5 5 5
5 5 5 5 5 5 5 5 5 5 5 8 8 8 8 8 8 8 13 13
"""
LEAD_TIMES = map(lambda x: float(x), filter(lambda x: x.isdigit(),
    LEAD_TIMES_INPUT.strip().replace('\n', ' ').split(' ')))
SIMULATION_COUNT = 10000

# magic multiplier
LEAD_TIMES = [x * 0.75 * 0.75 for x in LEAD_TIMES]

#print('lead times: ' + str(LEAD_TIMES))

# simulated durations in days
simulations = []
success_count = 0.0
for i in xrange(SIMULATION_COUNT):
    linear_duration = 0
    for j in xrange(STORY_COUNT):
        ub_story_time = random.choice(LEAD_TIMES)
        linear_duration += ub_story_time
    duration = linear_duration / DEVELOPERS_COUNT
    simulations.append(duration)
    if duration <= DEADLINE_IN_DAYS:
        success_count += 1
success_probability = success_count / SIMULATION_COUNT

#print('simulations: ' + str(simulations))
print('success probability: ' + str(success_probability * 100) + '%')
