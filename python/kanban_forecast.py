#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Kanban style forecasting for sprints/projects using the Monte Carlo method."""

import random
import sys
import re

SIMULATION_COUNT = 10000
HOURS_IN_DAY = 24

def convert_lead_time(s):
    groups = re.match(r'([0-9]+)d([0-9]+)h', s).groups()
    return float(groups[0]) * HOURS_IN_DAY + float(groups[1])

if (len(sys.argv) != 4):
    print('usage: ' + sys.argv[0] + ' DEADLINE_IN_DAYS DEVELOPERS_COUNT STORY_COUNT < LEAD_TIMES')
    sys.exit(2)

deadline_in_days = int(sys.argv[1])
developers_count = float(sys.argv[2])
story_count = int(sys.argv[3])

# lead times for all recent done stories, in format XdYh, e.g.: 1d5h 3d22h 2d0h
lead_times_input = ''.join(sys.stdin.readlines())

lead_times = map(lambda x: convert_lead_time(x),
        filter(lambda x: re.match(r'[0-9]+d[0-9]+h', x),
        lead_times_input.strip().replace('\n', ' ').split(' ')))
#print('lead times: ' + str(lead_times) + '\n')

# simulated durations in days
simulations = []
success_count = 0.0
for i in xrange(SIMULATION_COUNT):
    linear_duration = 0
    for j in xrange(story_count):
        ub_story_time = random.choice(lead_times)
        linear_duration += ub_story_time
    duration = linear_duration / developers_count
    simulations.append(duration)
    if duration <= deadline_in_days * HOURS_IN_DAY:
        success_count += 1
success_probability = success_count / SIMULATION_COUNT

#print('simulations: ' + str(simulations))
print('success probability: ' + str(success_probability * 100) + '%')
