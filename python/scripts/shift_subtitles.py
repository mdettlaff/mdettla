#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

u"""Shifts subtitles in srt format by a given number of seconds."""

__author__ = u'Michał Dettlaff'


import datetime
import doctest
import re
import sys


def parse_line(line, delta_seconds):
    u"""
    >>> print parse_line('foo', 0)
    foo
    >>> print parse_line('foo', 1)
    foo
    >>> print parse_line('00:00:41,578 --> 00:00:44,968', 0)
    00:00:41,578 --> 00:00:44,968
    >>> print parse_line('00:00:41,578 --> 00:00:44,968', 1)
    00:00:42,578 --> 00:00:45,968
    >>> print parse_line('00:00:41,578 --> 00:00:44,96', 1)
    00:00:41,578 --> 00:00:44,96
    >>> print parse_line('00:00:41,578 --> 00:00:44,968', 2)
    00:00:43,578 --> 00:00:46,968
    >>> print parse_line('00:00:41,578 --> 00:00:44,968', -3)
    00:00:38,578 --> 00:00:41,968
    >>> print parse_line('00:00:55,578 --> 00:00:58,968', 5)
    00:01:00,578 --> 00:01:03,968
    >>> print parse_line('02:59:55,508 --> 03:59:59,908', 1)
    02:59:56,508 --> 04:00:00,908
    """

    def groups_to_time(groups):
        return datetime.timedelta(
                hours = int(groups[0]), minutes = int(groups[1]), \
                seconds = int(groups[2]), milliseconds = int(groups[3]))

    def time_to_str(duration):
        hours = duration.seconds / (60 * 60)
        minutes = (duration.seconds / 60) % 60
        seconds = duration.seconds % 60
        milliseconds = duration.microseconds / 1000
        return '%02d:%02d:%02d,%03d' % (hours, minutes, seconds, milliseconds)

    time_regex = r'(\d\d):(\d\d):(\d\d),(\d\d\d)'
    duration_regex = time_regex + ' --> ' + time_regex
    match = re.match(duration_regex, line)
    if match is not None:
        time_start = groups_to_time(match.groups()[:4])
        time_end = groups_to_time(match.groups()[4:])
        delta = datetime.timedelta(seconds = delta_seconds)
        return '%s --> %s' % (time_to_str(time_start + delta), \
                time_to_str(time_end + delta))
    return line


def main():
    if len(sys.argv) < 2:
        print >> sys.stderr, 'Usage: shift_subtitles.py SECONDS'
        sys.exit(2)
    delta_seconds = int(sys.argv[1])
    for line in sys.stdin.readlines():
        print parse_line(line.rstrip(), delta_seconds)


if __name__ == '__main__':
    doctest.testmod()
    main()

