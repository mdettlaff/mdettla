#!/usr/bin/env python
# -*- coding: UTF-8 -*-

"""Pokazuje kto napisał ile linii kodu, na podstawie cvs annotate."""

import os
import re
import sys

USAGE = 'Użycie: cvs_blame ŹRÓDŁA...'


def get_users_loc(args):
    users_loc = {} # loc = lines of code
    command = 'cvs annotate ' + ' '.join(args) + ' 2>/dev/null'
    for line in os.popen(command).readlines():
        match = re.match(r'\d+\.\d+\s+\((\w+)', line)
        if match:
            username = match.group(1)
            if username not in users_loc:
                users_loc[username] = 0
            users_loc[username] += 1
    return users_loc


def show_users_loc(users_loc):
    sorted_users_loc = sorted(users_loc.iteritems(),
            key = lambda x: x[1], reverse = True)
    total_loc = sum(users_loc.values())
    sorted_users_loc.append(('razem', total_loc))
    get_longest = lambda xs: max([len(str(x)) for x in xs])
    padding_username = get_longest([x[0] for x in sorted_users_loc])
    padding_loc = get_longest([x[1] for x in sorted_users_loc])
    format = '%%-%ds  %%%dd (%%.2f%%%%)' % (padding_username, padding_loc)
    for user, loc in sorted_users_loc:
        percent = float(loc) / total_loc * 100
        print format % (user, loc, percent)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit(USAGE)
    else:
        users_loc = get_users_loc(sys.argv[1:])
        if users_loc:
            show_users_loc(users_loc)
        else:
            print >> sys.stderr, 'Brak wyników dla cvs annotate.'

