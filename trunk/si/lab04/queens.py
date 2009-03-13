#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
from collections import deque


usage = u"""\
Rozwiązuje problem N królowych dla szachownicy NxN.
Użycie: python """ + sys.argv[0] + """ N\
"""

open_nodes = deque() # węzły do rozwinięcia
closed_nodes = [] # zbadane węzły
path = [] # pozycje królowych w kolejnych wierszach szachownicy


def dfs(N):
    global path
    u"""Algorytm Depth First Search przeszukiwania przestrzeni rozwiązań."""
    if not open_nodes:
        return False
    node = best(open_nodes)
    open_nodes.remove(node)
    closed_nodes.append(node)
    path = list(node)
    print path
    for x_pos in range(N):
        if not is_conflicted(path, x_pos):
            neighbor = list(path + [x_pos])
            if neighbor not in closed_nodes:
                open_nodes.append(neighbor)
    if not len(path) < N:
        return True
    elif dfs(N):
        return True
    elif path:
        path.pop()
        return dfs(N)
    else:
        return False


def is_conflicted(solution, x_pos):
    """Sprawdź czy po dodaniu królowej w kolumnie x_pos wystąpi szachowanie."""
    for i, row in enumerate(solution):
        if x_pos == row or abs(x_pos - row) == abs(len(solution) - i):
            return True
    return False


def best(solutions):
    """Funkcja oceny: zwraca najlepsze rozwiązanie z podanych."""
    return solutions[-1]


def queens_solution_dfs(N):
    """Znajduje rozwiązanie dodając kolejno wiersze z jedną królową."""
    w0 = [0]
    open_nodes.append(w0)
    return dfs(N)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        N = int(sys.argv[1])
        if queens_solution_dfs(N):
            print path
        else:
            print u'Nie znaleziono rozwiązania.'
    else:
        print usage

