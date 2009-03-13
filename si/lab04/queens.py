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
solution = [] # pozycje królowych w kolejnych wierszach szachownicy
calls = 0 # liczba wywołań funkcji dfs()


def queens_solution_dfs(N, w0):
    u"""Znajdź rozwiązanie dodając kolejno wiersze z jedną królową."""
    open_nodes.append([w0])
    if dfs(N):
        return solution
    else:
        return queens_solution_dfs(N, w0 + 1) # dla innej pozycji początkowej


def dfs(N):
    u"""Algorytm Depth First Search przeszukiwania przestrzeni rozwiązań."""
    global solution
    global calls
    calls += 1
    if not open_nodes:
        return False
    node = best(open_nodes)
    open_nodes.remove(node)
    closed_nodes.append(node)
    solution = node
    for x_pos in range(N):
        if not is_conflict(solution, x_pos):
            neighbor = list(solution + [x_pos])
            if neighbor not in closed_nodes:
                open_nodes.append(neighbor)
    if not len(solution) < N:
        return True
    elif dfs(N):
        return True
    elif solution:
        solution.pop()
        return dfs(N)
    else:
        return False


def is_conflict(solution, x):
    u"""Sprawdź czy po dodaniu królowej w kolumnie x wystąpi szachowanie."""
    for i, row in enumerate(solution):
        if x == row or abs(x - row) == abs(len(solution) - i):
            return True
    return False


def best(solutions):
    u"""Funkcja oceny: zwróć najlepsze rozwiązanie z podanych."""
    return solutions[0] # TODO: zwraca pierwsze z brzegu; wymyślić coś lepszego


def print_solution(solution):
    line = '+'
    for j in range(N):
        line += '---+'
    for i in range(N):
        print line
        for j in range(N+1):
            print '|',
            if j == solution[i]:
                print 'X',
            else:
                print ' ',
        print
    print line


if __name__ == '__main__':
    if len(sys.argv) > 1:
        sys.setrecursionlimit(20000)
        N = int(sys.argv[1])
        print_solution(queens_solution_dfs(N, 0))
        print 'Liczba wywołań:', calls
    else:
        print usage

