#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys


usage = u"""\
Rozwiązuje problem N królowych dla szachownicy NxN przy pomocy algorytmu
Depth-First Search z heurystyką.
Użycie: python """ + sys.argv[0] + """ N\
"""

# węzłami są rozwiązania (ustawienia królowych na szachownicy)
open_nodes = [] # tuple: (heurystyka węzła, węzeł do rozwinięcia)
closed_nodes = [] # zbadane węzły
solution = [] # pozycje królowych w kolejnych wierszach szachownicy
calls = 0 # liczba wywołań funkcji dfs()


def queens_solution_dfs(N, w0):
    u"""Znajdź rozwiązanie dodając kolejno wiersze z jedną królową."""
    open_nodes.append((0, [w0]))
    if dfs(N):
        return solution
    else:
        return queens_solution_dfs(N, w0+1) # dla innej pozycji początkowej


def dfs(N):
    u"""Algorytm Depth-First Search przeszukiwania przestrzeni rozwiązań."""
    global solution
    global calls
    calls += 1
    if not open_nodes:
        return False
    h_node = min(open_nodes) # tupla: (heurystyka, węzeł o min. heurystyce)
    solution = h_node[1] # węzeł o minimalnej heurystyce
    open_nodes.remove(h_node)
    closed_nodes.append(solution)
    neighbors = []
    for x in range(N): # pozycja następnej królowej, w następnym wierszu
        if is_square_free(x, len(solution), solution):
            neighbor = solution + [x]
            if neighbor not in closed_nodes:
                neighbors.append(neighbor)
    open_nodes.extend(heuristics(neighbors))
    if not len(solution) < N:
        return True
    elif dfs(N):
        return True
    elif solution:
        solution.pop()
        return dfs(N)
    else:
        return False


def heuristics(solutions):
    u"""Funkcja oceny (heurystyka): oceń podane rozwiązania.

    Naszą heurystyką będzie ilość pól, które nie są zagrożone szachowaniem.
    Zwraca listę tupli o postaci: (heurystyka rozwiązania, rozwiązanie)

    """
    heuristic = [] # pozycje niezaszachowanych pól
    for i in range(N): # inicjalizacja tablicy [N][len(solution)]
        heuristic.append([False]*N)
    init_free = 0
    for x in range(N): # obliczamy heurystykę wspólną dla wszystkich rozwiązań
        for y in range(N):
            if is_square_free(x, y, solution):
                heuristic[x][y] = True
                init_free += 1
    solutions_with_heuristics = []
    for s in solutions:
        free_squares = init_free
        for x in range(N):
            for y in range(N):
                if heuristic[x][y] and (x == s[-1] or
                        abs(x-s[-1]) == abs(y-len(solution))):
                    free_squares -= 1
        solutions_with_heuristics.append((free_squares, s))
    return solutions_with_heuristics


def is_square_free(x, y, solution):
    u"""Sprawdź czy podane pole nie jest zagrożone szachowaniem."""
    for i, queen in enumerate(solution):
        if x == queen or abs(x-queen) == abs(y-i):
            return False
    return True


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
    try:
        if len(sys.argv) > 1:
            sys.setrecursionlimit(20000)
            N = int(sys.argv[1])
            print_solution(queens_solution_dfs(N, 0))
            print u'Liczba wywołań:', calls
        else:
            print usage
    except ValueError:
        print u'Błąd: parametr musi być liczbą całkowitą.'

