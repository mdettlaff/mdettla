#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Program rozwiązujący Sudoku algorytmem Depth-First Search z heurystyką."""

__author__ = u'Michał Dettlaff'

import sys


usage = u"""Użycie: python sudoku.py"""

N = 4
open_nodes = [] # tuple: (heurystyka węzła, węzeł do rozwinięcia)
closed_nodes = [] # zbadane węzły
solution = [] # kolejne liczby dodawane na planszę wraz z pozycjami
calls_count = 0 # liczba wywołań funkcji dfs()


def print_solution(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for position in solution:
        board[position[0].y][position[0].x] = position[1]
    s = ''
    for y, row in enumerate(board):
        for x, square in enumerate(row):
            s += str(square)
            #if x % 3 == 2:
            #    s += ' '
        #if y % 3 == 2:
        #    s += '\n'
        s += '\n'
    print s[:-2]

def is_consistent(board, value, coords):
    if board[coords.y][coords.x] != 0:
        return False
    # rząd
    for x in range(N):
        if board[coords.y][x] == value:
            return False
    # kolumna
    for y in range(N):
        if board[y][coords.x] == value:
            return False
    # kwadrat
    #sq = lambda z: 3 * (z / 3)
    #for y in range(sq(coords.y), 3 + sq(coords.y)):
    #    for x in range(sq(coords.x), 3 + sq(coords.x)):
    #        if board[y][x] == value:
    #            return False
    return True

def is_completed(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for position in solution:
        #print position
        board[position[0].y][position[0].x] = position[1]
    for row in board:
        if 0 in row:
            return False
    return True

def neighbors(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for position in solution:
        #print position
        board[position[0].y][position[0].x] = position[1]
    nbors = []
    for y in range(N):
        for x in range(N):
            for a in range(1, N+1):
                if is_consistent(board, a, Coords(x, y)):
                    neighbor = solution + [(Coords(x, y), a)]
                    nbors.append(neighbor)
    return nbors


class Coords:
    u"""Przechowuje współrzędne (x, y)."""
    def __init__(self, x, y):
        self.x = x
        self.y = y

    #def __eq__(self, other):
    #    return self.x == other.x and self.y == other.y

    def __ne__(self, other):
        return self.x != other.x or self.y != other.y

    def __str__(self):
        return '(' + str(self.x) + ', ' + str(self.y) + ')'


def sudoku_solution_dfs(w0):
    open_nodes.append((0, [w0]))
    print dfs(N)
    print solution
    print_solution(solution)


def dfs(N):
    u"""Algorytm Depth-First Search przeszukiwania przestrzeni rozwiązań."""
    global solution
    global calls_count
    calls_count += 1
    print len(open_nodes), len(solution)
    print_solution(solution)
    print
    if not open_nodes:
        return False
    #h_node = min(open_nodes) # tupla: (heurystyka, węzeł o min. heurystyce)
    h_node = open_nodes[-1]
    solution = h_node[1] # węzeł o minimalnej heurystyce
    open_nodes.remove(h_node)
    closed_nodes.append(solution)
    nbors = []
    for neighbor in neighbors(solution):
        if neighbor not in closed_nodes:
            nbors.append(neighbor)
    open_nodes.extend(heuristics(nbors))
    if is_completed(solution):
        return True
    elif dfs(N):
        return True
    elif solution:
        print 'pop'
        solution.pop()
        return dfs(N)
    else:
        return False


def heuristics(solutions):
    u"""Funkcja oceny (heurystyka): oceń podane rozwiązania."""
    solutions_with_heuristics = []
    for s in solutions:
        import random
        solutions_with_heuristics.append((random.randint(0, 100), s))
    return solutions_with_heuristics


if __name__ == '__main__':
    sys.setrecursionlimit(20000)
    sudoku_solution_dfs((Coords(2, 2), 1))
    #print Solution()
    print u'Liczba wywołań:', calls_count

