#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Program rozwiązujący Sudoku algorytmem Depth-First Search z heurystyką."""

__author__ = u'Michał Dettlaff'

import sys


usage = u"""Użycie: python sudoku.py"""

N = 9
open_nodes = [] # tuple: (heurystyka węzła, węzeł do rozwinięcia)
closed_nodes = [] # zbadane węzły
solutions = []
calls_count = 0 # liczba wywołań funkcji dfs()


class Solution:
    def __init__(self, other=None):
        if other is None:
            self.board = []
            for i in range(N):
                self.board.append(N*[0])
        else:
            self.board = other.board[:]

    def __str__(self):
        s = ''
        for y, row in enumerate(self.board):
            for x, square in enumerate(row):
                s += str(square)
                if x % 3 == 2:
                    s += ' '
            if y % 3 == 2:
                s += '\n'
            s += '\n'
        return s[:-2]

    def is_consistent(self, value, coords):
        if self.board[coords.y][coords.x] != 0:
            return False
        # rząd
        for x in range(N):
            if self.board[coords.y][x] == value:
                return False
        # kolumna
        for y in range(N):
            if self.board[y][coords.x] == value:
                return False
        # kwadrat
        sq = lambda z: 3 * (z / 3)
        for y in range(sq(coords.y), 3 + sq(coords.y)):
            for x in range(sq(coords.x), 3 + sq(coords.x)):
                if self.board[y][x] == value:
                    return False
        return True

    def is_completed(self):
        for row in self.board:
            if 0 in row:
                return False
        return True

    def neighbors(self):
        nbors = []
        for y in range(N):
            for x in range(N):
                for a in range(1, N+1):
                    if self.is_consistent(a, Coords(x, y)):
                        neighbor = Solution(self)
                        neighbor.board[y][x] = a
                        nbors.append(neighbor)
        return nbors


class Coords:
    u"""Przechowuje współrzędne (x, y)."""
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __ne__(self, other):
        return self.x != other.x or self.y != other.y

    def __str__(self):
        return '(' + str(self.x) + ', ' + str(self.y) + ')'


def sudoku_solution_dfs(w0):
    open_nodes.append((0, w0))
    print dfs(N)
    print solutions


def dfs(N):
    u"""Algorytm Depth-First Search przeszukiwania przestrzeni rozwiązań."""
    global solutions
    global calls_count
    calls_count += 1
    if not open_nodes:
        return False
    h_node = min(open_nodes) # tupla: (heurystyka, węzeł o min. heurystyce)
    solutions.append(h_node[1]) # węzeł o minimalnej heurystyce
    open_nodes.remove(h_node)
    closed_nodes.append(solutions[-1])
    print solutions[-1], '\n\n'
    neighbors = []
    open_nodes.extend(heuristics(solutions[-1].neighbors()))
    if solutions[-1].is_completed():
        return True
    elif dfs(N):
        return True
    elif solutions:
        print 'pop'
        solutions.pop()
        return dfs(N)
    else:
        return False


def heuristics(solutions):
    u"""Funkcja oceny (heurystyka): oceń podane rozwiązania."""
    solutions_with_heuristics = []
    for s in solutions:
        solutions_with_heuristics.append((0, s))
    return solutions_with_heuristics


if __name__ == '__main__':
    #sys.setrecursionlimit(20000)
    print sudoku_solution_dfs(Solution())
    #print Solution()
    print u'Liczba wywołań:', calls_count

