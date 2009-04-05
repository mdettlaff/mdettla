#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Program rozwiązujący Sudoku algorytmem Depth-First Search z heurystyką."""

__author__ = u'Michał Dettlaff'

import math
import random
import sys


usage = u"""Użycie: python sudoku.py"""

N = 9
UNIT_SIZE = int(math.sqrt(N)) # rozmiar dużego kwadratu

open_nodes = [] # tuple: (heurystyka węzła, węzeł do rozwinięcia)
closed_nodes = [] # zbadane węzły
solution = [] # kolejne cyfry dodawane na planszę
calls_count = 0 # liczba wywołań funkcji dfs()


def print_solution(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for i, num in enumerate(solution):
        x = i % N
        y = i / N
        board[y][x] = num
    s = ''
    for y, row in enumerate(board):
        for x, square in enumerate(row):
            s += str(square)
            if x % UNIT_SIZE == 2:
                s += ' '
        if y % UNIT_SIZE == 2:
            s += '\n'
        s += '\n'
    print s[:-1]

def is_consistent(board, value, solution):
    if len(solution) == N*N:
        return True
    value_x = len(solution) % N
    value_y = len(solution) / N
    if board[value_y][value_x] != 0:
        return False
    # rząd
    for x in range(N):
        if board[value_y][x] == value:
            return False
    # kolumna
    for y in range(N):
        if board[y][value_x] == value:
            return False
    # kwadrat
    sq = lambda z: UNIT_SIZE * (z / UNIT_SIZE)
    for y in range(sq(value_y), UNIT_SIZE + sq(value_y)):
        for x in range(sq(value_x), UNIT_SIZE + sq(value_x)):
            if board[y][x] == value:
                return False
    return True

def is_completed(solution):
    return len(solution) == N*N

def neighbors(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for i, num in enumerate(solution):
        x = i % N
        y = i / N
        board[y][x] = num
    nbors = []
    for a in range(1, N+1):
        if is_consistent(board, a, solution):
            neighbor = solution + [a]
            nbors.append(neighbor)
    return nbors


def sudoku_solution_dfs(w0):
    open_nodes.append((0, [w0]))
    if dfs(N):
        return solution
    else:
        return None


def dfs(N):
    u"""Algorytm Depth-First Search przeszukiwania przestrzeni rozwiązań."""
    global solution
    global calls_count
    calls_count += 1
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
        else:
            'closed!'
    open_nodes.extend(heuristics(nbors))
    if is_completed(solution):
        return True
    elif dfs(N):
        return True
    elif solution:
        solution.pop()
        return dfs(N)
    else:
        return False


def heuristics(solutions):
    u"""Funkcja oceny (heurystyka): oceń podane rozwiązania."""
    # TODO
    solutions_with_heuristics = []
    for s in solutions:
        solutions_with_heuristics.append((0, s))
    return solutions_with_heuristics


if __name__ == '__main__':
    sudoku_solution_dfs(random.randint(1, N))
    print_solution(solution)
    print u'Liczba wywołań Depth-First Search:', calls_count

