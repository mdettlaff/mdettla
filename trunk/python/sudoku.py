#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Program generujący Sudoku algorytmem Depth-First Search."""

__author__ = u'Michał Dettlaff'

import math
import random


N = 9
UNIT_SIZE = int(math.sqrt(N)) # rozmiar dużego kwadratu

open_nodes = [] # tuple: (heurystyka węzła, węzeł do rozwinięcia)
closed_nodes = [] # zbadane węzły
solution = [] # kolejne cyfry dodawane na planszę
calls_count = 0 # liczba wywołań funkcji dfs()


def get_board(solution):
    board = []
    for i in range(N):
        board.append(N*[0])
    for i, num in enumerate(solution):
        x = i % N
        y = i / N
        board[y][x] = num
    return board


def is_consistent(board, value, value_x, value_y):
    if board[N-1][N-1] != 0:
        return True
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


def get_neighbors(solution):
    board = get_board(solution)
    neighbors = []
    for a in range(1, N+1):
        if is_consistent(board, a, len(solution) % N, len(solution) / N):
            neighbor = solution + [a]
            neighbors.append(neighbor)
    return neighbors


def print_solution(solution):
    board = get_board(solution)
    s = ''
    for y, row in enumerate(board):
        for x, square in enumerate(row):
            s += str(square)
            if x % UNIT_SIZE == UNIT_SIZE - 1:
                s += ' '
        if y % UNIT_SIZE == UNIT_SIZE - 1:
            s += '\n'
        s += '\n'
    print s[:-1]


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
    h_node = open_nodes[-1]
    solution = h_node[1] # węzeł o minimalnej heurystyce
    open_nodes.remove(h_node)
    closed_nodes.append(solution)
    neighbors = []
    for neighbor in get_neighbors(solution):
        if neighbor not in closed_nodes:
            neighbors.append(neighbor)
    open_nodes.extend([(0, n) for n in neighbors])
    if len(solution) == N*N: # znaleziono rozwiązanie
        return True
    elif dfs(N):
        return True
    elif solution:
        solution.pop()
        return dfs(N)
    else:
        return False


def main():
    if sudoku_solution_dfs(random.randint(1, N)) is not None:
        print_solution(solution)
        print u'Liczba wywołań Depth-First Search:', calls_count
    else:
        print u'Nie znaleziono rozwiązania.'


if __name__ == '__main__':
    main()

