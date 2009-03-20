#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from ga import Maze, Coords
import sys


usage = u"""\
Algorytm A*, znajdujący najkrótszą ścieżkę w labiryncie.
Użycie: python a_star.py PLIK_Z_LABIRYNTEM\
"""

graph = {} # klucz: id węzła, wartość: węzeł
open_nodes = [] # węzły do rozwinięcia
closed_nodes = [] # zbadane węzły
w0 = None # węzeł początkowy
wg = None # węzeł końcowy


class Node:
    def __init__(self, id):
        self.id = id
        self.neighbors = {} # klucz: węzeł sąsiadujący, wartość: koszt
        self.previous = None

    def cost(self, neighbor):
        u"""Zwróć koszt przejścia z tego węzła do podanego sąsiada."""
        return self.neighbors[neighbor]

    def __cmp__(self, node):
        return cmp(self.f, node.f)

    def __eq__(self, node):
        return self.id == node.id

    def __hash__(self):
        return hash(self.id)

    def __str__(self):
        description = str(self.id) + ' ->'
        for neighbor, cost in self.neighbors.iteritems():
            description += ' ' + str(neighbor.id) + ' ' + str(cost)
        description += ', h=' + str(self.h)
        if self.previous:
            description += ', prev=' + str(self.previous.id)
        return description


def read_input():
    global w0
    global wg
    maze = Maze(sys.argv[1])
    # tworzymy węzły
    for j, row in enumerate(maze.squares):
        for i, square in enumerate(row):
            if square == 0 or square == 2 or square == 4:
                node_id = (i, j)
                node = Node(node_id)
                graph[node_id] = node
                if square == 2:
                    w0 = node
                elif square == 4:
                    wg = node
    # dodajemy informacje o sąsiadach (krawędziach) i heurezie do węzłów
    for j, row in enumerate(maze.squares):
        for i, square in enumerate(row):
            if square == 0 or square == 2 or square == 4:
                node_id = (i, j)
                node = graph[node_id]
                node_position = Coords(i, j) # współrzędne węzła
                manhattan = lambda p1, p2: abs(p1.x - p2.x) + abs(p1.y - p2.y)
                node.h = manhattan(node_position, Coords(wg.id[0], wg.id[1]))
                if node_position != maze.move(node_position, 'left'):
                    neighbor = graph[(i-1, j)]
                    node.neighbors[neighbor] = 1
                if node_position != maze.move(node_position, 'right'):
                    neighbor = graph[(i+1, j)]
                    node.neighbors[neighbor] = 1
                if node_position != maze.move(node_position, 'up'):
                    neighbor = graph[(i, j-1)]
                    node.neighbors[neighbor] = 1
                if node_position != maze.move(node_position, 'down'):
                    neighbor = graph[(i, j+1)]
                    node.neighbors[neighbor] = 1


def a_star(w0, wg):
    u"""Algorytm A*."""
    open_nodes.append(w0)
    w0.g = .0
    w0.f = w0.h
    while open_nodes:
        node = min(open_nodes)
        if node == wg:
            return reconstruct_path(wg)
        open_nodes.remove(node)
        closed_nodes.append(node)
        for neighbor in node.neighbors:
            if neighbor in closed_nodes:
                continue
            current_g = node.g + node.cost(neighbor)
            is_current_better = False
            if neighbor not in open_nodes:
                open_nodes.append(neighbor)
                is_current_better = True
            elif current_g < neighbor.g:
                is_current_better = True
            if is_current_better:
                neighbor.previous = node
                neighbor.g = current_g
                neighbor.f = neighbor.g + neighbor.h
    return None


def reconstruct_path(node):
    u"""Zwróć ścieżkę powstałą w wyniku działania algorytmu A*."""
    path = []
    while node.previous:
        path.append(node)
        node = node.previous
    return reversed(path)


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            read_input()
            #print u'Graf wejściowy:'
            #for node in graph.values():
            #    print node

            path = a_star(w0, wg)

            print u'Najkrótsza ścieżka od wejścia do wyjścia:'
            if path is not None:
                path_length = .0
                print w0.id, path_length,
                for node in path:
                    path_length += node.previous.cost(node)
                    print '->', node.id, path_length,
            else:
                print u'Nie znaleziono ścieżki.'
        else:
            print usage
    except IOError:
        print u'Nie można odnaleźć podanego pliku.'
        sys.exit(1)

