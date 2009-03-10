#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import re
import sys


usage = u"""\
Algorytm Depth First Search, znajdujący najkrótszą ścieżkę pomiędzy danymi
wierzchołkami grafu.
Użycie: python dfs.py PLIK_GRAFU\
"""

graph = {} # klucz: id węzła, wartość: węzeł
open_nodes = set() # węzły do rozwinięcia
closed_nodes = set() # zbadane węzły
path = []
w0 = None # węzeł początkowy
wg = None # węzeł końcowy


class Node:
    def __init__(self, id):
        self.id = id
        self.neighbors = {} # klucz: id sąsiada, wartość: koszt

    def cost(self, neighbor):
        u"""Zwróć koszt przejścia z tego węzła do podanego sąsiada."""
        return self.neighbors[neighbor]

    def __cmp__(self, node):
        u"""Funkcja kosztu."""
        return cmp(self.f, node.f)

    def __eq__(self, node):
        return self.id == node.id

    def __hash__(self):
        return self.id

    def __str__(self):
        description = str(self.id) + ' ->'
        for neighbor, cost in self.neighbors.iteritems():
            description += ' %d(%d)' % (neighbor, cost)
        return description


def read_input():
    global w0
    global wg
    graph_filename = sys.argv[1]
    graph_file = open(graph_filename)
    lines = graph_file.readlines()
    # dodajemy węzły
    for node_id, line in enumerate(lines):
        if line == '\n':
            break
        line_data = re.split(' +', line)
        node = Node(node_id)
        # dodajemy sąsiadów węzła i ich koszty
        for j, cost in enumerate(line_data):
            cost = int(cost)
            if cost >= 0:
                node.neighbors[j] = cost
        graph[node_id] = node
    last_line = re.split(' +', lines[-1])
    graph_file.close()
    w0 = graph[int(last_line[0])]
    wg = graph[int(last_line[1])]


def dfs():
    u"""Algorytm Depth First Search szukania najkrótszej ścieżki."""
    if not open_nodes:
        return False
    node = min(open_nodes)
    open_nodes.remove(node)
    closed_nodes.add(node)
    path.append(node)
    for neighbor_id in path[-1].neighbors:
        neighbor = graph[neighbor_id]
        if neighbor not in closed_nodes:
            neighbor.f = path[-1].f + node.cost(neighbor.id)
            open_nodes.add(neighbor)
    if path[-1] == wg:
        return True
    else:
        if dfs():
            return True
        else:
            path.pop()
            return dfs()


def find_path_dfs():
    u"""Znajdź najkrótszą ścieżkę przy pomocy algorytmu DFS."""
    w0.f = 0
    open_nodes.add(w0)
    return dfs()


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            read_input()
            print u'Graf wejściowy:'
            for node in graph.values():
                print node

            print u'\nNajkrótsza ścieżka z %d do %d:' % (w0.id, wg.id)
            if find_path_dfs():
                for node in path:
                    print '-> %d(%d)' % (node.id, node.f),
            else:
                print u'Nie znaleziono ścieżki.'
        else:
            print usage
    except IOError:
        print u'Nie można odnaleźć podanego pliku.'
        sys.exit(1)

