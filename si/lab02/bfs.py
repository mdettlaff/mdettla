#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import re
import sys
from collections import deque


usage = u"""\
Algorytm Breadth First Search, znajdujący najkrótszą ścieżkę pomiędzy danymi
wierzchołkami grafu.
Użycie: python bfs.py PLIK_GRAFU\
"""

graph = {} # klucz: id węzła, wartość: węzeł
open_nodes = deque() # węzły do rozwinięcia
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


def find_path_bfs(visit):
    u"""Znajdź najkrótszą ścieżkę przy pomocy algorytmu BFS."""
    w0.f = 0
    open_nodes.append(w0)
    return bfs(visit)


def bfs(visit):
    u"""Algorytm Breadth First Search szukania najkrótszej ścieżki w grafie.
    
    visit - Funkcja wykonywana podczas przechodzenia grafu.

    """
    while open_nodes:
        node = open_nodes.popleft()
        visit(node)
        if node in closed_nodes:
            continue
        closed_nodes.add(node)
        path.append(node)
        # dodajemy do open_nodes sąsiadów posortowanych wg kosztu
        neighbors = []
        for neighbor_id in path[-1].neighbors:
            neighbor = graph[neighbor_id]
            neighbor.f = path[-1].f + node.cost(neighbor.id)
            if neighbor not in open_nodes:
                neighbors.append(neighbor)
        neighbors.sort()
        open_nodes.extend(neighbors)
        if path[-1] == wg:
            return True


def print_node(node):
    print node.id,


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            read_input()
            print u'Graf wejściowy:'
            for node in graph.values():
                print node

            print u'\nKolejność przeszukiwania węzłów:'
            if find_path_bfs(print_node):
                print u'\n\nNajkrótsza ścieżka z %d do %d:' % (w0.id, wg.id)
                for node in path:
                    print '-> %d(%d)' % (node.id, node.f),
            else:
                print u'\nNie znaleziono ścieżki.'
        else:
            print usage
    except IOError:
        print u'Nie można odnaleźć podanego pliku.'
        sys.exit(1)

