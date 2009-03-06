#!/usr/bin/env python
# -*- coding: utf8 -*-

import sys

usage = '''\
Algorytm A*, znajdujący najkrótszą ścieżkę pomiędzy danymi wierzchołkami grafu.
Użycie: python astar.py PLIK_GRAFU PLIK_HEUREZY WĘZEŁ_POCZĄTKOWY\
'''
# węzeł końcowy to węzeł o heurezie równiej 0

graph = {} # klucz: nazwa węzła, wartość: węzeł
open_nodes = [] # węzły do rozwinięcia
closed_nodes = [] # zbadane węzły
w0 = None
wg = None

class Node:
	def __init__(self, name):
		self.name = name
		self.neighbors = {} # klucz: węzeł sąsiadujący, wartość: koszt

	def __str__(self):
		description = self.name + ' ->'
		for neighbor, cost in self.neighbors.iteritems():
			description += ' ' + neighbor.name + ' ' + cost
		description += ', h=' + str(self.h)
		return description

	def __cmp__(self, node):
		return cmp(self.name, node.name)
	
	def __hash__(self):
		return hash(self.name)


def read_input():
	global w0
	global wg
	graph_filename = sys.argv[1]
	h_filename = sys.argv[2]
	w0_name = sys.argv[3]

	# tworzymy węzły
	graph_file = open(graph_filename)
	for line in graph_file.readlines():
		line_data = line.split(' ')
		node_name = line_data[0]
		node = Node(node_name)
		if not node_name in graph:
			graph[node_name] = Node(node_name)
	graph_file.close()
	# dodajemy informacje o sąsiadach (krawędziach) do węzłów
	graph_file = open(graph_filename)
	for line in graph_file.readlines():
		line_data = line.split(' ')
		node_name = line_data[0]
		neighbor_name = line_data[1]
		neighbor = graph[neighbor_name]
		cost = line_data[2].strip()
		graph[node_name].neighbors[neighbor] = cost
	graph_file.close()
	# dodajemy informacje o heurezie do węzłów
	h_file = open(h_filename)
	for line in h_file.readlines():
		line_data = line.split(' ')
		node_name = line_data[0]
		h = float(line_data[1].strip())
		graph[node_name].h = h
		if h == 0:
			wg = graph[node_name]
	h_file.close()
	w0 = graph[w0_name]


if __name__ == '__main__':
	try:
		if len(sys.argv) > 3:
			read_input()
			print 'Graf wejściowy:'
			for node in graph.values():
				print node
			print 'Idziemy z', w0.name, 'do', wg.name
		else:
			print usage
	except IOError:
		print 'Nie można odnaleźć podanego pliku.'

