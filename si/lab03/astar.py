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
		self.previous = None

	def __cmp__(self, node):
		return cmp(self.f, node.f)

	def __eq__(self, node):
		return self.name == node.name

	def __hash__(self):
		return hash(self.name)

	def __str__(self):
		description = self.name + ' ->'
		for neighbor, cost in self.neighbors.iteritems():
			description += ' ' + neighbor.name + ' ' + str(cost)
		description += ', h=' + str(self.h)
		if self.previous:
			description += ', prev=' + self.previous.name
		return description


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
		cost = float(line_data[2].strip())
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
	if w0_name not in graph:
		print 'Podanego węzła początkowego nie ma w grafie.'
		sys.exit(1)
	w0 = graph[w0_name]

def a_star(w0, wg):
	'Algorytm A*.'
	open_nodes.append(w0)
	w0.g = .0
	w0.f = w0.h
	while len(open_nodes) != 0:
		node = min(open_nodes)
		if node == wg:
			return path(wg)
		open_nodes.remove(node)
		closed_nodes.append(node)
		for neighbor in node.neighbors:
			if neighbor in closed_nodes:
				continue
			current_g = node.g + node.neighbors[neighbor]
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

def path(node):
	'Zwraca ścieżkę powstałą w wyniku działania algorytmu A*.'
	path = []
	while node.previous:
		path.append(node)
		node = node.previous
	return reversed(path)


if __name__ == '__main__':
	try:
		if len(sys.argv) > 3:
			read_input()
			print 'Graf wejściowy:'
			for node in graph.values():
				print node

			path = a_star(w0, wg)

			if path is not None:
				print '\nNajkrótsza ścieżka z', w0.name, 'do', wg.name + ':'
				path_length = .0
				print w0.name, path_length,
				for node in path:
					path_length += node.previous.neighbors[node]
					print '->', node.name, path_length,
			else:
				print '\nNie znaleziono ścieżki.'
		else:
			print usage
	except IOError:
		print 'Nie można odnaleźć podanego pliku.'
		sys.exit(1)

