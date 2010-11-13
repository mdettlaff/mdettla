#!/usr/bin/env python
# -*- encoding: UTF-8 -*-


# locations
L = [None, 'bar', 'store/disco', 'casino', 'hotel']


class Graph(object):
    def __init__(self, nodes):
        self.nodes = nodes

    def remove_node(self, node):
        for dependent in set(node.dependents):
            remove_edge(node, dependent)
        for dependency in set(node.dependencies):
            remove_edge(dependency, node)
        self.nodes.remove(node)

    def __get_leaves(self):
        return set(filter(lambda node: node.is_leaf, self.nodes))

    leaves = property(__get_leaves)


class Node(object):
    def __init__(self, value):
        self.value = value
        self.dependents = set()
        self.dependencies = set()

    def __is_leaf(self):
        return not self.dependencies

    def __str__(self):
        return str(self.value)

    is_leaf = property(__is_leaf)


class NodeValue(object):
    def __init__(self, action, location):
        self.action = action
        self.location = location

    def __str__(self):
        return '%s {%s}' % (self.action, self.location)


class Game(object):
    def __init__(self, location, distances):
        self.location = location
        self.distances = distances

    def distance_to(self, location):
        for locations, distance in self.distances:
            if location in locations and self.location in locations:
                return distance
        raise Exception('unknown location: ' + location)


def add_edge(node1, node2):
    node1.dependents.add(node2)
    node2.dependencies.add(node1)


def remove_edge(node1, node2):
    node1.dependents.remove(node2)
    node2.dependencies.remove(node1)


def min_cost_node(nodes, game):
    cost = lambda node: game.distance_to(node.value.location)
    return min(nodes, key = cost)


def solve_naive(graph, game):
    solution = []
    while graph.leaves:
        leaf = min_cost_node(graph.leaves, game)
        game.location = leaf.value.location
        solution.append(leaf)
        dependents = set(leaf.dependents)
        graph.remove_node(leaf)
    return solution


def solve(graph, game):
    solution = []
    leaves = graph.leaves
    while leaves:
        leaf = min_cost_node(leaves, game)
        game.location = leaf.value.location
        solution.append(leaf)
        leaves.remove(leaf)
        dependents = set(leaf.dependents)
        for dependent in dependents:
            remove_edge(leaf, dependent)
        dependents_leaves = set(filter(lambda node: node.is_leaf, dependents))
        leaves = leaves.union(dependents_leaves)
    return solution


def generate_walkthrough(graph, game):
    walkthrough = []
    leaves = graph.leaves
    leaf = min_cost_node(leaves, game)
    while leaves:
        game.location = leaf.value.location
        walkthrough.append(leaf)
        leaves.remove(leaf)
        dependents = set(leaf.dependents)
        for dependent in dependents:
            remove_edge(leaf, dependent)
        dependents_leaves = set(filter(lambda node: node.is_leaf, dependents))
        leaves = leaves.union(dependents_leaves)
        if dependents_leaves:
            leaf = min_cost_node(dependents_leaves, game)
        elif leaves:
            leaf = min_cost_node(leaves, game)
    return walkthrough


def solve_game(solve_function, graph, game):
    print solve_function.__name__ + ':'
    for node in solve_function(graph, game):
        print node


if __name__ == '__main__':

    def solve_larry(solve_function):

        def create_nodes():
            change_channel = Node(NodeValue('change channel 7 times', L[1]))
            use_remote = Node(NodeValue('use remote', L[1]))
            knock_say_password = \
                    Node(NodeValue('knock, say "Ken sent me"', L[1]))
            read_wall = Node(NodeValue('read wall 4 times', L[1]))
            give_whiskey = Node(NodeValue('give him whiskey', L[1]))
            order_whiskey = Node(NodeValue('sit down, order whiskey', L[1]))
            add_edge(order_whiskey, give_whiskey)
            add_edge(give_whiskey, use_remote)
            add_edge(read_wall, knock_say_password)
            add_edge(knock_say_password, use_remote)
            add_edge(use_remote, change_channel)
            return set([order_whiskey, give_whiskey, read_wall,
                    knock_say_password, use_remote, change_channel])

        distances = set([
            ((L[1], L[2]), 1), ((L[1], L[3]), 2), ((L[1], L[4]), 2),
            ((L[2], L[3]), 1), ((L[2], L[4]), 1), ((L[3], L[4]), 0)])

        graph = Graph(create_nodes())
        game = Game(L[1], distances)
        solve_game(solve_function, graph, game)

    solve_larry(solve_naive)
    print
    solve_larry(solve)
    print
    solve_larry(generate_walkthrough)

