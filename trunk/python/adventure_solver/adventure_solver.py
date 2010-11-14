#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

"""Solves adventure games given a dependency graph and a map of locations."""


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
        return '%s: %s' % (self.location, self.action)


class Game(object):
    def __init__(self, initial_location, distances):
        self.current_location = initial_location
        self.distances = distances

    def distance_to(self, location):
        if location == self.current_location:
            return 0
        for locations, distance in self.distances:
            if location in locations and self.current_location in locations:
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


def naive_topological_sort(graph, game = None):
    solution = []
    while graph.leaves:
        leaf = graph.leaves.pop()
        solution.append(leaf)
        graph.remove_node(leaf)
    return solution


def solve(graph, game):
    """Generates an efficient solution."""

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
    """Generates an efficient and narratively coherent solution."""

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


def print_solution(solve_function, graph, game):
    print solve_function.__name__ + ':'
    for node in solve_function(graph, game):
        print node


if __name__ == '__main__':

    def solve_larry(solve_function):

        def create_nodes():

            give_apple = Node(NodeValue('look her, give apple', L[4]))
            buy_apple = Node(NodeValue('buy apple', L[3]))
            undress = Node(NodeValue('undress', L[4]))
            push_button = Node(NodeValue('look desk, push button', L[4]))
            give_pills = Node(NodeValue('give pills to her', L[4]))
            get_pills = Node(NodeValue('get pills', L[1]))
            smash_window = Node(NodeValue('smash window', L[1]))
            get_hammer = Node(NodeValue('get hammer', L[1]))
            go_in_trash_bin = Node(NodeValue('go in trash bin', L[1]))
            lean_over = Node(NodeValue('lean over', L[1]))
            get_rope = Node(NodeValue('get rope', L[4]))
            cut_rope = Node(NodeValue('cut rope with knife', L[4]))
            get_in_bed = Node(NodeValue('get in bed', L[4]))
            pour_wine = Node(NodeValue('pour wine', L[4]))
            order_wine = Node(NodeValue('order wine to honeymoon suite', L[2]))
            dial_phone = Node(NodeValue('dial phone 5558039', L[2]))
            turn_on_radio = Node(NodeValue('turn on radio', L[4]))
            knock_suite = Node(NodeValue('knock (door with heart)', L[4]))
            get_married = Node(NodeValue('get married (100$ needed)', L[3]))
            give_money = Node(NodeValue('give money to fawn (100$)', L[2]))
            go_dance = Node(NodeValue('go to dancefloor', L[2]))
            ask_dance = Node(NodeValue('ask fawn for a dance', L[2]))
            give_ring = Node(NodeValue('give ring to fawn', L[2]))
            get_ring = Node(NodeValue('get ring from sink', L[1]))
            give_rose = Node(NodeValue('give rose to fawn', L[2]))
            get_rose = Node(NodeValue('get rose from table', L[1]))
            give_candy = Node(NodeValue('give candy to fawn', L[2]))
            get_candy = Node(NodeValue('get candy', L[1]))
            show_pass = Node(NodeValue('show pass', L[2]))
            get_pass = Node(NodeValue('get pass (from the bin)', L[3]))
            give_wine = Node(NodeValue('give wine to wino', L[2]))
            get_wine = Node(NodeValue('get wine, pay', L[2]))
            read_magazine = Node(NodeValue('read magazine', L[2]))
            get_magazine = Node(NodeValue('get magazine, pay', L[2]))
            tie_rope = Node(NodeValue(
                'tie rope to me, tie rope to balcony', L[1]))
            open_window = Node(NodeValue('open window', L[1]))
            change_channel = Node(NodeValue('change channel 7 times', L[1]))
            use_remote = Node(NodeValue('use remote', L[1]))
            knock_say_password = \
                    Node(NodeValue('knock, say "Ken sent me"', L[1]))
            read_wall = Node(NodeValue('read wall 4 times', L[1]))
            give_whiskey = Node(NodeValue('give him whiskey', L[1]))
            order_whiskey = Node(NodeValue('sit down, order whiskey', L[1]))
            nodes = set()
            def edge(node1, node2):
                add_edge(node1, node2)
                return nodes.union(set([node1, node2]))
            nodes = edge(buy_apple, give_apple)
            nodes = edge(undress, give_apple)
            nodes = edge(push_button, undress)
            nodes = edge(give_pills, push_button)
            nodes = edge(get_pills, give_pills)
            nodes = edge(smash_window, get_pills)
            nodes = edge(get_hammer, smash_window)
            nodes = edge(go_in_trash_bin, get_hammer)
            nodes = edge(lean_over, smash_window)
            nodes = edge(tie_rope, lean_over)
            nodes = edge(get_rope, tie_rope)
            nodes = edge(cut_rope, get_rope)
            nodes = edge(get_in_bed, cut_rope)
            nodes = edge(pour_wine, get_in_bed)
            nodes = edge(order_wine, pour_wine)
            nodes = edge(dial_phone, order_wine)
            nodes = edge(turn_on_radio, dial_phone)
            nodes = edge(knock_suite, turn_on_radio)
            nodes = edge(knock_suite, pour_wine)
            nodes = edge(get_married, knock_suite)
            nodes = edge(give_money, get_married)
            nodes = edge(go_dance, give_money)
            nodes = edge(ask_dance, go_dance)
            nodes = edge(give_rose, give_money)
            nodes = edge(get_rose, give_rose)
            nodes = edge(give_ring, give_money)
            nodes = edge(get_ring, give_ring)
            nodes = edge(give_candy, give_money)
            nodes = edge(get_candy, give_candy)
            nodes = edge(show_pass, give_rose)
            nodes = edge(show_pass, give_ring)
            nodes = edge(show_pass, give_candy)
            nodes = edge(show_pass, ask_dance)
            nodes = edge(get_pass, show_pass)
            nodes = edge(change_channel, get_candy)
            nodes = edge(give_wine, get_in_bed)
            nodes = edge(get_wine, give_wine)
            nodes = edge(read_magazine, tie_rope)
            nodes = edge(get_magazine, read_magazine)
            nodes = edge(open_window, tie_rope)
            nodes = edge(change_channel, open_window)
            nodes = edge(use_remote, change_channel)
            nodes = edge(knock_say_password, use_remote)
            nodes = edge(read_wall, knock_say_password)
            nodes = edge(give_whiskey, use_remote)
            nodes = edge(order_whiskey, give_whiskey)
            return nodes

        distances = set([
            ((L[1], L[2]), 1), ((L[1], L[3]), 2), ((L[1], L[4]), 2),
            ((L[2], L[3]), 1), ((L[2], L[4]), 1), ((L[3], L[4]), 0)])

        graph = Graph(create_nodes())
        game = Game(L[1], distances)
        print_solution(solve_function, graph, game)

    solve_larry(naive_topological_sort)
    print
    solve_larry(solve)
    print
    solve_larry(generate_walkthrough)

