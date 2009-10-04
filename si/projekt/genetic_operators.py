# -*- encoding: UTF-8 -*-

u"""Operatory genetyczne."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import math
import random


# OPERATORY KRZYŻOWANIA =======================================================
#
# Operatory krzyżowania są to funkcje, które biorą jako argument parę rodziców
# posiadających atrybut 'genotype' (genotyp) i zwracają tuplę zawierającą
# genotypy jednego lub dwojga potomków. Genotyp to lista genów osobnika.

def cx_crossover(parents):
    u"""Cycle Crossover."""
    genotype_len = len(parents[0].genotype)
    offspring_genotype = [None for i in range(genotype_len)]
    # geny, których jeszcze nie skopiowaliśmy do genotypu potomka
    unused = set(parents[0].genotype)
    random_order = range(0, genotype_len)
    random.shuffle(random_order)
    for i in random_order:
        parent = random.randint(0, 1)
        if parents[parent].genotype[i] in unused:
            gene = parents[parent].genotype[i]
            offspring_genotype[i] = gene
            unused.remove(gene)
        elif parents[1 - parent].genotype[i] in unused:
            gene = parents[1 - parent].genotype[i]
            offspring_genotype[i] = gene
            unused.remove(gene)
        else:
            # bierzemy arbitralnie wybrany niepowtarzający się gen
            offspring_genotype[i] = unused.pop()
    return offspring_genotype,

def pmx_crossover(parents):
    u"""Partially-Mapped Crossover."""
    genotype_len = len(parents[0].genotype)

    r1 = random.randrange(genotype_len)
    r2 = random.randrange(genotype_len)
    cut_point_left = min(r1, r2)
    cut_point_right = max(r1, r2)

    def crossover_from(parents, parent_index):
        u"""Tworzy nowy genotyp potomka na podstawie zadanego rodzica."""
        offspring_genotype = [None for i in range(genotype_len)]
        # kopiujemy odcinek genotypu drugiego rodzica do genotypu potomka
        offspring_genotype[cut_point_left:cut_point_right + 1] = \
                parents[1 - parent_index]. \
                genotype[cut_point_left:cut_point_right + 1]
        # ustawiamy mapowania genów
        mappings1 = {}
        mappings2 = {}
        for i in range(cut_point_left, cut_point_right + 1):
            mappings1[parents[0].genotype[i]] = parents[1].genotype[i]
            mappings2[parents[1].genotype[i]] = parents[0].genotype[i]
        # kopiujemy pozostałe geny od danego rodzica do potomka, stosując
        # mapowania w wypadku wystąpienia powtórzeń
        for i in range(cut_point_left) + \
                range(cut_point_right + 1, genotype_len):
            gene = parents[parent_index].genotype[i]
            while gene in offspring_genotype:
                if gene in mappings1:
                    gene = mappings1[gene]
                    del mappings2[gene]
                elif gene in mappings2:
                    gene = mappings2[gene]
                    del mappings1[gene]
            offspring_genotype[i] = gene
        return offspring_genotype

    return crossover_from(parents, 0), crossover_from(parents, 1)

def pos_crossover(parents):
    u"""Position Based Crossover."""
    genotype_len = len(parents[0].genotype)

    positions_count = random.randrange(1, genotype_len)
    positions = random.sample(range(0, genotype_len), positions_count)

    def crossover_from(parents, parent_index):
        u"""Tworzy nowy genotyp potomka na podstawie zadanego rodzica."""
        offspring_genotype = [None for i in range(genotype_len)]
        # kopiujemy geny drugiego rodzica z danych pozycji do genotypu potomka
        for i in positions:
            offspring_genotype[i] = parents[1 - parent_index].genotype[i]
        # ustalamy geny na pozostałych pozycjach, na podstawie danego rodzica
        parent_iterator = iter(parents[parent_index].genotype)
        for i in set(range(genotype_len)).difference(positions):
            gene = parent_iterator.next()
            while gene in offspring_genotype:
                gene = parent_iterator.next()
            offspring_genotype[i] = gene
        return offspring_genotype

    return crossover_from(parents, 0), crossover_from(parents, 1)


# OPERATORY MUTACJI ===========================================================
#
# Operatory mutacji są to funkcje, które biorą jako argument genotyp (listę
# genów) i modyfikują go.

def swap_mutate(genotype):
    u"""Mutacja przez zamianę genów miejscami."""
    mut_count = int(math.floor(1 / random.uniform(.1, 1)))
    # pozycje, na których wystąpią mutacje
    mp = random.sample(range(0, len(genotype)), mut_count * 2)
    for i in range(0, len(mp), 2):
        genotype[mp[i]], genotype[mp[i + 1]] = \
                genotype[mp[i + 1]], genotype[mp[i]]

