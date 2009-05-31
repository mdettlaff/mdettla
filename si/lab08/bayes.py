#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Przykład zastosowania naiwnego klasyfikatora bayesowskiego."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'


class NBC:
    u"""Naiwny klasyfikator bayesowski."""
    def __init__(self):
        # baza danych obiektów na podstawie których odbywa się klasyfikacja
        self.database = []

    def add_classifiable(self, classifiable):
        u"""Dodaj obiekt do bazy danych.

        Jeśli podany obiekt nie ma z góry określonej klasy do której powinien
        należeć, zostaje przed dodaniem zaklasyfikowany przez klasyfikator.

        :Parameters:
            - `classifiable`: Dodawany obiekt.

        """
        if classifiable.domain is None:
            classifiable.domain = self.classify(classifiable)
        self.database.append(classifiable)

    def classify(self, classifiable):
        u"""Zwróć kategorię do której zaklasyfikowany zostanie dany obiekt."""
        # klasa -> prawdopodobieństwo przynależności obiektu do tej klasy
        probabilities = {}
        all_domains = set([cl.domain for cl in self.database])
        for domain in all_domains:
            classifiables_in_domain = \
                    [cl for cl in self.database if domain == cl.domain]
            # prawdopodobieństwo że obiekt należy do klasy; najpierw bierzemy
            # prawdopodobieństwo a priori
            probability = \
                    float(len(classifiables_in_domain)) / len(self.database)
            for feature in classifiable.features:
                classifiables_having_feature = \
                        [cl for cl in classifiables_in_domain if \
                        feature in cl.features and \
                        classifiable.features[feature] == cl.features[feature]]
                possible_feature_values = set([cl.features[feature] \
                        for cl in self.database if feature in cl.features])
                # obliczamy prawdopodobieństwo dla danej cechy, stosując
                # wygładzanie Laplace'a
                probability *= (1.0 + len(classifiables_having_feature)) \
                        / (len(classifiables_in_domain) + \
                        len(possible_feature_values))
            probabilities[domain] = probability
        print probabilities # DEBUG
        return max(probabilities, key = lambda x: probabilities[x])

    def __str__(self):
        return '\n'.join([unicode(cl) for cl in self.database])


class Classifiable:
    u"""Obiekt, który może zostać poddany klasyfikacji przez klasyfikator."""
    def __init__(self, id, domain, **features):
        u"""Utwórz obiekt do klasyfikacji lub dodania do bazy klasyfikatora.

        :Parameters:
            - `id`: Unikatowy identyfikator obiektu.
            - `domain`: Klasa, do której należy obiekt. Jeśli klasa jest równa
              `None`, obiekt zostanie przydzielony do odpowiedniej klasy przez
              klasyfikator.
            - `features`: Słownik z cechami obiektu i ich wartościami.

        """
        self.id = id
        self.domain = domain
        self.features = features

    def __str__(self):
        s = ''
        s += '%s\t%s\t' % (self.id, self.domain)
        for feature, feature_value in self.features.iteritems():
            s += feature + ': ' + feature_value + ', '
        return s[:-2]


class Tree(Classifiable):
    # długości
    KROTKIE = u'krótkie'
    SREDNIE = u'średnie'
    DLUGIE = u'długie'
    BARDZO_DLUGIE = u'b. długie'

    def __init__(self, id, domain, dlugosc_szpilek_mm, dlugosc_szyszek_mm):
        self.id = id
        self.domain = domain
        self.features = {}
        # podział na kategorie długości szpilek
        if dlugosc_szpilek_mm < 20:
            self.features[u'długość szpilek'] = Tree.KROTKIE
        elif dlugosc_szpilek_mm < 30:
            self.features[u'długość szpilek'] = Tree.SREDNIE
        elif dlugosc_szpilek_mm < 40:
            self.features[u'długość szpilek'] = Tree.DLUGIE
        else:
            self.features[u'długość szpilek'] = Tree.BARDZO_DLUGIE
        # podział na kategorie długości szyszek
        if dlugosc_szyszek_mm < 50:
            self.features[u'długość szyszek'] = Tree.KROTKIE
        elif dlugosc_szyszek_mm < 100:
            self.features[u'długość szyszek'] = Tree.SREDNIE
        else:
            self.features[u'długość szyszek'] = Tree.DLUGIE


def main():
    classifier = NBC()

    classifier.add_classifiable(Tree(1, u'sosna', 33, 40))
    classifier.add_classifiable(Tree(2, u'jodła', 27, 13))
    classifier.add_classifiable(Tree(3, u'sosna', 42, 55))
    classifier.add_classifiable(Tree(4, u'świerk', 11, 120))
    print u'Baza danych o drzewach:\n', unicode(classifier)

    print u'Klasyfikujemy nowe drzewo:'
    new_tree = Tree(5, None, 22, 130)
    print unicode(new_tree)
    classifier.add_classifiable(new_tree)
    print unicode(new_tree)


if __name__ == '__main__':
    main()

