#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Naiwny klasyfikator bayesowski."""


class NBC:
    u"""Naiwny klasyfikator bayesowski."""
    def __init__(self):
        # baza danych obiektów na podstawie których odbywa się klasyfikacja
        self.database = []

    def add_classifiable(self, classifiable):
        u"""Dodaj dany obiekt do bazy danych.

        Jeśli podany obiekt nie ma z góry określonej klasy do której powinien
        należeć, zostaje przed dodaniem zaklasyfikowany przez klasyfikator.

        :Parameters:
            - `classifiable`: Dodawany obiekt.

        """
        if classifiable.domain is None:
            classifiable.domain = self.classify(self, classifiable)
        self.database.append(classifiable)

    def classify(self, classifiable):
        u"""Zwróć kategorię do której zaklasyfikowany zostanie dany obiekt."""
        # klasa -> prawdopodobieństwo przynależności obiektu do tej klasy
        probabilities = {}
        all_domains = set()
        for cl in self.database:
            all_domains.add(cl.domain)
        for domain in all_domains:
            # tworzymy listę obiektów należących do danej klasy
            classifiables_in_domain = []
            for cl in self.database:
                if domain == cl.domain:
                    classifiables_in_domain.append(cl)
            # prawdopodobieństwo że obiekt należy do klasy; najpierw bierzemy
            # prawdopodobieństwo a priori
            probability = float(len(classifiables_in_domain)) \
                    / len(self.database)
            for feature in classifiable.features:
                # obliczamy ilość obiektów tej klasy mających daną cechę
                classifiables_having_feature_count = 0
                for cl in classifiables_in_domain:
                    if classifiable.features[feature] == cl.features[feature]:
                        classifiables_having_feature_count += 1
                # obliczamy ilość możliwych wartości cechy
                possible_feature_values = set()
                for cl in self.database:
                    possible_feature_values.add(cl.features[feature])
                # obliczamy zgodnie ze wzorem na prawdopodobieństwo dla
                # danej cechy
                probability *= (1.0 + classifiables_having_feature_count) \
                        / (len(classifiables_in_domain) + \
                        len(possible_feature_values))
            probabilities[domain] = probability
        print probabilities # DEBUG
        return max(probabilities, key = lambda x: probabilities[x])


class Classifiable:
    u"""Obiekt, który może zostać poddany klasyfikacji przez klasyfikator."""
    def __init__(self, domain = None, **kwargs):
        self.domain = domain
        self.features = kwargs


class Dog(Classifiable):
    # długość ogona
    VERY_SHORT = 0
    SHORT = 1
    MEDIUM = 2
    LONG = 3
    VERY_LONG = 4
    # kolor nosa
    BLACK = 0
    PINK = 1


def main():
    classifier = NBC()

    classifier.add_classifiable(Dog('spaniel',
        tail_len = Dog.MEDIUM, nose_color = Dog.BLACK))
    classifier.add_classifiable(Dog('golden retriever',
        tail_len = Dog.LONG, nose_color = Dog.BLACK))

    print classifier.classify(Dog(
        tail_len = Dog.MEDIUM, nose_color = Dog.PINK))


if __name__ == '__main__':
    main()

