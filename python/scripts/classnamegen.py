#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import random

adjectives = [
        ('Abstract', -5), ('Transactional', 0), ('Delegating', 0),
        ('ContextAware', 0), ('Configurable', 0), ('Hierarchical', 0),
        ('Observable', 0), ('Intercepting', 0), ('Stateless', 0),
        ('Stateful', 0), ('Managed', 0), ('Mock', 5), ('Parametrized', 0),
        ('Smart', -4), ('Remote', 0), ('Generic', -4), ('Embedded', 0),
        ('Dynamic', 0), ('Default', -3), ('Customizable', 0)
        ]
verbs = [
        ('Manager', 2), ('Context', 0), ('Proxy', 0), ('Service', 0),
        ('Factory', 0), ('Impl', 5), ('Bean', 0), ('Injector', 0),
        ('Controller', 0), ('Handler', 0), ('Locator', 3), ('Holder', 4),
        ('Exception', 4), ('Transaction', 0), ('Template', 0), ('Adapter', 0),
        ('Builder', 0), ('Prototype', 0), ('Pool', 0), ('Decorator', 0),
        ('Facade', 1), ('Wrapper', 0), ('Filter', 0), ('Observer', 0),
        ('Strategy', 0), ('Listener', 0), ('Session', 0), ('Cake', 0),
        ('Placeholder', 0), ('Configurer', 2), ('Loader', 2), ('Converter', 2),
        ('Dispatcher', 1), ('Resolver', 1)
        ]

def select_words(words, max_words):
    words = words[:]
    sorted_words = lambda words: sorted(words, key = lambda x: x[1])
    random.shuffle(words)
    selected_words_tuples = sorted_words(words[0:random.randint(1, max_words)])
    selected_words = map(lambda x: x[0], selected_words_tuples)
    return ''.join(selected_words)

print select_words(adjectives, 3) + select_words(verbs, 6)
