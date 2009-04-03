#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Animacja pokazująca działanie algorytmu genetycznego.

Dla kolejnych pokoleń wybierany jest układ klawiatury o najlepszym
przystosowaniu, który następnie wyświetlany jest na ekranie.

"""

from PyQt4 import QtCore, QtGui
import sys
import threading

import ga_keyb


usage = u"""Użycie: python animation.py PLIK_TEKSTOWY..."""

WIDTH = 516
HEIGHT = 196
iterations = ga_keyb.DEFAULT_ITERATIONS
population_size = ga_keyb.DEFAULT_POPULATION_SIZE
tournament_size = ga_keyb.DEFAULT_TOURNAMENT_SIZE
p_c = ga_keyb.DEFAULT_P_C # prawdopodobieństwo krzyżowania
p_m = ga_keyb.DEFAULT_P_M # prawdopodobieństwo mutacji
weights = ga_keyb.DEFAULT_WEIGHTS
words = ga_keyb.DEFAULT_WORDS
encoding = ga_keyb.DEFAULT_ENCODING


class AnimationWindow(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, WIDTH, HEIGHT)
        self.setWindowTitle('Algorytm genetyczny - animacja')
        self.canvas = KeyboardCanvas(self) # obszar rysowania
        self.setCentralWidget(self.canvas)
        self.canvas.start()
        self.center()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2,
                (screen.height()-size.height())/2)


class KeyboardCanvas(QtGui.QFrame):
    u"""Obszar na którym będziemy rysować klawiaturę."""
    def __init__(self, parent):
        QtGui.QFrame.__init__(self, parent)
        self.timer = QtCore.QBasicTimer()
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.keyboard_image = QtGui.QImage('keyboard.png')
        self.current_layout = None
        self.generator = genetic_algorithm()

    def start(self):
        self.timer.start(0, self)

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        black = QtGui.QColor(0x000000)
        white = QtGui.QColor(0xFFFFFF)

        painter.fillRect(0, 0, WIDTH, HEIGHT, white) # tło
        painter.setPen(black)
        painter.drawImage(4, 4, self.keyboard_image)
        if self.current_layout:
            left_margins = [62, 75, 95]
            top_margins = [53, 86, 120]
            for i, row in enumerate(self.current_layout.phenotype):
                x = left_margins[i]
                y = top_margins[i]
                for key in row:
                    painter.drawText(x, y, key.upper())
                    x += 34
            painter.drawText(5, 186, 'Pokolenie ' + str(self.generation_count)
                    + ', przystosowanie ' + str(self.current_layout.fitness))

    def timerEvent(self, event):
        try:
            self.generation_count, self.current_layout = self.generator.next()
            self.repaint()
        except StopIteration:
            self.timer.stop()


def genetic_algorithm():
    for i, best in enumerate(ga_keyb.epoch(iterations, population_size,
        p_c, p_m, ga_keyb.select_tournament, [tournament_size],
        ga_keyb.fitness, [corpus] + weights)):
        yield (i+1, best)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        print u'Analiza statystyczna tekstu...'
        corpus = ga_keyb.Corpus(sys.argv[1:], encoding, words)

        app = QtGui.QApplication(sys.argv)
        animation = AnimationWindow()
        animation.show()
        sys.exit(app.exec_())
    else:
        print __doc__[:-1]
        print usage

