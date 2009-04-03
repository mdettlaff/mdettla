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
HEIGHT = 200


class AnimationWindow(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, WIDTH, HEIGHT)
        self.setWindowTitle('Algorytm genetyczny - animacja')
        self.canvas = KeyboardCanvas(self) # obszar rysowania
        self.setCentralWidget(self.canvas)
        self.statusbar = self.statusBar()
        self.connect(self.canvas,
                QtCore.SIGNAL('messageToStatusbar(QString)'), self.statusbar,
                QtCore.SLOT('showMessage(QString)'))
        self.center()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2,
                (screen.height()-size.height())/2)


class KeyboardCanvas(QtGui.QFrame):
    u"""Obszar na którym będziemy rysować klawiaturę."""
    key_size = 30

    def __init__(self, parent):
        QtGui.QFrame.__init__(self, parent)
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.current_layout = None

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        black = QtGui.QColor(0x000000)
        white = QtGui.QColor(0xFFFFFF)

        self.drawLayout(painter, white, black)

    def nextGeneration(self, generation_count, keyboard_layout):
        self.current_layout = keyboard_layout
        self.update()
        self.emit(QtCore.SIGNAL('messageToStatusbar(QString)'),
                'Pokolenie ' + str(generation_count) +
                ', przystosowanie ' + str(self.current_layout.fitness))

    def drawLayout(self, painter, bg_color, color):
        painter.fillRect(0, 0, WIDTH, HEIGHT, bg_color) # tło
        painter.setPen(color)
        painter.drawImage(4, 4, QtGui.QImage('keyboard.png'))
        if self.current_layout:
            painter.drawText(60, 52, self.current_layout.genotype[0].upper())


class GeneticAlgorithmThread(threading.Thread):
    def run(self):
        corpus = ga_keyb.Corpus(sys.argv[1:], 'UTF-8', 128)
        #results = []
        for i, best in enumerate(ga_keyb.epoch(16, 100, .7, .7,
            ga_keyb.select_tournament, [4], ga_keyb.fitness,
            [corpus] + [1, 1, 1, 1])):
            #results.append(best)
            animation.canvas.nextGeneration(i+1, best)
            print '%d\t%d' % (i+1, best.fitness)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        app = QtGui.QApplication(sys.argv)
        animation = AnimationWindow()
        animation.show()

        GeneticAlgorithmThread().start()

        sys.exit(app.exec_())
    else:
        print __doc__[:-1]
        print usage

