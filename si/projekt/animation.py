#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

u"""Animacja pokazująca działanie algorytmu genetycznego.

Dla kolejnych pokoleń wybierany jest układ klawiatury o najlepszym
przystosowaniu, który następnie wyświetlany jest na ekranie.

"""

from PyQt4 import QtCore, QtGui
import sys

import ga_keyb


usage = u"""Użycie: python animation.py PLIK_TEKSTOWY..."""

WIDTH = 516
HEIGHT = 365


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
        self.current_stats = None
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
            # statystyka
            painter.drawText(5, 187, 'pokolenie ' + str(self.generation_count))
            painter.drawText(5, 210, u'przystosowanie:\t' +
                    str(self.current_layout.fitness))
            painter.drawText(5, 230, u'rzędy klawiszy:\t' + self.stats[0])
            painter.drawText(5, 250, u'palce lewej ręki:\t' +
                    self.stats[1][3:])
            painter.drawText(5, 270, u'palce prawej ręki:\t' +
                    self.stats[2][3:])
            painter.drawText(5, 290, u'użycie rąk:\t\t' + self.stats[3])
            painter.drawText(5, 310, u'alternacja rąk:\t' + self.stats[4])
            painter.drawText(5, 330, u'zmiana palca:\t' + self.stats[5])
            painter.drawText(5, 350, u'ruchy do środka:\t' + self.stats[6])

    def timerEvent(self, event):
        try:
            self.generation_count, self.current_layout = self.generator.next()
            self.stats = ga_keyb.statistics(self.current_layout, corpus). \
                    split('\n')
            self.repaint()
        except StopIteration:
            self.timer.stop()


def genetic_algorithm():
    for i, best in enumerate(
            ga_keyb.epoch(
                ga_keyb.DEFAULT_ITERATIONS,
                ga_keyb.DEFAULT_POPULATION_SIZE,
                ga_keyb.DEFAULT_P_C, # prawdopodobieństwo krzyżowania
                ga_keyb.DEFAULT_P_M, # prawdopodobieństwo mutacji
                ga_keyb.select_tournament,
                (ga_keyb.DEFAULT_TOURNAMENT_SIZE,),
                ga_keyb.fitness,
                (corpus,)
                )
            ):
        yield i+1, best


def main(argv):
    global corpus
    if len(argv) > 1:
        print u'Analiza statystyczna tekstu...'
        corpus = ga_keyb.Corpus(argv[1:],
                ga_keyb.DEFAULT_ENCODING, ga_keyb.DEFAULT_WORDS)

        app = QtGui.QApplication(argv)
        animation = AnimationWindow()
        animation.show()
        sys.exit(app.exec_())
    else:
        print __doc__[:-1]
        print usage


if __name__ == '__main__':
    main(sys.argv)

