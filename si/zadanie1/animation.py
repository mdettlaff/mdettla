#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from PyQt4 import QtCore, QtGui
import ga
import sys
import random


usage = u"""\
Wizualizacja poszukiwania drogi w labiryncie przez algorytm genetyczny.
Użycie: python animation.py PLIK_Z_LABIRYNTEM\
"""


class AnimationWindow(QtGui.QMainWindow):
    def __init__(self, maze):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, maze.width * Maze.sq_size,
                maze.height * Maze.sq_size)
        self.setWindowTitle('Algorytm genetyczny - animacja')
        self.maze_canvas = Maze(self, maze)
        self.setCentralWidget(self.maze_canvas)
        #self.statusbar = self.statusBar()
        self.center()
        self.maze_canvas.start()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2,
                (screen.height()-size.height())/2)


class Maze(QtGui.QFrame):
    speed = 1000
    sq_size = 30

    def __init__(self, parent, maze):
        QtGui.QFrame.__init__(self, parent)
        self.timer = QtCore.QBasicTimer()
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.maze = maze # abstrakcyjna reprezentacja labiryntu (ga.Maze)
        self.width = 15
        self.height = 10

    def start(self):
        self.timer.start(Maze.speed, self)

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        black = QtGui.QColor(0x000000)
        blue = QtGui.QColor(0x0000CC)

        self.drawMaze(painter, black)

    def timerEvent(self, event):
        self.update()

    def drawMaze(self, painter, color):
        u"""Narysuj sam labirynt, bez rozwiązania."""
        # linie
        painter.setPen(color)
        for j in range(self.height):
            painter.drawLine(0, j * Maze.sq_size,
                    self.width * Maze.sq_size, j * Maze.sq_size)
        for i in range(self.width):
            painter.drawLine(i * Maze.sq_size, 0,
                    i * Maze.sq_size, self.height * Maze.sq_size)
        # przeszkody
        for j in range(self.height):
            for i in range(self.width):
                if self.maze.squares[j][i] == 1:
                    painter.fillRect(i * Maze.sq_size, j * Maze.sq_size,
                            Maze.sq_size, Maze.sq_size, color)


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            ga.maze = ga.Maze(sys.argv[1])
            selection = ga.select_proportional
            results = ga.epoch(ga.m, ga.l, ga.p_c, ga.p_m, 1, selection, None)
            path = results[-1].phenotype()
            print path

            app = QtGui.QApplication(sys.argv)
            animation = AnimationWindow(ga.maze)
            animation.show()

            sys.exit(app.exec_())
        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', sys.argv[1]
        sys.exit(1)

