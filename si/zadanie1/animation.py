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
    def __init__(self, maze, moves):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, maze.width * Maze.sq_size,
                maze.height * Maze.sq_size + 15)
        self.setWindowTitle('Algorytm genetyczny - animacja')
        self.maze_canvas = Maze(self, maze, moves)
        self.setCentralWidget(self.maze_canvas)
        self.statusbar = self.statusBar()
        self.connect(self.maze_canvas,
                QtCore.SIGNAL('messageToStatusbar(QString)'), self.statusbar,
                QtCore.SLOT('showMessage(QString)'))
        self.maze_canvas.start()
        self.center()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2,
                (screen.height()-size.height())/2)


class Maze(QtGui.QFrame):
    speed = 200
    sq_size = 30

    def __init__(self, parent, maze, moves):
        QtGui.QFrame.__init__(self, parent)
        self.timer = QtCore.QBasicTimer()
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.maze = maze # abstrakcyjna reprezentacja labiryntu (ga.Maze)
        self.moves = moves
        self.path = [] # kolejne pozycje agenta
        self.path.append(self.maze.start_pos)

    def start(self):
        self.timer.start(Maze.speed, self)

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        black = QtGui.QColor(0x000000)
        grey = QtGui.QColor(0x808080)
        red = QtGui.QColor(0xCC0000)
        green = QtGui.QColor(0x00CC00)
        blue = QtGui.QColor(0x0000FF)

        self.drawMaze(painter, black, green, red)
        self.drawSolution(painter, grey, blue)

    def timerEvent(self, event):
        if self.path[-1] == self.maze.end_pos:
            self.timer.stop()
        self.advanceAgent()
        self.update()
        self.emit(QtCore.SIGNAL('messageToStatusbar(QString)'),
                'ruch ' + str(len(self.path)-1))

    def drawMaze(self, painter, color, start_color, end_color):
        u"""Narysuj sam labirynt, bez rozwiązania."""
        # linie
        painter.setPen(color)
        for j in range(self.maze.height):
            painter.drawLine(0, j * Maze.sq_size,
                    self.maze.width * Maze.sq_size, j * Maze.sq_size)
        for i in range(self.maze.width):
            painter.drawLine(i * Maze.sq_size, 0,
                    i * Maze.sq_size, self.maze.height * Maze.sq_size)
        # ściany
        for j in range(self.maze.height):
            for i in range(self.maze.width):
                if self.maze.squares[j][i] == 1:
                    painter.fillRect(i * Maze.sq_size, j * Maze.sq_size,
                            Maze.sq_size, Maze.sq_size, color)
        # pozycja startowa i końcowa
        painter.fillRect(self.maze.start_pos.x * Maze.sq_size + 1,
                self.maze.start_pos.y * Maze.sq_size + 1,
                Maze.sq_size - 1, Maze.sq_size - 1, start_color)
        painter.fillRect(self.maze.end_pos.x * Maze.sq_size + 1,
                self.maze.end_pos.y * Maze.sq_size + 1,
                Maze.sq_size - 1, Maze.sq_size - 1, end_color)

    def drawSolution(self, painter, path_color, agent_color):
        for i in range(1, len(self.path)):
            position = self.path[i]
            if position == self.path[-1]:
                color = agent_color
            else:
                color = path_color
            if position != self.maze.start_pos: # nie zasłaniamy startu
                painter.fillRect(position.x * Maze.sq_size + 1,
                        position.y * Maze.sq_size + 1,
                        Maze.sq_size - 1, Maze.sq_size - 1, color)

    def advanceAgent(self):
        u"""Przejdź dalej w kierunku wyjścia z labiryntu."""
        if moves and self.path[-1] != self.maze.end_pos:
            move = self.moves.pop(0)
            self.path.append(self.maze.move(self.path[-1], move))


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            app = QtGui.QApplication(sys.argv)

            ga.maze = ga.Maze(sys.argv[1])
            selection = ga.select_proportional
            results = ga.epoch(ga.m, ga.l, ga.p_c, ga.p_m, 1, selection, None)
            moves = results[-1].phenotype()

            animation = AnimationWindow(ga.maze, moves)
            animation.show()

            sys.exit(app.exec_())
        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', sys.argv[1]
        sys.exit(1)

