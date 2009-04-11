#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Gra w życie Johna Conwaya - przykład automatu komórkowego."""

__author__ = u'Michał Dettlaff'

from PyQt4 import QtCore, QtGui
import getopt
import random
import sys


usage = """\
Użycie: python game_of_life.py [opcje]
Opcje:
    -p, --pattern=NAME  Zacznij od struktury o nazwie NAME. Możliwe wartości:
                        glider, lwss, f-pentomino, diehard, immortal, random
    -s, --speed=TIME    Szybkość animacji; opóźnienie równe TIME milisekund.\
"""

DEFAULT_SPEED = 200
BOARD_WIDTH = 32
BOARD_HEIGHT = 32


class AnimationWindow(QtGui.QMainWindow):
    def __init__(self, board, speed):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, Scene.cell_size * len(board[0]),
                Scene.cell_size * len(board) + 25)
        self.setWindowTitle(u'Gra w życie')
        self.scene = Scene(self, board, speed)
        self.setCentralWidget(self.scene)
        self.statusbar = self.statusBar()
        self.connect(self.scene, QtCore.SIGNAL('messageToStatusbar(QString)'),
                self.statusbar, QtCore.SLOT('showMessage(QString)'))
        self.scene.start()
        self.center()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width() - size.width()) / 2,
                (screen.height() - size.height()) / 2)


class Scene(QtGui.QFrame):
    u"""Obszar na którym będziemy rysować."""
    cell_size = 16

    def __init__(self, parent, board, speed):
        QtGui.QFrame.__init__(self, parent)
        Scene.speed = speed
        self.timer = QtCore.QBasicTimer()
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.iter_count = 0
        self.board = board
        self.board_width = len(self.board[0])
        self.board_height = len(self.board)

    def start(self):
        self.timer.start(Scene.speed, self)

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        black = QtGui.QColor(0x000000)
        white = QtGui.QColor(0xFFFFFF)
        grey = QtGui.QColor(0x808080)

        self.drawScene(painter, white, black, grey)

    def timerEvent(self, event):
        self.iter_count += 1
        self.nextStep()
        self.update()
        self.emit(QtCore.SIGNAL('messageToStatusbar(QString)'),
                u'Iteracja ' + str(self.iter_count))

    def drawScene(self, painter, bg_color, color, line_color):
        cs = Scene.cell_size
        painter.fillRect(0, 0, self.board_width * cs, self.board_height * cs,
                bg_color)
        # komórki
        for j, row in enumerate(self.board):
            for i, cell in enumerate(row):
                if cell == 1:
                    painter.fillRect(cs * i, cs * j, cs, cs, color)
        # linie siatki
        painter.setPen(line_color)
        for j in range(self.board_height + 1):
            painter.drawLine(0, j * cs, self.board_width * cs, j * cs)
        for i in range(self.board_width + 1):
            painter.drawLine(i * cs, 0, i * cs, self.board_height * cs)

    def nextStep(self):
        live = []
        die = []
        for j, row in enumerate(self.board):
            for i, cell in enumerate(row):
                neighbors_count = 0
                if j > 0 and i > 0 and self.board[j-1][i-1]:
                    neighbors_count += 1
                if j > 0 and self.board[j-1][i]:
                    neighbors_count += 1
                if j > 0 and i < self.board_width - 1 and self.board[j-1][i+1]:
                    neighbors_count += 1
                if i > 0 and self.board[j][i-1]:
                    neighbors_count += 1
                if i < self.board_width - 1 and self.board[j][i+1]:
                    neighbors_count += 1
                if j < self.board_height - 1 and i > 0 and \
                        self.board[j+1][i-1]:
                    neighbors_count += 1
                if j < self.board_height - 1 and self.board[j+1][i]:
                    neighbors_count += 1
                if j < self.board_height - 1 and i < self.board_width - 1 and \
                        self.board[j+1][i+1]:
                    neighbors_count += 1
                if cell == 0 and neighbors_count == 3:
                    live.append((i, j))
                elif cell == 1 and not 2 <= neighbors_count <= 3:
                    die.append((i, j))
        for coords in live:
            self.board[coords[1]][coords[0]] = 1
        for coords in die:
            self.board[coords[1]][coords[0]] = 0
        if not live and not die:
            self.timer.stop()
            self.iter_count -= 1


def main(argv):
    speed = DEFAULT_SPEED
    # glider
    pattern = [ [0, 1, 0],
                [0, 0, 1],
                [1, 1, 1] ]
    vertical_shift = 0
    try:
        options, args = getopt.getopt(argv[1:], 'hp:s:', ['help',
            'speed=', 'pattern=',])
        for option, argument in options:
            if option in ('-h', '--help'):
                print __doc__
                print usage
                sys.exit()
            elif option in ('-s', '--speed'):
                speed = int(argument)
            elif option in ('-p', '--pattern'):
                if argument == 'glider':
                    pattern = [ [0, 1, 0],
                                [0, 0, 1],
                                [1, 1, 1] ]
                elif argument == 'lwss':
                    pattern = [ [1, 0, 0, 1, 0],
                                [0, 0, 0, 0, 1],
                                [1, 0, 0, 0, 1],
                                [0, 1, 1, 1, 1] ]
                elif argument == 'f-pentomino':
                    pattern = [ [0, 1, 1],
                                [1, 1, 0],
                                [0, 1, 0] ]
                elif argument == 'diehard':
                    pattern = [ [0, 0, 0, 0, 0, 0, 1, 0],
                                [1, 1, 0, 0, 0, 0, 0, 0],
                                [0, 1, 0, 0, 0, 1, 1, 1] ]
                    vertical_shift = -5
                elif argument == 'immortal':
                    pattern = [ [1, 1, 1, 0, 1],
                                [1, 0, 0, 0, 0],
                                [0, 0, 0, 1, 1],
                                [0, 1, 1, 0, 1],
                                [1, 0, 1, 0, 1] ]
                elif argument == 'random':
                    pattern = [[random.randint(0, 1) for i in \
                            range(BOARD_WIDTH)] for j in range(BOARD_HEIGHT)]
                else:
                    print u'nieznana nazwa struktury:', argument
                    print usage
                    sys.exit(2)
    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)

    board = [[0]*BOARD_WIDTH for i in range(BOARD_HEIGHT)]
    # umieść strukturę na planszy
    x = BOARD_WIDTH / 2 - len(pattern[0]) / 2 - len(pattern[0]) % 2
    y = BOARD_HEIGHT / 2 - len(pattern) / 2 - len(pattern) % 2 + vertical_shift
    for j, row in enumerate(board[y : y + len(pattern)]):
        row[x : x + len(pattern[0])] = pattern[j]

    app = QtGui.QApplication(sys.argv)
    animation = AnimationWindow(board, speed)
    animation.show()
    sys.exit(app.exec_())


if __name__ == '__main__':
    main(sys.argv)

