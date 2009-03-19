#!/usr/bin/python
# -*- coding: UTF-8 -*-


from PyQt4 import QtCore, QtGui
import sys
import random


class Animation(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.setGeometry(300, 300, 500, 500)
        self.setWindowTitle('Algorytm genetyczny - animacja')
        self.maze = Maze(self)
        self.setCentralWidget(self.maze)
        #self.statusbar = self.statusBar()
        self.center()
        self.maze.start()

    def center(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2,
                (screen.height()-size.height())/2)


class Maze(QtGui.QFrame):
    speed = 500

    def __init__(self, parent):
        QtGui.QFrame.__init__(self, parent)
        self.timer = QtCore.QBasicTimer()
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.square_size = 100

    def start(self):
        self.timer.start(Maze.speed, self)

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        #rect = self.contentsRect()
        color = QtGui.QColor(0x1010CC)

        painter.setPen(color.light())
        painter.fillRect(10, 10, self.square_size, self.square_size, color)
        painter.drawLine(0, 0, 500, 500)

    def timerEvent(self, event):
        self.square_size = random.randint(50, 400)
        self.update()


if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)

    animation = Animation()
    animation.show()

    sys.exit(app.exec_())

