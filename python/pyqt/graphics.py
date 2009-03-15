#!/usr/bin/python

import sys
from PyQt4 import QtGui as gui

app = gui.QApplication(sys.argv)

scene = gui.QGraphicsScene()
scene.addText('Pop goes the weasel!')
scene.addLine(-100, -100, 50, -50)
scene.addRect(-100, -150, 150, -50)

view = gui.QGraphicsView(scene)
view.resize(640, 480)
view.setWindowTitle('PyQt4 Graphics Demo')
view.show()

sys.exit(app.exec_())
