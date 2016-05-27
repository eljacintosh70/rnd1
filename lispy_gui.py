# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'lispy_gui.ui'
#
# Created: Fri May 27 15:04:50 2016
#      by: pyside-uic 0.2.15 running on PySide 1.2.1
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(931, 415)
        self.editLispyCommand = QtGui.QPlainTextEdit(Form)
        self.editLispyCommand.setGeometry(QtCore.QRect(0, 10, 921, 160))
        font = QtGui.QFont()
        font.setPointSize(12)
        self.editLispyCommand.setFont(font)
        self.editLispyCommand.setFrameShape(QtGui.QFrame.StyledPanel)
        self.editLispyCommand.setFrameShadow(QtGui.QFrame.Sunken)
        self.editLispyCommand.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOn)
        self.editLispyCommand.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAsNeeded)
        self.editLispyCommand.setDocumentTitle("")
        self.editLispyCommand.setLineWrapMode(QtGui.QPlainTextEdit.NoWrap)
        self.editLispyCommand.setPlainText("#Comentarios son lineas comenzando con \'#\'\n"
"#Las lineas en blanco se ignoran y quedan eliminadas en la salida\n"
"#No usar acentos pues no he hecho esa parte aun\n"
"(quote (testing 1 (2.0) -3.14e159))\n"
"(+ 2 2)\n"
"(+ (* 2 100) (* 1 10))\n"
"(if (> 6 5) (+ 1 1) (+ 2 2))\n"
"(if (< 6 5) (+ 1 1) (+ 2 2))\n"
"(define x 3)\n"
"(begin (define x 1) (set! x (+ x 1)) (+ x 1))\n"
"((lambda (x) (+ x x)) 5)\n"
"(define twice (lambda (x) (* 2 x)))\n"
"(define compose (lambda (f g) (lambda (x) (f (g x)))))\n"
"((compose list twice) 5)\n"
"(define repeat (lambda (f) (compose f f)))\n"
"((repeat twice) 5)\n"
"\n"
"#Un factorial:\n"
"(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))\n"
"(fact 3)\n"
"(fact 50)\n"
"(define abs (lambda (n) ((if (> n 0) + -) 0 n)))\n"
"(list (abs -3) (abs 0) (abs 3))\n"
"(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))\n"
"(define zip (combine cons))\n"
"(zip (list 1 2 3 4) (list 5 6 7 8))\n"
"(define riff-shuffle (lambda (deck) (begin (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq))))) (define mid (lambda (seq) (/ (length seq) 2))) ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))\n"
"(riff-shuffle (list 1 2 3 4 5 6 7 8))\n"
"((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))\n"
"(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))\n"
"#Otro factorial\n"
"(define (factorial n acc) (if (= n 0) acc (factorial (- n 1) (* n acc))))\n"
"(define (fact n) (factorial n 1))\n"
"(fact 50)\n"
"")
        self.editLispyCommand.setCursorWidth(2)
        self.editLispyCommand.setObjectName("editLispyCommand")
        self.btnQuit = QtGui.QPushButton(Form)
        self.btnQuit.setGeometry(QtCore.QRect(820, 180, 98, 31))
        self.btnQuit.setObjectName("btnQuit")
        self.editOutput = QtGui.QPlainTextEdit(Form)
        self.editOutput.setGeometry(QtCore.QRect(0, 220, 921, 180))
        self.editOutput.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOn)
        self.editOutput.setCursorWidth(2)
        self.editOutput.setObjectName("editOutput")
        self.btnEvaluate = QtGui.QPushButton(Form)
        self.btnEvaluate.setGeometry(QtCore.QRect(0, 180, 98, 31))
        self.btnEvaluate.setObjectName("btnEvaluate")
        self.btnClearInput = QtGui.QPushButton(Form)
        self.btnClearInput.setGeometry(QtCore.QRect(140, 180, 98, 31))
        self.btnClearInput.setObjectName("btnClearInput")

        self.retranslateUi(Form)
        QtCore.QObject.connect(self.btnQuit, QtCore.SIGNAL("clicked()"), Form.close)
        QtCore.QObject.connect(self.btnEvaluate, QtCore.SIGNAL("clicked()"), Form.DoEvaluate)
        QtCore.QObject.connect(self.btnClearInput, QtCore.SIGNAL("clicked()"), Form.DoClearInput)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        Form.setWindowTitle(QtGui.QApplication.translate("Form", "lispy - Un intérprete de Lisp", None, QtGui.QApplication.UnicodeUTF8))
        self.btnQuit.setText(QtGui.QApplication.translate("Form", "&Quit", None, QtGui.QApplication.UnicodeUTF8))
        self.btnEvaluate.setText(QtGui.QApplication.translate("Form", "&Evaluate", None, QtGui.QApplication.UnicodeUTF8))
        self.btnClearInput.setText(QtGui.QApplication.translate("Form", "Clear Input", None, QtGui.QApplication.UnicodeUTF8))


if __name__ == "__main__":
    import sys
    app = QtGui.QApplication(sys.argv)
    Form = QtGui.QWidget()
    ui = Ui_Form()
    ui.setupUi(Form)
    Form.show()
    sys.exit(app.exec_())

