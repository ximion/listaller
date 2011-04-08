#-------------------------------------------------
#
# Project file for Visual-LDD
# Created on D:2010-07-18 T:17:30:59
#
#-------------------------------------------------

QT += core gui

TARGET = visual-ldd
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    read_elf.cpp \
    treeitem.cpp \
    treemodel.cpp

HEADERS += mainwindow.h \
    read_elf.h \
    treemodel.h \
    treeitem.h

FORMS += mainwindow.ui


RESOURCES += \
    resources.qrc
