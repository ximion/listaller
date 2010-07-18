#-------------------------------------------------
#
# Project created by QtCreator 2010-07-18T17:30:59
#
#-------------------------------------------------

QT       += core gui

TARGET = visual-ldd
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    read_elf.cpp \
    treeitem.cpp \
    treemodel.cpp

HEADERS  += mainwindow.h \
    read_elf.h \
    treemodel.h \
    treeitem.h

FORMS    += mainwindow.ui

OTHER_FILES +=

RESOURCES += \
    resources.qrc
