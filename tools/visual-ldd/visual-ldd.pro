TEMPLATE = app

QT += opengl \
    webkit \
    phonon \
    multimedia

TARGET = VisualLDD

HEADERS	+= autopackage_ldd_Sub.h \
	read_elf.h

SOURCES	+= main.cpp \
	autopackage_ldd_Sub.cpp \
	read_elf.cpp

FORMS	= autopackage_ldd.ui

IMAGES	= images/about \
	images/editcopy \
	images/editcut \
	images/editpaste \
	images/filenew \
	images/fileopen \
	images/filesave \
	images/print \
	images/redo \
	images/searchfind \
	images/undo \
	images/exit

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}
