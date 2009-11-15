# Makefile for Listaller 0.4
#
# Targets:
# The First target is the one build when there is nothing on make command line
all:
	chmod +x ./make-base.sh
	chmod +x ./make-gui.sh
	./make-base.sh
	./make-gui.sh WIDGET=gtk2
	./make-gui.sh WIDGET=qt4

all-qt:
	chmod +x ./make-base.sh
	chmod +x ./make-gui.sh
	./make-base.sh
	./make-gui.sh WIDGET=qt4

all-gtk:
	chmod +x ./make-base.sh
	chmod +x ./make-gui.sh
	./make-base.sh
	./make-gui.sh WIDGET=gtk2

all-nogui:
	chmod +x ./make-base.sh
	./make-base.sh

install-all:
	chmod +x ./install-core.sh
	chmod +x ./install-tools.sh
	chmod +x ./install-data.sh
	chmod +x ./install-gui.sh
	./install-core.sh
	./install-tools.sh
	./install-data.sh
	./install-gui.sh WIDGET=gtk2
	./install-gui.sh WIDGET=qt4

install-all-bin:
	chmod +x ./install-core.sh
	chmod +x ./install-tools.sh
	chmod +x ./install-gui.sh
	./install-core.sh
	./install-tools.sh
	./install-gui.sh WIDGET=gtk2
	./install-gui.sh WIDGET=qt4

install-data:
	chmod +x ./install-data.sh
	./install-data.sh

install-tools:
	chmod +x ./install-core.sh
	chmod +x ./install-tools.sh
	./install-core.sh
	./install-tools.sh

install-front-gtk:
	chmod +x ./install-core.sh
	chmod +x ./install-gui.sh
	./install-core.sh
	./install-gui.sh WIDGET=gtk2 

install-front-qt:
	chmod +x ./install-core.sh
	chmod +x ./install-gui.sh
	./install-core.sh
	./install-gui.sh WIDGET=qt4

install-all-gtk:
	chmod +x ./install-core.sh
	chmod +x ./install-gui.sh
	chmod +x ./install-data.sh
	chmod +x ./install-tools.sh
	./install-core.sh
	./install-tools.sh
	./install-data.sh
	./install-gui.sh WIDGET=gtk2

install-all-qt:
	chmod +x ./install-core.sh
	chmod +x ./install-gui.sh
	chmod +x ./install-data.sh
	chmod +x ./install-tools.sh
	./install-core.sh
	./install-tools.sh
	./install-data.sh
	./install-gui.sh WIDGET=qt4

licreator-qt:
	chmod +x ./make-creator.sh
	./make-creator.sh WIDGET=qt4
	
licreator-gtk:
	chmod +x ./make-creator.sh
	./make-creator.sh WIDGET=gtk2

install-licreator-gtk:
	chmod +x ./install-creator.sh
	./install-creator.sh WIDGET=gtk2

install-licreator-qt:
	chmod +x ./install-creator.sh
	./install-creator.sh WIDGET=qt4

clean:
	chmod +x ./clean.sh
	./clean.sh

uninstall:
	chmod +x ./uninstall.sh
	./uninstall.sh
