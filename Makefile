#
# Targets
#

#
# The First target is the one build when there is nothing on make command line
#
all:
	chmod +x ./make.sh
	./make.sh WIDGET=gtk2
	./make.sh WIDGET=qt4

all-qt:
	chmod +x ./make.sh
	./make.sh WIDGET=qt4

all-gtk:
	chmod +x ./make.sh
	./make.sh WIDGET=gtk2

install-data:
	chmod +x ./install-data.sh
	./install-data.sh

install-build-tools:
	chmod +x ./install-build-tools.sh
	./install-build-tools.sh

install-core:
	chmod +x ./install-cmds.sh
	./install-cmds.sh

licreator:
	chmod +x ./make-creator.sh
	./make-creator.sh

install-licreator-gtk:
	chmod +x ./install-creator.sh
	./install-creator.sh WIDGET=gtk2

install-licreator-qt:
	chmod +x ./install-creator.sh
	./install-creator.sh WIDGET=qt4

litheme:
	chmod +x ./make-litheme.sh
	./make-litheme.sh

install-litheme:
	chmod +x ./install-litheme.sh
	./install-litheme.sh

clean:
	chmod +x ./clean.sh
	./clean.sh

install-front-gtk:
	chmod +x ./install.sh
	./install.sh WIDGET=gtk2 

install-front-qt:
	chmod +x ./install.sh
	./install.sh WIDGET=qt4

install-all-gtk:
	chmod +x ./install.sh
	./install.sh WIDGET=gtk2
	chmod +x ./install-data.sh
	./install-data.sh
	chmod +x ./install-cmds.sh
	./install-cmds.sh

install-all-qt:
	chmod +x ./install.sh
	./install.sh WIDGET=qt4
	chmod +x ./install-data.sh
	./install-data.sh
	chmod +x ./install-cmds.sh
	./install-cmds.sh

uninstall:
	chmod +x ./uninstall.sh
	./uninstall.sh
