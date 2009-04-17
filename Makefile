#
# Targets
#

#
# The First target is the one build when there is nothing on make command line
#
all:
	chmod +x ./make.sh
	./make.sh

litools-inst:
	chmod +x ./install-tools.sh
	./install-tools.sh

licreator:
	chmod +x ./install-tools.sh
	./install-tools.sh

licreator-inst:
	chmod +x ./install-creator.sh
	./install-creator.sh

litheme:
	chmod +x ./make-litheme.sh
	./make-litheme.sh

litheme-inst:
	chmod +x ./install-litheme.sh
	./install-litheme.sh

clean:
	chmod +x ./clean.sh
	./clean.sh

install:
	chmod +x ./install.sh
	./install.sh

lcl:
	chmod +x ./makelcl.sh
	./makelcl.sh

uninstall:
	chmod +x ./uninstall.sh
	./uninstall.sh

packages:
	chmod +x ./createpkg.sh
	./createpkg.sh
