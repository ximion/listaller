#
# Targets
#

#
# The First target is the one build when there is nothing on make command line
#
all:
	./make.sh

litools-inst:
	./install-tools.sh

licreator:
	./make-creator.sh

licreator-inst:
	./install-creator.sh

litheme:
	./make-litheme.sh

litheme-inst:
	./install-litheme.sh

clean:
	./clean.sh

install:
	./install.sh

lcl:
	./makelcl.sh

uninstall:
	./uninstall.sh

packages: 
	./createpkg.sh
