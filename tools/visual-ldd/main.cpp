#include <qapplication.h>
#include "autopackage_ldd_Sub.h"

using std::cerr;


int main( int argc, char ** argv )
{
    QApplication a(argc, argv);
    if(argc > 2)
    {
    	cerr << "Usage: " << argv[0] << " [ELFfilename]\n";
    	exit(-1);
    }
    autopackage_ldd_Sub w(0, 0, QString(argv[1]));
    w.sh
    a.connect( &a, SIGNAL( lastWindowClosed() ), &a, SLOT( quit() ) );
    return a.exec();
}
