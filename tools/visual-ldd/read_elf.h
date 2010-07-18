#ifndef READ_ELF_H
#define READ_ELF_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <qstring.h>
#include <vector>
#include <iostream>
#include "autopackage_ldd_Sub.h"

extern std::vector <QString> neededLibVector;
extern std::vector <QString> rpathVector;
#endif 
