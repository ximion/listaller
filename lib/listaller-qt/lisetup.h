/*
    libinstaller-qt - Qt4 wrapper for libListaller
    Copyright (C) 2010 Matthias Klumpp

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LISETUP_H
#define LISETUP_H

#include<QtGui>

#include<listaller.h>

class LiSetup {
  Q_OBJECT
public:
    LiSetup();
    ~LiSetup();
    
    void initialize(QString pkgName);
    void setSuMode(bool b);
    
    QString getDisallows() const;
    
    QString getSupportedDistributions() const;
    
    QString appName() const;
    QString appVersion() const;
    
    void setTestmode(bool b);
    

private:
    void* setup;
};

#endif // LISETUP_H
