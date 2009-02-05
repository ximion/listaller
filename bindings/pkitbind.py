#!/usr/bin/python
# -*- coding: utf-8 -*-
# Licensed under the GNU General Public License Version 2
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# (c) 2008 
#     Listaller Project
#     Matthias Klumpp <matthias@nlinux.org>

from packagekit.client import *
from packagekit.enums import *
from packagekit.misc import *
import getopt
import sys
import os.path

pk = PackageKitClient()

if sys.argv[1]=='--cache-refresh':
  print 'RefreshCache...'
  print pk.refresh_cache()

if sys.argv[1]=='--is-installed':
  print 'Resolve: '+sys.argv[2]+' (check if installed)'
  try:
      pkg = pk.resolve(sys.argv[2],FILTER_INSTALLED)
  except: 
   sys.exit(2)
  try:
   s = str(pkg[0])
  except:
   print('Not found!')
   sys.exit(0);
  print(s)
  if s.find(sys.argv[2]) < 0:
   sys.exit(0)
  else:
   sys.exit(1)

if sys.argv[1]=='--resolve':
  print 'Resolve: '+sys.argv[2]
  try:
      pkg = pk.resolve(sys.argv[2],FILTER_NONE)
  except: 
   sys.exit(2)
  print pkg[0]
 # print pkg[1]

if sys.argv[1]=='--details':
  print 'Details: '+sys.argv[2]
  pkg = pk.get_details([sys.argv[2]+';;;'])
  print "Not yet implemeted!"
  sys.exit(100)


if sys.argv[1]=='--install-local':
   print 'Installation of local package: '+os.path.basename(sys.argv[2])
   try:
      pkg = pk.install_files(False,[sys.argv[2]])
   except:
      print 'Failed.'
      sys.exit(2)
   print PackageKitPackage(pkg)[1]

if sys.argv[1]=='--install':
   print 'Installation of: '+sys.argv[2]
   try:
      pkg = pk.install_packages([sys.argv[2]+';;;'])
   except:
      print 'Failed.'
      sys.exit(2)
   print pkg[0]

if sys.argv[1]=='--get-requires':
  print 'Reverse dependencies of: '+sys.argv[2]
  pkg = pk.get_requires([sys.argv[2]+';;;'])
  for item in pkg:
      print item

if sys.argv[1]=='--s-file':
  try:
   pkg = pk.search_file(sys.argv[2],FILTER_INSTALLED)
   print pkg[0]
  except:
   print "Failed!"
   sys.exit(2)

if sys.argv[1]=='--s-dfile':
  try:
    pkg = pk.search_file(sys.argv[2],FILTER_NONE)
    print pkg
  except:
    print "Failed!"
    sys.exit(2)
  
if sys.argv[1]=='--remove':
  print 'Removing: '+sys.argv[2]
  pkg = pk.remove_packages([sys.argv[2]+';;;'])

sys.exit(0)