<?xml version="1.0"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />
<title>~vcs-imports/listaller/trunk-gtk2 : contents of bindings/compile_lib.sh at revision 82</title>
<link href="/static/css/global.css" rel="stylesheet" />
<script src="/static/javascript/mootools-1.2-core.js" type="text/javascript"></script>
<script src="/static/javascript/mootools-1.2-more.js" type="text/javascript"></script>
<script src="/static/javascript/custom.js" type="text/javascript"></script>
<script type="text/javascript">
var global_path = 'http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/';
</script>
<script src="/static/javascript/yui/build/yui/yui-min.js" type="text/javascript"></script>
<link href="/static/css/annotate.css" media="all" type="text/css" rel="stylesheet" />
</head>
<body>
<div id="finderBox">
<form action="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/changes">
<label>search:</label>
<input autocomplete="off" onblur="hide_search();" type="search" name="q" id="q" />
</form>
<div>
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/atom" title="RSS feed for ~vcs-imports/listaller/trunk-gtk2">
<img src="/static/images/ico_rss.gif" alt="RSS" class="rssfeed" />
</a>
</div>
</div>
<ul id="menuTabs">
<li><a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/changes" title="Changes">Changes</a></li>
<li><a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/files" title="Files" id="on">Files</a></li>
<li><a href="https://help.launchpad.net/Loggerhead" title="Help">Help</a></li>
</ul>
<div id="loggerheadCont">
<div id="search_terms"></div>
<h1>
<a href="https://code.launchpad.net/~vcs-imports/listaller/trunk-gtk2">~vcs-imports/listaller/trunk-gtk2</a>
<span>: <span class="breadcrumb">
/<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/files/82?file_id=bindings-20090110142800-i4igek102jvrdcs8-1">bindings</a>/compile_lib.sh
</span> (revision 82)</span>
</h1>
<div>
<ul id="submenuTabs">
<li id="first">
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/files/82">browse files</a>
</li>
<li>
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/revision?start_revid=82">view revision</a>
</li>
<li>
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/changes?start_revid=82&amp;filter_file_id=bindingscompile_lib.-20090126020003-xqq8vwocl6elpbok-1">view changes to this file</a>
</li>
<li id="last">
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/download/head%3A/bindingscompile_lib.-20090126020003-xqq8vwocl6elpbok-1/compile_lib.sh">download file</a>
</li>
</ul>
<div class="annotate">
<table id="logentries">
<tr class="logheader">
<td class="annoLineTit">Line</td>
<td class="annoRevTit">Revision</td>
<td class="annoContTit">Contents</td>
</tr>
<tr class="blueRow1">
<td class="annoLine">1</td>
<td class="annoRev">
<a href="http://bazaar.launchpad.net/%7Evcs-imports/listaller/trunk-gtk2/revision?start_revid=79&amp;filter_file_id=bindingscompile_lib.-20090126020003-xqq8vwocl6elpbok-1" title="79 by ximion, on 25 Jan 2009 20:48 (2009-01-25 20:48:54)">79</a>
</td>
<td class="annoCont">#!/bin/bash
</td>
</tr><tr class="blueRow1">
<td class="annoLine">2</td>
<td class="annoRev">
</td>
<td class="annoCont"># get latest from http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html
</td>
</tr><tr class="blueRow1">
<td class="annoLine">3</td>
<td class="annoRev">
</td>
<td class="annoCont"># for more info, you can visit the links on above page to FreePascal and Lazarus wiki
</td>
</tr><tr class="blueRow1">
<td class="annoLine">4</td>
<td class="annoRev">
</td>
<td class="annoCont">
</td>
</tr><tr class="blueRow1">
<td class="annoLine">5</td>
<td class="annoRev">
</td>
<td class="annoCont"># download corresponding qt source
</td>
</tr><tr class="blueRow1">
<td class="annoLine">6</td>
<td class="annoRev">
</td>
<td class="annoCont"># unpack qt source -&gt; the created directory (Qt4 dir) will be used below
</td>
</tr><tr class="blueRow1">
<td class="annoLine">7</td>
<td class="annoRev">
</td>
<td class="annoCont"># do configure and gmake in that Qt4 dir
</td>
</tr><tr class="blueRow1">
<td class="annoLine">8</td>
<td class="annoRev">
</td>
<td class="annoCont">
</td>
</tr><tr class="blueRow1">
<td class="annoLine">9</td>
<td class="annoRev">
</td>
<td class="annoCont">NAME=libkbind.so
</td>
</tr><tr class="blueRow1">
<td class="annoLine">10</td>
<td class="annoRev">
</td>
<td class="annoCont">QTDIR=/usr/share/qt4
</td>
</tr><tr class="blueRow1">
<td class="annoLine">11</td>
<td class="annoRev">
</td>
<td class="annoCont">INCLUDE_PATH="-I. -I$QTDIR/include -I$QTDIR/include/Qt -I$QTDIR/include/QtGui -I$QTDIR/include/QtCore -I/usr/include/KDE -Iqlcl "
</td>
</tr><tr class="blueRow1">
<td class="annoLine">12</td>
<td class="annoRev">
</td>
<td class="annoCont">LIB_PATH=$QTDIR/lib
</td>
</tr><tr class="blueRow1">
<td class="annoLine">13</td>
<td class="annoRev">
</td>
<td class="annoCont">export LD_LIBRARY_PATH=$LIB_PATH
</td>
</tr><tr class="blueRow1">
<td class="annoLine">14</td>
<td class="annoRev">
</td>
<td class="annoCont">if [ -e "$LIB_PATH/libQtCore.so.4" ]
</td>
</tr><tr class="blueRow1">
<td class="annoLine">15</td>
<td class="annoRev">
</td>
<td class="annoCont">then
</td>
</tr><tr class="blueRow1">
<td class="annoLine">16</td>
<td class="annoRev">
</td>
<td class="annoCont">  echo please wait for compile to finish ...
</td>
</tr><tr class="blueRow1">
<td class="annoLine">17</td>
<td class="annoRev">
</td>
<td class="annoCont">  g++ -D BINUX $INCLUDE_PATH libkbind.cpp -o libkbind.so -shared -fPIC -lQtCore -lQtGui  -Xlinker -soname=$NAME -Xlinker --library-path -Xlinker $LIB_PATH
</td>
</tr><tr class="blueRow1">
<td class="annoLine">18</td>
<td class="annoRev">
</td>
<td class="annoCont">  echo Showing used Qt libraries when LD_LIBRARY_PATH=$LD_LIBRARY_PATH
</td>
</tr><tr class="blueRow1">
<td class="annoLine">19</td>
<td class="annoRev">
</td>
<td class="annoCont">  ldd $NAME | grep libk
</td>
</tr><tr class="blueRow1">
<td class="annoLine">20</td>
<td class="annoRev">
</td>
<td class="annoCont">  echo stripping library
</td>
</tr><tr class="blueRow1">
<td class="annoLine">21</td>
<td class="annoRev">
</td>
<td class="annoCont">  strip --strip-all $NAME
</td>
</tr><tr class="blueRow1">
<td class="annoLine">22</td>
<td class="annoRev">
</td>
<td class="annoCont">  echo Done
</td>
</tr><tr class="blueRow1">
<td class="annoLine">23</td>
<td class="annoRev">
</td>
<td class="annoCont">else
</td>
</tr><tr class="blueRow1">
<td class="annoLine">24</td>
<td class="annoRev">
</td>
<td class="annoCont">  echo "Please Modify location of Qt4 in this script"
</td>
</tr><tr class="blueRow1">
<td class="annoLine">25</td>
<td class="annoRev">
</td>
<td class="annoCont">fi
</td>
</tr>
</table>
</div>
</div>
<p class="fl">Loggerhead is a web-based interface for <a href="http://bazaar-vcs.org/">Bazaar</a> branches</p>
</div>
</body>
</html>