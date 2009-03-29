{ listallmngr.lpr
  Copyright (C) Listaller Project 2008

  listallmngr.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  listallmngr.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>}
//** Application that manages all installed applications
program listallmngr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  manager, settings, uninstall, pkgconvertdisp, swcatalog, LResources, ipkhandle,
  utilities, translations, gettext;

{$IFDEF WINDOWS}{$R listallmngr.rc}{$ENDIF}

procedure TranslateInterface;
var PODirectory, Lang, FallbackLang: String;
begin
Lang:=Copy(GetEnvironmentVariable('LANG'), 1, 2);
PODirectory := GetDataFile('lang/');
GetLanguageIDs(Lang, FallbackLang);
translations.TranslateUnitResourceStrings('LCLStrConsts', PODirectory + 'lclstrconsts-%s.po', Lang, FallbackLang);
translations.TranslateUnitResourceStrings('trstrings', PODirectory + 'listaller-%s.po', Lang, FallbackLang);
translations.SystemCharSetIsUTF8:=true;
end;

begin
  {$I listallmngr.lrs}
  //Load Translation
  TranslateInterface();
  Application.Title:='Listaller Manager';
  Application.ShowMainForm:=false;
  Application.Initialize;
  pkit := GetDataFile('pkitbind/pkitbind.py')+' ';
  writeLn('Application initialized.');
  Application.CreateForm(TMnFrm, MnFrm);
  Application.CreateForm(TFmConfig, FmConfig);
  Application.CreateForm(TConvDisp, ConvDisp);
  Application.CreateForm(TSCForm, SCForm);
  Application.Run;
  writeLn('Listaller manager closed.');
end.
