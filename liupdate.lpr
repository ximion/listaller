{ liupdate.lpr
  Copyright (C) Listaller Project 2008

  liupdate.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  liupdate.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Application liUpdater updates IPK-installed applications
program liupdate;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mnupdate, httpsend, updexec,
  common, ipkhandle, ldunit, LResources,
  GetText, Translations, SysUtils;

{$IFDEF WINDOWS}{$R liupdate.rc}{$ENDIF}

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
  {$I liupdate.lrs}
  Application.Title:='Updater';
  Application.Initialize;
  writeLn('Updater initialized.');
  Application.ShowMainForm:=false;
  RegDir:=SyblToPath('$INST/app-reg/');
  Application.CreateForm(TUMnForm, UMnForm);
  Application.CreateForm(TUExecFm, UExecFm);
  writeLn('GUI created.');
  Application.CreateForm(TLoadForm, LoadForm);
  //Load translation resources
  TranslateInterface;
  writeLn('Translation loaded.');
  writeLn('Completed.');
  Application.Run;
  writeLn('Updater closed.');
end.

