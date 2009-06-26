{ litranslator.pas
  Copyright (C) Listaller Project 2008-2009

  litranslator.pa is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  litranslator.pa is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Applies localisation to all Listaller modules
{ This file is a DefaultTranslater unit with some special Listaller modifications }
unit litranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GetText, Controls, typinfo, FileUtil;

type
 TDefaultTranslator=class(TAbstractTranslator)
 private
  FMOFile:TMOFile;
 public
  constructor Create(MOFileName:string);
  destructor Destroy;override;
  procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);override;
 end;

implementation
uses Menus;

function FindLocaleFileName:string;
var LANG,lng:string;
  i: Integer;
  liname: String;
begin
 //Win32 user may decide to override locale with LANG variable.
 LANG:=GetEnvironmentVariableUTF8('LANG');
 if LANG='' then begin
   for i:=1 to Paramcount-1 do
    if (ParamStrUTF8(i)='--LANG') or
     (ParamStrUTF8(i)='-l') or
     (ParamStrUTF8(i)='--lang') then LANG:=ParamStrUTF8(i+1);
 end;

 liname:='listaller';

 if LANG<>'' then begin
  //ParamStrUTF8(0) is said not to work properly in linux, but I've tested it
  Result:=ExtractFilePath(ParamStrUTF8(0))+LANG+
    DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+LANG+
    DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  //In unix-like systems we can try to search for global locale
  Result:='/usr/share/locale/'+LANG+'/LC_MESSAGES/'
   +liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  //Let us search for reducted files
  lng:=copy(LANG,1,2);
  //At first, check all was checked
  Result:=ExtractFilePath(ParamStrUTF8(0))+lng+
    DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+lng+
    DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
    +lng+DirectorySeparator+liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  //Full language in file name - this will be default for the project
  //We need more carefull handling, as it MAY result in incorrect filename
  try
    Result:=ExtractFilePath(ParamStrUTF8(0))+liname+'.'+LANG+'.mo';
    if FileExistsUTF8(Result) then exit;
   //Common location (like in Lazarus)
    Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+liname+'.'+LANG+'.mo';
    if FileExistsUTF8(Result) then exit;

    Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+liname+'-'+LANG+'.mo';
    if FileExistsUTF8(Result) then exit;

    Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+liname+'.'+LANG+'.mo';
    if FileExistsUTF8(Result) then exit;
  except
  end;
  Result:='/usr/share/locale/'+lng+'/LC_MESSAGES/'
   +liname+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+liname+'.'+lng+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+liname+'.'+lng+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+liname+'-'+lng+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+liname+'.'+lng+'.mo';
  if FileExistsUTF8(Result) then exit;
 end;
 Result:=ChangeFileExt(ParamStrUTF8(0),'.mo');
 if FileExistsUTF8(Result) then exit;

 Result:='';
end;
var lcfn:string;

{ TDefaultTranslator }

constructor TDefaultTranslator.Create(MOFileName: string);
begin
  inherited Create;
  FMOFile:=TMOFile.Create(UTF8ToSys(MOFileName));
end;

destructor TDefaultTranslator.Destroy;
begin
  FMOFile.Free;
//If someone will use this class incorrectly, it can be destroyed
//before Reader destroying. It is a very bad thing, but in THIS situation
//in this case is impossible. May be, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TDefaultTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: String;
begin
  if not Assigned(FMOFile) then exit;
  if not Assigned(PropInfo) then exit;
{DO we really need this?}
  if Instance is TComponent then
   if csDesigning in (Instance as TComponent).ComponentState then exit;
{End DO :)}
  if (AnsiUpperCase(PropInfo^.PropType^.Name)<>'TTRANSLATESTRING') then exit;
  s:=FMOFile.Translate(Content);
  if s<>'' then Content:=s;
end;

var Dot1:integer;
    LCLPath:string;
initialization
//It is safe to place code here as no form is initialized before unit
//initialization made
//We are to search for all
  try
    lcfn:=FindLocaleFileName;
  except
    lcfn:='';
  end;

  if lcfn<>'' then
  begin
    TranslateResourceStrings(UTF8ToSys(lcfn));
    LCLPath:=ExtractFileName(lcfn);
    Dot1:=pos('.',LCLPath);
    if Dot1>1 then
    begin
      Delete(LCLPath,1,Dot1-1);
      LCLPath:=ExtractFilePath(lcfn)+'lcl'+LCLPath;
      if FileExistsUTF8(LCLPath) then
        TranslateResourceStrings(UTF8ToSys(LCLPath));
    end;

    LRSTranslator:=TDefaultTranslator.Create(lcfn);

  end;
writeLn('Translation loaded.');

finalization
  LRSTranslator.Free;
end.

