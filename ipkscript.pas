{ ipkscript.pas
  Copyright (C) Listaller Project 2008-2009

  ipkscript.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  ipkscript.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Contains the TIPKScript class
unit ipkscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, litypes;

type

//** Basic IPK reader class
TIPKBasic = class
 private
  function GetValue(s: String): String;
  function SearchKeyIndex(s: String): Integer;
  function SolveInclude(s: String): String;

  procedure WriteType(atype: TPkgType);
  function  ReadType: TPkgType;
  procedure WriteName(s: String);
  function  ReadName: String;
  procedure WriteVersion(s: String);
  function  ReadVersion: String;
  procedure WriteIcon(s: String);
  function  ReadIcon: String;
  procedure WriteSDesc(s: String);
  function  ReadSDesc: String;
  procedure WriteGroup(g: GroupType);
  function  ReadGroup: GroupType;
  procedure WriteAuthor(s: String);
  function  ReadAuthor: String;
  procedure WriteMaintainer(s: String);
  function  ReadMaintainer: String;
  procedure WriteDisallows(s: String);
  function  ReadDisallows: String;
 protected
  text: TStringList;
  FBasePath: String;
  clang: String;
 public
  constructor Create;
  destructor  Destroy;override;

  function SaveToFile(s: String): Boolean;
  function LoadFromFile(s: String): Boolean;
  property BasePath: String read FBasePath write FBasePath;
  property SType: TPkgType read ReadType write WriteType;
  property AppName: String read ReadName write WriteName;
  property AppVersion: String read ReadVersion write WriteVersion;
  procedure ReadAppLicense(info: TStringList);
  procedure WriteAppLicense(path: String);
  procedure WriteAppLicense(info: TStringList);
  procedure ReadAppDescription(info: TStringList);
  procedure WriteAppDescription(path: String);
  procedure WriteAppDescription(info: TStringList);
  property Icon: String read ReadIcon write WriteIcon;
  property LangCode: String read clang write clang;
  property SDesc: String read ReadSDesc write WriteSDesc;
  property Group: GroupType read ReadGroup write WriteGroup;
  property Author: String read ReadAuthor write WriteAuthor;
  property Maintainer: String read ReadMaintainer write WriteMaintainer;
  property Disallows: String read ReadDisallows write WriteDisallows;
 end;

TIPKScript = class(TIPKBasic)
 private
 public
  constructor Create;override;
  destructor  Destroy;override;
end;

implementation

{ TIPKBasic }

constructor TIPKBasic.Create;
begin
 inherited;
 text:=TStringList.Create;
 FBasePath:=ExtractFilePath(paramstr(0));
 text.Add('IPK-Standard-Version: 1.0');
 text.Add('');
 clang:='';
end;

destructor TIPKBasic.Destroy;
begin
 text.Free;
 inherited;
end;

function TIPKBasic.SaveToFile(s: String): Boolean;
begin
result:=true;
try
 text.SaveTofile(s);
 FBasePath:=ExtractFilePath(s);
except
 Result:=false;
end;
end;

function TIPKBasic.LoadFromFile(s: String): Boolean;
begin
 result:=true;
 if FileExists(s) then
 begin
 text.LoadFromFile(s);
 if text[0]<>'IPK-Standard-Version: 1.0' then
 begin
  Result:=false;
  text.Clear;
  text.Add('IPK-Standard-Version: 1.0');
  text.Add('');
  exit;
 end;
 FBasePath:=ExtractFilePath(s);
 end else Result:=false;
end;

function TIPKBasic.GetValue(s: String): String;
begin
 Result:=LowerCase(copy(s,pos(':',s)+1,length(s)));
 if Result[1]=' ' then
  Result:=copy(Result,2,length(Result));
end;

function TIPKBasic.SearchKeyIndex(S: String): Integer;
var i: Integer;h: String;
begin
 Result:=-1;
 i:=text.Count;
 //First search for localized entry
 if clang<>'' then
 begin
 for i:=0 to text.count-1 do
 begin
  h:=copy(text[i],0,pos(':',text[i])-1);
  if LowerCase(h)=LowerCase(s)+'['+clang+']' then begin Result:=i;break;end;
 end;
 end;
 if (i=text.Count)and(LowerCase(h)<>LowerCase(s)+'['+clang+']') then
 //Then search the general key
 for i:=0 to text.count-1 do
 begin
  h:=copy(text[i],0,pos(':',text[i])-1);
  if LowerCase(h)=LowerCase(s) then begin Result:=i;break;end;
 end;
end;

function TIPKBasic.SolveInclude(s: String): String;
var h: String;
begin
 h:=copy(s,pos('"',s)+1,length(s));
 h:=copy(h,0,pos('"',s)-1);
 if h[1]='.' then
  Result:=FBasePath+'/'+h
 else
  Result:=h;
end;

procedure TIPKBasic.WriteType(atype: TPkgType);
var h: String;
begin
case AType of
 ptLinstall: h:='Type: linstall';
 ptDLink: h:='Type: dlink';
 ptContainer: h:='Type: container';
end;
if SearchKeyIndex('Type')>-1 then
 text[SearchKeyIndex('Type')]:=h
else
 text.Add(h);
end;

function TIPKBasic.ReadType: TPkgType;
var s: String;j: Integer;
begin
 Result:=ptUnknown;
 j:=SearchKeyIndex('Type');
 if j>-1 then
 begin
 s:=text[j];
 if GetValue(s)='linstall' then Result:=ptLinstall;
 if GetValue(s)='dlink' then Result:=ptDLink;
 if GetValue(s)='container' then Result:=ptContainer;
 end;
end;

procedure TIPKBasic.WriteName(s: String);
begin
s:='Name: '+s;
if SearchKeyIndex('Name')>-1 then
 text[SearchKeyIndex('Name')]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadName: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Name');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteVersion(s: String);
var k: String;
begin
if clang='' then
 k:='Version'
else
 k:='Version['+clang+']';
 s:=k+': '+s;

if SearchKeyIndex(k)>-1 then
 text[SearchKeyIndex(k)]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadVersion: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Version');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.ReadAppLicense(info: TStringList);
var i: Integer;s: String;
begin
 i:=SearchKeyIndex('License');
 s:='';
 if i>-1 then
  s:=text[i];
 info.Clear;
 if s='' then exit;
 if pos('include:"',s)>0 then
  info.LoadFromFile(SolveInclude(s))
 else
 begin
 info.Add(GetValue(text[i]));
 Inc(i);
  repeat
   s:=text[i];
   if s[1]=' ' then
    s:=copy(s,2,length(s));
   info.Add(s);
   Inc(i);
  until (i>text.Count)or(text[i][1]<>' ');
 end;
end;

procedure TIPKBasic.WriteAppLicense(path: String);
var s: String;i: Integer;
begin
 s:='License: include:"'+path+'"';

 i:=SearchKeyIndex('License');
  if i>0 then
  begin
   text.Delete(i);
   while (i<text.Count)and(text[i][1]=' ') do
    text.Delete(i);
  end;

if i>-1 then
 text[i]:=s
else
 text.Add(s);
end;

procedure TIPKBasic.WriteAppLicense(info: TStringList);
var i: Integer;
begin
 if info.Count>=0 then
 begin
  i:=SearchKeyIndex('License');
  if i>0 then
  begin

  text.Delete(i);
  while (i<text.Count)and(text[i]<>'')and(text[i][1]=' ') do
   text.Delete(i);
  end;

 text.Add('License: '+info[0]);
 for i:=1 to info.Count-1 do
  text.Add(' '+info[i]);

 end;
end;

procedure TIPKBasic.ReadAppDescription(info: TStringList);
var i: Integer;s: String;
begin
 i:=SearchKeyIndex('Description');
 s:='';
 if i>-1 then
  s:=text[i];
 info.Clear;
 if s='' then exit;
 if pos('include:"',s)>0 then
  info.LoadFromFile(SolveInclude(s))
 else
 begin
 info.Add(GetValue(text[i]));
 Inc(i);
  repeat
   s:=text[i];
   if s[1]=' ' then
    s:=copy(s,2,length(s));
   info.Add(s);
   Inc(i);
  until (i>text.Count)or(text[i][1]<>' ');
 end;
end;

procedure TIPKBasic.WriteAppDescription(path: String);
var s: String;i: Integer;
begin
 s:='Description: include:"'+path+'"';

 i:=SearchKeyIndex('Description');
  if i>0 then
  begin
   text.Delete(i);
   while (i<text.Count)and(text[i][1]=' ') do
    text.Delete(i);
  end;

if i>-1 then
 text[i]:=s
else
 text.Add(s);
end;

procedure TIPKBasic.WriteAppDescription(info: TStringList);
var i: Integer;
begin
 if info.Count>=0 then
 begin
  i:=SearchKeyIndex('Description');
  if i>0 then
  begin

  text.Delete(i);
  while (i<text.Count)and(text[i]<>'')and(text[i][1]=' ') do
   text.Delete(i);
  end;

 text.Add('Description: '+info[0]);
 for i:=1 to info.Count-1 do
  text.Add(' '+info[i]);

 end;
end;

procedure TIPKBasic.WriteIcon(s: String);
begin
s:='Icon: '+s;
if SearchKeyIndex('Version')>-1 then
 text[SearchKeyIndex('Version')]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadIcon: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Icon');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteSDesc(s: String);
var k: String;
begin
if clang='' then
 k:='SDesc'
else
 k:='SDesc['+clang+']';

s:=k+': '+s;
if SearchKeyIndex(k)>-1 then
 text[SearchKeyIndex(k)]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadSDesc: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('SDesc');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteGroup(g: GroupType);
var s: String;
begin
case g of
 gtALL: s:='All';
 gtEDUCATION: s:='Education';
 gtOFFICE: s:='Office';
 gtDEVELOPMENT: s:='Development';
 gtGRAPHIC: s:='Graphic';
 gtNETWORK: s:='Network';
 gtGAMES: s:='Games';
 gtSYSTEM: s:='System';
 gtMULTIMEDIA: s:='Multimedia';
 gtADDITIONAL: s:='Additional';
 gtOTHER: s:='Other';
end;
s:='Group: '+s;

if SearchKeyIndex('Group')>-1 then
 text[SearchKeyIndex('Group')]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadGroup: GroupType;
var j: Integer;s: String;
begin
 Result:=gtUNKNOWN;
 j:=SearchKeyIndex('SDesc');
 if j>-1 then
  s:=GetValue(text[j]);

  s:=LowerCase(s);
 if s='all' then Result:=gtALL;
 if s='education' then Result:=gtEDUCATION;
 if s='office' then Result:=gtOFFICE;
 if s='development' then Result:=gtDEVELOPMENT;
 if s='graphic' then Result:=gtGRAPHIC;
 if s='network' then Result:=gtNETWORK;
 if s='games' then Result:=gtGAMES;
 if s='system' then Result:=gtSYSTEM;
 if s='multimedia' then Result:=gtMULTIMEDIA;
 if s='additional' then Result:=gtADDITIONAL;
 if s='other' then Result:=gtOTHER;
end;

procedure TIPKBasic.WriteAuthor(s: String);
var k: String;
begin
if clang='' then
 k:='Author'
else
 k:='Author['+clang+']';

s:=k+': '+s;
if SearchKeyIndex(k)>-1 then
 text[SearchKeyIndex(k)]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadAuthor: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Author');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteMaintainer(s: String);
var k: String;
begin
if clang='' then
 k:='Maintainer'
else
 k:='Maintainer['+clang+']';

s:=k+': '+s;
if SearchKeyIndex(k)>-1 then
 text[SearchKeyIndex(k)]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadMaintainer: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Maintainer');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteDisallows(s: String);
var k: String;
begin
 k:='Disallow'
s:=k+': '+s;
if SearchKeyIndex(k)>-1 then
 text[SearchKeyIndex(k)]:=s
else
 text.Add(s);
end;

function TIPKBasic.ReadDisallows: String;
var j: Integer;
begin
 Result:='';
 j:=SearchKeyIndex('Disallow');
 if j>-1 then
  Result:=GetValue(text[j]);
end;

procedure TIPKBasic.WriteProfiles(lst: TStrings);
var k: String;
begin

end;

procedure TIPKBasic.ReadProfiles(lst: TStrings);
var j: Integer;
begin

end;

{ TIPKScript }

constructor TIPKScript.Create;
begin
 inherited;
end;

destructor TIPKScript.Destroy;
begin
 inherited;
end;

end.

