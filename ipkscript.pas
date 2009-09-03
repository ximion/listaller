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
  Classes, SysUtils, Dialogs;

type
//** Listaller package types
TPkgType = (ptLinstall, ptDLink, ptContainer, ptUnknown);

//** A IPK script file
TIPKScript = class
 private
  text: TStringList;
  function GetValue(s: String;nospace: Boolean=true): String;
  function SearchKeyIndex(s: String): Integer;


  procedure WriteType(atype: TPkgType);
  function  ReadType: TPkgType;


 public
  constructor Create;
  destructor  Destroy;override;

  procedure SaveToFile(s: String);
  procedure LoadFromFile(s: String);
  property SType: TPkgType read ReadType write WriteType;
 end;

implementation

{ TIPKScript }

constructor TIPKScript.Create;
begin
 inherited;
 text:=TStringList.Create;
end;

destructor TIPKScript.Destroy;
begin
 text.Free;
 inherited;
end;

procedure TIPKScript.SaveToFile(s: String);
begin
 text.SaveTofile(s);
end;

procedure TIPKScript.LoadFromFile(s: String);
begin
 text.LoadFromFile(s);
end;

function TIPKScript.GetValue(s: String;nospace: Boolean=true): String;
begin
 Result:=LowerCase(copy(s,pos(':',s)+1,length(s)));
 if NoSpace then
  Result:=StringReplace(Result,' ','',[rfReplaceAll]);
end;

function TIPKScript.SearchKeyIndex(S: String): Integer;
var i: Integer;h: String;
begin
 Result:=-1;
 for i:=0 to text.count-1 do
 begin
  h:=copy(text[i],0,pos(':',text[i])-1);
  if LowerCase(h)=LowerCase(s) then begin Result:=i;break;end;
 end;
end;

procedure TIPKScript.WriteType(atype: TPkgType);
var h: String;
begin
case AType of
 ptLinstall: h:='Type: linstall';
 ptDLink: h:='Type: dlink';
 ptContainer: h:='Type: container';
end;
if text.IndexOf('Type:')>-1 then
 text[text.IndexOf('Type:')]:=h
else
 text.Add(h);
end;

function TIPKScript.ReadType: TPkgType;
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

end.

