(* Copyright (C) 2010 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Update Pascal bindings from library source
unit pasbind;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, PasTree, PScanner, SysUtils;

type
  TPasLibBindGen = class
  private
    FLibName: string;
    FName: string;
    FOutName: string;
    function ReplaceTypes(s: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run(component: string);
    property LibName: string read FLibName write FLibName;
    property LibMainSrcFile: string read FName write FName;
    property BindOutFile: string read FOutName write FOutName;
  end;

implementation

{ TPasLibBindGen }

constructor TPasLibBindGen.Create;
begin

end;

destructor TPasLibBindGen.Destroy;
begin
  inherited;
end;

function TPasLibBindGen.ReplaceTypes(s: String): String;
begin
  Result := StrSubst(s, ': TLiAppItem', ': LiAppItem');
  Result := StrSubst(Result, ': TLiAppManager', ': LiAppManager');
  Result := StrSubst(Result, ': TLiInstallation', ': LiInstallation');
  Result := StrSubst(Result, ': TLiAppUpdater', ': LiAppUpdater');
end;

procedure TPasLibBindGen.Run(component: string);
var
  src: TStringList;
  exported: TStringList;
  res: TStringList;
  i: integer;
  r, h: string;
  catchNext: boolean;
begin
  src := TStringList.Create;
  src.LoadFromFile(FName);

  exported := TStringList.Create;
  for i := src.IndexOf('exports') to src.Count - 1 do
  begin
    exported.Add(LowerCase(StrSubst(StrSubst(StrSubst(src[i], ' ', ''), ',', ''),
      ';', '')));
    if (length(src[i]) > 2) and (LowerCase(src[i][length(src[i])]) = ';') then
      break;
  end;

  res := TStringList.Create;
  catchNext := False;
  for i := src.IndexOf('{@' + component + '}') + 1 to src.Count -1 do
  begin
    r := '';
    if pos('{@', src[i]) > 0 then
      break;
    if StringReplace(src[i], ' ', '', [rfReplaceAll]) = '' then
      Continue;
    if catchNext then
    begin
      r := src[i];
      res.Add(ReplaceTypes(r));
      r := StringReplace(r, ' ', '', [rfReplaceAll]);
      if r[length(r)] <> ';' then
        catchNext := True
      else
      begin
        catchNext := False;
        res[res.Count - 1] := res[res.Count - 1] + 'external ' + FLibName + ';';
      end;
    end;
    if ((pos('function', src[i]) > 0) or (pos('procedure', src[i]) > 0)) and
      (src[i][2] <> '/') then
    begin
      r := src[i];
      //Extract function name
      h := copy(r, pos(' ', r) + 1, length(r));
      if pos('(', r) <= 0 then
        h := copy(h, 1, pos(':', h) - 1)
      else
        h := copy(h, 1, pos('(', h) - 1);

      if StringReplace(h, ' ', '', [rfReplaceAll]) = '' then
        Continue;
      if exported.IndexOf(h) > 0 then
      begin
        res.Add(ReplaceTypes(r));
        r := StringReplace(r, ' ', '', [rfReplaceAll]);
        if r[length(r)] <> ';' then
          catchNext := True
        else
          res[res.Count - 1] := res[res.Count - 1] + 'external ' + FLibName + ';';
      end;
    end;
  end;
  exported.Free;
  src.LoadFromFile(FOutName);

  i := src.IndexOf('{@Begin:' + component + '}') + 1;
  repeat
    src.Delete(i);
    if i > src.Count - 1 then
    begin
      writeLn('Invalid output template!');
      writeLn(' Did you forget the "End"-Template?');
      halt(2);
    end;
  until pos('{@End:' + component + '}', src[i]) > 0;

  if StringReplace(res[0], ' ', '', [rfReplaceAll]) <> '' then
    res.Insert(0, '');

  i := src.IndexOf('{@Begin:' + component + '}');
  src.Insert(i + 1, res.Text);
  res.Free;

  //Write file back
  src.SaveToFile(FOutName);
  src.Free;
end;

end.

