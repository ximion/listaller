(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 * Matthias Klumpp
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as publishedf by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
unit pkpasbind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LiUtils;

type
  TPKHeader2Pas = class
  private
    proc: TProcess;
    h2pas: String;
    infile, outfile: String;

    procedure SetInFile(fi: String);
  protected
    uText: TStringList;

    procedure PerformH2PAS;
    procedure RemoveCruft;
    procedure ApplyFixes;
    procedure RemoveDuplicates;
    procedure ExperimentalChanges;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;

    property InputFile: String read infile write SetInFile;
    property OutputFile: String read outfile write outfile;
  end;

implementation

uses PackageKit;

{ TPKHeader2Pas }

constructor TPKHeader2Pas.Create;
begin
  inherited;
  h2pas := 'h2pas -d -i -p -C -c' + ' ';
  proc := TProcess.Create(nil);
  proc.Options := [poUsePipes, poStdErrToOutput, poWaitOnExit];
  uText := TStringList.Create;
end;

destructor TPKHeader2Pas.Destroy;
begin
  uText.Free;
  proc.Free;
  inherited;
end;

procedure TPKHeader2Pas.SetInFile(fi: String);
begin
  uText.Clear;
  infile := fi;
end;

procedure TPKHeader2Pas.PerformH2PAS;
var
  fo: String;
  tmp: TStringList;
  i: Integer;
  add: Boolean;
begin
  tmp := TStringList.Create;
  fo := BaseTmpDir + 'pkunit.inc';
  proc.CommandLine := h2pas + '-o ' + fo + ' ' + infile;
  proc.Execute;
  tmp.LoadFromFile(fo);
  DeleteFile(fo);
  add := false;
  uText.Clear;

  // TMP
  add := true;
  for i := 0 to tmp.Count - 1 do
  begin
    if pos('{ $IFDEF FPC }', tmp[i]) > 0 then
      add := true;
    if add then
      uText.Add(tmp[i]);
  end;
  tmp.Free;

  uText.Insert(0, '// Part of Listaller PackageKit bindings //');
end;

procedure TPKHeader2Pas.RemoveCruft;
var
  i: Integer;
  removeendif: Boolean;

  function xpos(str: String): Integer;
  begin
    Result := pos(str, uText[i]);
  end;

begin
  i := 0;
  removeendif := false;
  while i < uText.Count do
  begin
    uText[i] := StrSubst(uText[i], 'external;', 'external pklib2;');
    if (xpos('{$endif}') > 0) and (removeendif) then
    begin
      uText.Delete(i);
      removeendif := false;
    end
    else if xpos('{$if !') > 0 then
    begin
      uText.Delete(i);
      removeendif := true;
    end
    else if xpos('{$include') > 0 then
      uText.Delete(i)
    else if xpos('{$error') > 0 then
      uText.Delete(i)
    else if xpos('function PK_') > 0 then
      uText.Delete(i)
    else if (xpos('_') > 0) and (xpos(' = ') > 0) and (xpos('(') <= 0) then
      uText.Delete(i)
    else
    //Temp helper classes
    if (xpos('parent : GObject') > 0) or (xpos('priv : ') > 0) or
      (xpos('parent_class : GObjectClass;') > 0) or (xpos(': procedure ;') > 0) or
      (xpos(': procedure') > 0) then
      uText.Delete(i)
    else if (pos(':guint;', StrSubst(uText[i], ' ', '')) > 0) or
      (pos(':gchar;', StrSubst(uText[i], ' ', '')) > 0) then
      uText.Delete(i)
    else if xpos('  var') > 0 then
      uText.Delete(i)
    else
      Inc(i);
  end;
end;

procedure TPKHeader2Pas.ApplyFixes;
var
  i: Integer;
  lasterrpos: Integer;
begin
  RemoveCruft;
  for i := 0 to uText.Count - 1 do
  begin
    if pos('= ^', uText[i]) > 0 then
      uText[i] := copy(uText[i], 1, pos('= ', uText[i])) + ' Pointer;';
    if pos('(* error', uText[i]) > 0 then
      lasterrpos := i;
  end;

  // Fix comment-line closing
  for i := lasterrpos to uText.Count - 1 do
    if pos('*)', uText[i]) > 0 then
    begin
      lasterrpos := -1;
      break;
    end;
  i := lasterrpos;
  if lasterrpos > 0 then
    while i < uText.Count do
    begin
      uText.Delete(i);
    end;
end;

procedure TPKHeader2Pas.RemoveDuplicates;
var
  tmp: TStringList;
  i: Integer;
  s: String;
begin
  i := 0;
  s := '';
  tmp := TStringList.Create;
  while i < uText.Count do
  begin
    if pos(' = ', uText[i]) > 0 then
    begin
      s := StrSubst(copy(uText[i], 1, pos('=', uText[i]) - 1), ' ', '');
      s := LowerCase(s);
      if tmp.IndexOf(s) > 0 then
        uText.Delete(i)
      else
      begin
        tmp.Add(s);
        Inc(i);
      end;
    end
    else
      Inc(i);
  end;
  tmp.Free;
end;

procedure TPKHeader2Pas.ExperimentalChanges;
var
  i: Integer;
begin
  i := 0;
  while i < uText.Count do
  begin
    if pos(' end dygd;', uText[i]) > 0 then
      uText.Delete(i)
    else if pos(' typesv', LowerCase(uText[i])) > 0 then
      uText.Delete(i)
    else if LowerCase(StrSubst(uText[i], ' ', '')) = 'const' then
      uText.Delete(i)
    else
      Inc(i);
  end;
end;

procedure TPKHeader2Pas.Execute;
begin
  // Do the initial conversion
  PerformH2PAS;
  ApplyFixes;
  RemoveDuplicates;
  ExperimentalChanges;
  uText.SaveToFile(outfile);
end;

end.

