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
//** Helper for generating C-Headers from Pascal library source
unit pascconv;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, PasTree, PParser, SysUtils;

type
  //Simple, stupid dummy engine
  TSimplePTCEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  TPtCConverter = class
  private
    funcdecl: TStringList;
    function SolveArguments(arg: String): String;
    function ProcessClassDef(el: TPasElement; res: TStringList): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ConvertToCInfo(fname: String): TStringList;
  end;

implementation

function TSimplePTCEngine.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimplePTCEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;

function SubstTypes(s: String): String;
var
  h: String;
begin
  h := LowerCase(s);
  //Workarounds
  if h = 'tstringlist' then
  begin
    Result := 'StringList *';
    exit;
  end;
  //Some listaller types to replace
  if h = 'liappitem' then
  begin
    Result := 'LiAppItem *';
    exit;
  end;
  if h = 'liappmanager' then
  begin
    Result := 'LiAppManager *';
    exit;
  end;
  if h = 'liinstallation' then
  begin
    Result := 'LiInstallation *';
    exit;
  end;
  if h = 'liappupdater' then
  begin
    Result := 'LiAppUpdater *';
    exit;
  end;
  if h = 'tobject' then
  begin
    Result := 'void* ';
    exit;
  end;

  if h = 'pointer' then
  begin
    Result := 'void* ';
    exit;
  end;
  if h[1] = 'p' then
  begin
    h := copy(s, 2, length(s));
    if (LowerCase(h) = 'char') or (LowerCase(h) = 'gchar') or
      (LowerCase(h) = 'liappinfo') or (LowerCase(h) = '<placeholder>') then  //FIXME
      if LowerCase(h) = 'liappinfo' then
        Result := h + ' *'
      else
        Result := LowerCase(h) + ' *'
    else
      Result := s;
  end
  else
  if (h = 'integer') or (h = 'longint') or (h = 'shortint') or (h = 'smallint') then
    Result := 'int'
  else if h[1] = 'g' then
    Result := h
  else if h = 'double' then
    Result := h
  else if h = 'boolean' then
    Result := 'bool'
  else if h = 'tdatetime' then
    Result := 'double'
  else if h = 'widestring' then
    Result := 'char *'
  else
    Result := s;
end;

{ TPtCConverter }

constructor TPtCConverter.Create;
begin
  funcdecl := TStringList.Create;
end;

destructor TPtCConverter.Destroy;
begin
  funcdecl.Free;
  inherited;
end;

function TPtCConverter.SolveArguments(arg: String): String;
var
  f, g, r: String;
  ty: Boolean;
  i: Integer;

  procedure RemoveSpaces;
  begin
    g := StrSubst(g, ' ', '');
    f := StrSubst(f, ' ', '');
  end;

  procedure UpdateRVal;
  begin
    if pos('const ', f) > 0 then
     begin
       r := r + 'const ';
       f := StrSubst(f, 'const ', '');
     end;
    RemoveSpaces;
    g := SubstTypes(g);
    if pos('*', g) <= 0 then
      r := r + g + ' ' + f
    else
      r := r + g + f;
    if i <> length(arg) then
      r := r + ',';
  end;

begin
  arg := StrSubst(arg, #10, '');
  arg := StrSubst(arg, #13, '');
  r := '';
  ty := false;
  for i := 1 to length(arg) do
  begin
    if arg[i] = ':' then
      ty := not ty
    else
    if arg[i] = ';' then
    begin
      UpdateRVal();
      g := '';
      f := '';
      ty := not ty;
    end
    else
      if not ty then
        f := f + arg[i]
      else
        g := g + arg[i];
  end;
  //Add last parameters (no last ; appeared)
  if (g <> '') and (f <> '') then
    UpdateRVal;
  Result := r;
end;

function TPtCConverter.ProcessClassDef(el: TPasElement; res: TStringList): Boolean;
var
  eltype: String;
  h, r: String;

  procedure ArgumentsToHVar;
  begin
    h := StrSubst(el.GetDeclaration(false), #10, '');
    //h := StrSubst(h, ' ', '');
    h := copy(h, pos('(', h) + 1, length(h));
    h := copy(h, 1, pos(')', h) - 1);
    h := SolveArguments(h);
  end;

begin
  Result := false;
  eltype := LowerCase(el.ElementTypeName);
  if pos('type', eltype) <= 0 then
    exit;
  Result := true;
  if (pos('alias type', eltype) > 0) and (LowerCase(el.GetDeclaration(false)) =
    'pointer') then
  begin
    r := 'typedef void ';
    h := el.GetDeclaration(true);
    h := copy(h, 1, pos(' ', h) - 1);
    r := r + h + ';';
    res.Add('');
    res.Add(r);
  end
  else
  if pos('enumeration', eltype) > 0 then
  begin
    r := 'typedef enum {'#10;
    h := el.GetDeclaration(true);
    h := StrSubst(h, #10, '');
    h := StrSubst(h, ' ', '');
    h := copy(h, pos('(', h) + 1, length(h));
    h := copy(h, 1, pos(')', h) - 1);
    h := StrSubst(h, ',', ','#10'      ');
    h := '      ' + h;
    r := r + h + #10 + '} ' + el.FullName + ';';
    res.Add('');
    res.Add(r);
  end
  else
  if pos('procedure', eltype) > 0 then
  begin
    r := 'typedef void (*' + el.FullName + ') ';
    ArgumentsToHVar; //Quick & dirty helper function :)
    r := r + '(' + h + ');';
    funcdecl.Add('');
    funcdecl.Add(r);
  end
  else
  if pos('function', eltype) > 0 then
  begin
    //Fetch return type
    h := el.GetDeclaration(false);
    h := copy(h, pos(')', h) + 1, length(h));
    h := copy(h, pos(':', h) + 1, length(h));
    h := StrSubst(h, ' ', '');
    h := SubstTypes(h);

    r := 'typedef ' + h + ' (*' + el.FullName + ') ';
    ArgumentsToHVar; //Quick & dirty helper function :)
    r := r + '(' + h + ');';
    funcdecl.Add('');
    funcdecl.Add(r);
  end
  else
  if pos('record', eltype) > 0 then
  begin
    h := el.GetDeclaration(false);
    h := copy(h, 7, length(h)); //remove the "record" word
    Delete(h, length(h) - 3, length(h)); //remove "end" from the end
    h := SolveArguments(h); //SolveArguments() works here too!

    r := 'typedef struct {'#10;
    h := StrSubst(h, ',', ';'#10'      ');
    r := r + '      ' + h + ';'#10'} ' + el.FullName + ';';
    res.Add('');
    res.Add(r);
  end;
end;

function TPtCConverter.ConvertToCInfo(fname: String): TStringList;
var
  md: TPasModule;
  eng: TSimplePTCEngine;
  i: Integer;
  Decls: TList;
  element: TPasElement;
  func, h, x: String;
  res: TStringList;
  src: TStringList;
begin
  Result := nil;
  eng := TSimplePTCEngine.Create;
  md := ParseSource(eng, fname, 'linux', '');
  src := TStringList.Create;
  src.LoadFromFile(fname);

  funcdecl.Clear;
  //funcdecl.Add('G_BEGIN_DECLS');

  decls := md.InterfaceSection.Declarations;
  res := TStringList.Create;
  for i := 0 to Decls.Count - 1 do
  begin
    element := (TObject(Decls[I]) as TPasElement);

    if not ProcessClassDef(element, res) then
      if pos('external', LowerCase(src[element.SourceLinenumber-1])) > 0 then
        if element.ElementTypeName = 'function' then
        begin
          res.Add('');
          func := element.GetDeclaration(true);

          //Extract return type
          h := copy(func, pos(')', func) + 1, length(func));
          h := copy(h, pos(':', h) + 1, length(h));
          h := StrSubst(h, ' ', '');

          h := SubstTypes(h);
          if pos('*', h) <= 0 then
            x := h + ' '
          else
            x := h;

          x := x + element.Name;

          if pos('(', func) > 0 then
          begin
            h := copy(func, pos('(', func) + 1, length(func));
            h := copy(h, 1, pos(')', h) - 1);
            x := x + '(' + SolveArguments(h) + ');';
          end
          else
            x := x + '(void);';

          res.Add(x);
        end
        else
        if element.ElementTypeName = 'procedure' then
        begin
          res.Add('');
          func := element.GetDeclaration(true);

          x := 'void ';
          x := x + element.Name;

          if pos('(', func) > 0 then
          begin
            h := copy(func, pos('(', func) + 1, length(func));
            h := copy(h, 1, pos(')', h) - 1);
            x := x + '(' + SolveArguments(h) + ');';
          end
          else
            x := x + '(void);';

          res.Add(x);
        end;
  end;
  if funcdecl.Count > 2 then
  begin
    //funcdecl.Add('');
    //funcdecl.Add('G_END_DECLS');
    res.Add('');
    res.Add(funcdecl.Text);
  end;

  src.Free;
  FreeAndNil(md);
  FreeAndNil(eng);
  Result := res;
end;

end.

