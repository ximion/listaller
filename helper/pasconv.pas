{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Helper for generating C-Headers from Pascal library source
unit pasconv;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, PasTree, PParser, SysUtils;

type
  //Simple, stupid dummy engine
  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

function ConvertToCInfo(fname: String): TStringList;

implementation

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;

function SolveArguments(arg: String): String;
var
  f, g, r: String;
  ty: Boolean;
  i: Integer;

  procedure UpdateRVal;
  begin
    if g[1] = 'p' then
      r := r + copy(g, 2, length(g)) + ' *' + f
    else
      r := r + g + ' ' + f;
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
        if arg[i] <> ' ' then
          if not ty then
            f := f + arg[i]
          else
            g := g + LowerCase(arg[i]);
  end;
  //No ; appeared
  if (g <> '') and (f <> '') then
    UpdateRVal;
  Result := r;
end;

function ConvertToCInfo(fname: String): TStringList;
var
  md: TPasModule;
  eng: TSimpleEngine;
  I: Integer;
  Decls: TList;
  element: TPasElement;
  func, h, x: String;
  res: TStringList;
  src: TStringList;
begin
  Result := nil;
  eng := TSimpleEngine.Create;
  md := ParseSource(eng, fname, 'linux', '');
  src := TStringList.Create;
  src.LoadFromFile(fname);

  decls := md.InterfaceSection.Declarations;
  res := TStringList.Create;
  for I := 0 to Decls.Count - 1 do
  begin
    element := (TObject(Decls[I]) as TPasElement);

    if pos('external', LowerCase(src[element.SourceLinenumber])) > 0 then
      if element.ElementTypeName = 'function' then
      begin
        res.Add('');
        func := element.GetDeclaration(true);

        //Extract resturn type
        h := copy(func, pos(')', func) + 1, length(func));
        h := copy(h, pos(':', h) + 1, length(h));
        h := StrSubst(h, ' ', '');

        if LowerCase(h[1]) = 'p' then
          x := copy(h, 2, length(h)) + ' *'
        else
          x := h + ' ';

        x := LowerCase(x) + element.Name;

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
  src.Free;
  FreeAndNil(md);
  FreeAndNil(eng);
  Result := res;
end;

end.

