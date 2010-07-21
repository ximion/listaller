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

function SubstTypes(s: String): String;
var
  h: String;
begin
  h := LowerCase(s);
  if h[1] = 'p' then
  begin
    h := copy(h, 2, length(h)) + ' *';
    Result := h;
    exit;
  end;
  if h = 'integer' then
    Result := 'int'
  else if h[1] = 'g' then
      Result := h
    else
      Result := s;
end;

function SolveArguments(arg: String): String;
var
  f, g, r: String;
  ty: Boolean;
  i: Integer;

  procedure UpdateRVal;
  begin
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
        if arg[i] <> ' ' then
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

function ProcessClassDef(el: TPasElement; res: TStringList): Boolean;
var
  eltype: String;
  h, r: String;

  procedure ArgumentsToHVar;
  begin
    h := StrSubst(el.GetDeclaration(false), #10, '');
    h := StrSubst(h, ' ', '');
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
      r := 'typedef void (*' + el.FullName + 'GAsyncReadyCallback) ';
      ArgumentsToHVar; //Quick, dirty helper function :)
      r := r + '{(' + h + ');}';
      res.Add('');
      res.Add(r);
    end
    else
      if pos('function', eltype) > 0 then
      begin
        //Fetch return type
        h := el.GetDeclaration(false);
        h := copy(h, pos(')', h) + 1, length(h));
        h := copy(h, pos(':', h) + 1, length(h));
        h := StrSubst(h, ' ', '');

        r := 'typedef ' + h + ' (*' + el.FullName + 'GAsyncReadyCallback) ';
        ArgumentsToHVar; //Quick, dirty helper function :)
        r := r + '{(' + h + ');}';
        res.Add('');
        res.Add(r);
      end
      else
        if pos('record', eltype) > 0 then
        begin
          h := el.GetDeclaration(false);
          h := copy(h, 7, length(h)); //remove the "record" word
          Delete(h, length(h) - 3, length(h)); //remove "end" from the end
          h := SolveArguments(h); //SolveArguments() works here too!

          r := 'struct ' + el.FullName + #10'{'#10;
          h := StrSubst(h, ',', ';'#10'      ');
          r := r + '      ' + h + ';'#10'};';
          res.Add('');
          res.Add(r);
        end;
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

    if not ProcessClassDef(element, res) then
      if pos('external', LowerCase(src[element.SourceLinenumber])) > 0 then
        if element.ElementTypeName = 'function' then
        begin
          res.Add('');
          func := element.GetDeclaration(true);

          //Extract resturn type
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

