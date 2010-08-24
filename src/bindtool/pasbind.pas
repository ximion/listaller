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
//** Update Pascal bindings from library source
unit pasbind;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, PasTree, PParser, SysUtils, PScanner;

type
//Simple, stupid dummy engine
TSimplePTPEngine = class(TPasTreeContainer)
public
  function CreateElement(AClass: TPTreeElement; const AName: String;
    AParent: TPasElement; AVisibility: TPasMemberVisibility;
    const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
    override;
  function FindElement(const AName: String): TPasElement; override;
end;

TPasLibBindGen = class
private
  FLibName: String;
  FName: String;
public
  constructor Create;
  destructor Destroy; override;

  procedure Run;
  property LibName: String read FLibName write FLibName;
  property LibMainSrcFile: String read FName write FName;
end;

implementation

function TSimplePTPEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimplePTPEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;

{ TPasLibBindGen }

constructor TPasLibBindGen.Create;
begin

end;

destructor TPasLibBindGen.Destroy;
begin
  inherited;
end;

procedure TPasLibBindGen.Run;
var
  md: TPasModule;
  eng: TSimplePTPEngine;
  i: Integer;
  Decls: TList;
  element: TPasElement;
  func, h, x: String;
  res: TStringList;
  src: TStringList;
begin
  eng := TSimplePTPEngine.Create;
  writeLn('A');
  md := ParseSource(eng, fname, 'linux', '');
  src := TStringList.Create;
  src.LoadFromFile(fname);

  writeLn('B');
  decls := md.InterfaceSection.Declarations;

  for i := 0 to Decls.Count - 1 do
  begin
    element := (TObject(Decls[i]) as TPasElement);
    writeLn(element.GetDeclaration(false));
  end;
end;

end.

