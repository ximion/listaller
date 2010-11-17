(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 * Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as publishedf by the Free Software
 * Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Helper for generating updating/generating Listaller's library bindings
program bindingtool;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, PasCConv, LiUtils, PasBind, PKPasBind;

type

  { TBindGenerator }

  TBindGenerator = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TBindGenerator }

procedure TBindGenerator.DoRun;
var
  ErrorMsg: String;
  test: TStringList;
  tmpl: TStringList;
  hname: String;
  ccv: TPtCConverter;
  plg: TPasLibBindGen;
  pkh2pas: TPKHeader2Pas;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hioc',['help', 'pastoc', 'pasbind', 'pkheader', 'template:', 'cmp:']);
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('i','in') then
  begin
    if not FileExists(GetOptionValue('i', 'in')) then
    begin
      writeLn('No source file found!');
      halt(1);
    end;
  end;

  if HasOption('o','out') then
  begin
    if GetOptionValue('o', 'out') = '' then
    begin
      writeLn('No ouput file specified!');
      halt(1);
    end;
  end;

  if HasOption('c','cmp') then
  begin
    if GetOptionValue('c', 'cmp') = '' then
    begin
      writeLn('No component specified!');
      halt(1);
    end;
  end;

  if HasOption('template') then
  begin
    if not FileExists(GetOptionValue('template')) then
    begin
      writeLn('No template set!');
      halt(1);
    end;
  end;

  // Create C-Bindings for Listaller from Pascal binding
  if HasOption('pastoc') then
  if (FileExists(GetOptionValue('i', 'in')))and(HasOption('o','out')) then
  begin
    tmpl := TStringList.Create;
    if FileExists(GetOptionValue('template')) then
     tmpl.LoadFromFile(GetOptionValue('template'));

    ccv := TPtCConverter.Create;
    test := ccv.ConvertToCInfo(GetOptionValue('i', 'in'));
    ccv.Free;

    if tmpl.IndexOf('@DECL@') > 0 then
     tmpl[tmpl.IndexOf('@DECL@')] := test.Text
    else
     tmpl.Add(test.Text);

    hname := StrSubst(ExtractFileName(GetOptionValue('o', 'out')),'-','_');
    hname := StrSubst(hname, '.h', '');
    hname := '__'+UpperCase(hname);

    if tmpl.IndexOf('@BEGIN_DEFINE_ID@') > 0 then
     tmpl[tmpl.IndexOf('@BEGIN_DEFINE_ID@')] := '#ifndef '+hname+#10+'#define '+hname;

    if tmpl.IndexOf('@END_DEFINE_ID@') > 0 then
     tmpl[tmpl.IndexOf('@END_DEFINE_ID@')] := '#endif /* '+hname+' */';

    test.Free;

    tmpl.SaveToFile(GetOptionValue('o', 'out'));
    tmpl.Free;
  end;

  // Create Pascal binding from Pascal source
  if HasOption('pasbind') then
  if (FileExists(GetOptionValue('i', 'in'))) then
  begin
    plg := TPasLibBindGen.Create;
    plg.LibMainSrcFile := GetOptionValue('i', 'in');
    plg.BindOutFile := GetOptionValue('o', 'out');
    plg.LibName:='liblistaller';
    plg.Run(GetOptionValue('c','cmp'));
    plg.Free;
  end;

  // Generate Pascal binding from PackageKit C Header
  if HasOption('pkheader') then
  if (FileExists(GetOptionValue('i', 'in')))and(HasOption('o','out')) then
  begin
    pkh2pas := TPKHeader2Pas.Create;
    pkh2pas.InputFile := GetOptionValue('i', 'in');
    pkh2pas.OutputFile := GetOptionValue('o', 'out');
    pkh2pas.Execute;
    pkh2pas.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TBindGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBindGenerator.Destroy;
begin
  inherited Destroy;
end;

procedure TBindGenerator.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TBindGenerator;

{$R *.res}

begin
  Application:=TBindGenerator.Create(nil);
  Application.Title:='make-bind';
  Application.Run;
  Application.Free;
end.

