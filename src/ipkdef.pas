{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this unit. If not, see <http://www.gnu.org/licenses/>.}
//** Contains classes to process IPK files
unit ipkdef;

{$mode objfpc}{$H+}

interface

uses
  Classes, liBasic, liTypes, SysUtils;

type

  //** Basic IPK reader class
  TIPKBasic = class
  private
    function GetValue(s: String): String;
    function SearchKeyIndex(s: String; localized: Boolean = true): Integer;
    function SolveInclude(s: String): String;
    procedure WriteEntry(k, s: String);

    procedure WriteType(atype: TPkgType);
    function ReadType: TPkgType;
    procedure WriteName(s: String);
    function ReadName: String;
    procedure WriteVersion(s: String);
    function ReadVersion: String;
    procedure WriteIcon(s: String);
    function ReadIcon: String;
    procedure WriteSDesc(s: String);
    function ReadSDesc: String;
    procedure WriteGroup(g: TGroupType);
    function ReadGroup: TGroupType;
    procedure WriteAuthor(s: String);
    function ReadAuthor: String;
    procedure WriteMaintainer(s: String);
    function ReadMaintainer: String;
    procedure WriteDisallows(s: String);
    function ReadDisallows: String;
    procedure WriteAppCMD(s: String);
    function ReadAppCMD: String;
    procedure WriteArchs(s: String);
    function ReadArchs: String;
    procedure WritePkgName(s: String);
    function ReadPkgName: String;
    procedure WriteIPKName(s: String);
    function ReadIPKName: String;
    procedure WriteDSupport(s: String);
    function ReadDSupport: String;
    procedure WriteWizImage(s: String);
    function ReadWizImage: String;
    procedure WriteBinary(s: String);
    function ReadBinary: String;
    procedure WriteUSource(s: String);
    function ReadUSource: String;
    procedure WriteDesktopFiles(s: String);
    function ReadDesktopFiles: String;
    procedure WriteInTerminal(b: Boolean);
    function ReadInTerminal: Boolean;
  protected
    Text: TStringList;
    FBasePath: String;
    clang: String;
    procedure WriteField(Name: String; info: TStrings);
    procedure ReadField(Name: String; info: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

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
    property Group: TGroupType read ReadGroup write WriteGroup;
    property Author: String read ReadAuthor write WriteAuthor;
    property Maintainer: String read ReadMaintainer write WriteMaintainer;
    property Disallows: String read ReadDisallows write WriteDisallows;
    procedure ReadProfiles(lst: TStrings);
    procedure WriteProfiles(lst: TStrings);
    procedure ReadBuildCMDs(lst: TStrings);
    procedure WriteBuildCMDs(lst: TStrings);
    property AppCMD: String read ReadAppCMD write WriteAppCMD;
    property Architecture: String read ReadArchs write WriteArchs;
    property PkName: String read ReadPkgName write WritePkgName;
    property IPKName: String read ReadIPKName write WriteIPKName;
    property DSupport: String read ReadDSupport write WriteDSupport;
    property WizImage: String read ReadWizImage write WriteWizImage;
    property Binary: String read ReadBinary write WriteBinary;
    property USource: String read ReadUSource write WriteUSource;
    property Desktopfiles: String read ReadDesktopFiles write WriteDesktopFiles;
    property InTerminal: Boolean read ReadInTerminal write WriteInTerminal;
    procedure ReadDependencies(dname: String; info: TStringList);
    procedure WriteDependencies(dname: String; path: String);
    procedure WriteDependencies(dname: String; info: TStringList);
    function LoadFromFile(s: String): Boolean; virtual; abstract;
  end;

  TIPKControl = class;

  //** Class to handle IPK scripts
  TIPKScript = class(TIPKBasic)
  private
    fname: String;
  public
    constructor Create;
    destructor Destroy; override;

    function SaveToFile(s: String): Boolean;
    function LoadFromFile(s: String): Boolean; override;
    function LoadFromList(lst: TStrings): Boolean;
    procedure GetFiles(id: Integer; lst: TStrings);
    procedure GetDirectFileList(id: Integer; lst: TStrings);
    function FinalizeToControl: TIPKControl;
  end;

  //** Class to read IPK control files
  TIPKControl = class(TIPKBasic)
  private
    fname: String;
  public
    constructor Create;
    constructor Create(path: String);
    destructor Destroy; override;

    function SaveToFile(s: String): Boolean;

    property RawText: TStringList read Text write Text;
  end;

implementation

{ TIPKBasic }

constructor TIPKBasic.Create;
begin
  inherited;
  Text := TStringList.Create;
  FBasePath := ExtractFilePath(ParamStr(0));
  clang := '';
end;

destructor TIPKBasic.Destroy;
begin
  Text.Free;
  inherited;
end;

procedure TIPKBasic.WriteEntry(k, s: String);
begin
  s := k + ': ' + s;
  if SearchKeyIndex(k) > -1 then
    Text[SearchKeyIndex(k)] := s
  else
    Text.Add(s);
end;

function TIPKBasic.GetValue(s: String): String;
begin
  if pos(':', s) = length(s) then
  begin
    //There is an empty block (without value)
    Result := '';
    exit;
  end;
  Result := copy(s, pos(':', s) + 1, length(s));
  if (Result[1] = ' ') then
    Result := copy(Result, 2, length(Result));
end;

function TIPKBasic.SearchKeyIndex(S: String; localized: Boolean = true): Integer;
var
  i: Integer;
  h: String;
begin
  Result := -1;
  i := Text.Count;
  //First search for localized entry
  if (clang <> '') and (localized) then
  begin
    for i := 0 to Text.Count - 1 do
    begin
      h := copy(Text[i], 0, pos(':', Text[i]) - 1);
      if LowerCase(h) = LowerCase(s) + '[' + clang + ']' then
      begin
        Result := i;
        break;
      end;
    end;
  end;
  if (not localized) or (LowerCase(h) <> LowerCase(s) + '[' + clang + ']') then
    //Then search the general key
    for i := 0 to Text.Count - 1 do
    begin
      h := copy(Text[i], 0, pos(':', Text[i]) - 1);
      if LowerCase(h) = LowerCase(s) then
      begin
        Result := i;
        break;
      end;
    end;
end;

function TIPKBasic.SolveInclude(s: String): String;
var
  h: String;
begin
  h := copy(s, pos('"', s) + 1, length(s));
  h := copy(h, 0, pos('"', h) - 1);
  if h[1] = '.' then
    Result := FBasePath + '/' + h
  else
    Result := h;
end;

procedure TIPKBasic.WriteField(Name: String; info: TStrings);
var
  i: Integer;
begin
  if info.Count >= 0 then
  begin
    i := SearchKeyIndex(Name);
    if i > 0 then
    begin

      Text.Delete(i);
      while (i < Text.Count) and (Text[i] <> '') and (Text[i][1] = ' ') do
        Text.Delete(i);
    end;

    Text.Add(Name + ': ' + info[0]);
    for i := 1 to info.Count - 1 do
      Text.Add(' ' + info[i]);
  end;
end;

procedure TIPKBasic.ReadField(Name: String; info: TStrings);
var
  i: Integer;
  s: String;
begin
  i := SearchKeyIndex(Name);
  s := '';
  if i > -1 then
    s := Text[i];
  info.Clear;
  if s = '' then
    exit;
  if pos('include:"', s) > 0 then
    info.LoadFromFile(SolveInclude(s))
  else
  begin
    info.Add(GetValue(Text[i]));
    Inc(i);
    if i < Text.Count then
      repeat
        s := Text[i];
        if s[1] = ' ' then
        begin
          s := copy(s, 2, length(s));
          info.Add(s);
        end;
        Inc(i);
      until (i >= Text.Count) or (Text[i][1] <> ' ') or (length(Text[i]) < 1);
  end;
end;

procedure TIPKBasic.WriteType(atype: TPkgType);
var
  h: String;
begin
  case AType of
    ptLinstall: h := 'Type: linstall';
    ptDLink: h := 'Type: dlink';
    ptContainer: h := 'Type: container';
  end;
  if SearchKeyIndex('Type', false) > -1 then
    Text[SearchKeyIndex('Type', false)] := h
  else
    Text.Add(h);
end;

function TIPKBasic.ReadType: TPkgType;
var
  s: String;
  j: Integer;
begin
  Result := ptUnknown;
  j := SearchKeyIndex('Type', false);

  if j > -1 then
  begin
    s := Text[j];
    writeLn(s);
    if GetValue(s) = 'linstall' then
      Result := ptLinstall;
    if GetValue(s) = 'dlink' then
      Result := ptDLink;
    if GetValue(s) = 'container' then
      Result := ptContainer;
  end;
end;

procedure TIPKBasic.WriteName(s: String);
var
  k: String;
begin
  if clang = '' then
    k := 'Name'
  else
    k := 'Name[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadName: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Name');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteVersion(s: String);
var
  k: String;
begin
  if clang = '' then
    k := 'Version'
  else
    k := 'Version[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadVersion: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Version');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.ReadAppLicense(info: TStringList);
begin
  ReadField('License', info);
end;

procedure TIPKBasic.WriteAppLicense(path: String);
var
  s: String;
  i: Integer;
begin
  s := 'License: include:"' + path + '"';

  i := SearchKeyIndex('License');
  if i > 0 then
  begin
    Text.Delete(i);
    while (i < Text.Count) and (Text[i][1] = ' ') do
      Text.Delete(i);
  end;

  if i > -1 then
    Text[i] := s
  else
    Text.Add(s);
end;

procedure TIPKBasic.WriteAppLicense(info: TStringList);
begin
  WriteField('License', info);
end;

procedure TIPKBasic.ReadAppDescription(info: TStringList);
begin
  ReadField('Description', info);
end;

procedure TIPKBasic.WriteAppDescription(path: String);
var
  s: String;
  i: Integer;
begin
  s := 'Description: include:"' + path + '"';

  i := SearchKeyIndex('Description');
  if i > 0 then
  begin
    Text.Delete(i);
    while (i < Text.Count) and (Text[i][1] = ' ') do
      Text.Delete(i);
  end;

  if i > -1 then
    Text[i] := s
  else
    Text.Add(s);
end;

procedure TIPKBasic.WriteAppDescription(info: TStringList);
begin
  WriteField('Description', info);
end;

procedure TIPKBasic.WriteIcon(s: String);
begin
  WriteEntry('Icon', s);
end;

function TIPKBasic.ReadIcon: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Icon');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteSDesc(s: String);
var
  k: String;
begin
  if clang = '' then
    k := 'SDesc'
  else
    k := 'SDesc[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadSDesc: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('SDesc');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteGroup(g: TGroupType);
var
  s: String;
begin
  case g of
    gtALL: s := 'All';
    gtEDUCATION: s := 'Education';
    gtOFFICE: s := 'Office';
    gtDEVELOPMENT: s := 'Development';
    gtGRAPHIC: s := 'Graphic';
    gtNETWORK: s := 'Network';
    gtGAMES: s := 'Games';
    gtSYSTEM: s := 'System';
    gtMULTIMEDIA: s := 'Multimedia';
    gtADDITIONAL: s := 'Additional';
    gtOTHER: s := 'Other';
  end;
  s := 'Group: ' + s;

  if SearchKeyIndex('Group') > -1 then
    Text[SearchKeyIndex('Group')] := s
  else
    Text.Add(s);
end;

function TIPKBasic.ReadGroup: TGroupType;
var
  j: Integer;
  s: String;
begin
  Result := gtUNKNOWN;
  j := SearchKeyIndex('SDesc');
  if j > -1 then
    s := GetValue(Text[j]);

  s := LowerCase(s);
  if s = 'all' then
    Result := gtALL;
  if s = 'education' then
    Result := gtEDUCATION;
  if s = 'office' then
    Result := gtOFFICE;
  if s = 'development' then
    Result := gtDEVELOPMENT;
  if s = 'graphic' then
    Result := gtGRAPHIC;
  if s = 'network' then
    Result := gtNETWORK;
  if s = 'games' then
    Result := gtGAMES;
  if s = 'system' then
    Result := gtSYSTEM;
  if s = 'multimedia' then
    Result := gtMULTIMEDIA;
  if s = 'additional' then
    Result := gtADDITIONAL;
  if s = 'other' then
    Result := gtOTHER;
end;

procedure TIPKBasic.ReadBuildCMDs(lst: TStrings);
begin
  ReadField('Build', lst);
end;

procedure TIPKBasic.WriteBuildCMDs(lst: TStrings);
begin
  WriteField('Build', lst);
end;

procedure TIPKBasic.WriteAuthor(s: String);
var
  k: String;
begin
  if clang = '' then
    k := 'Author'
  else
    k := 'Author[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadAuthor: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Author');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteMaintainer(s: String);
var
  k: String;
begin
  if clang = '' then
    k := 'Maintainer'
  else
    k := 'Maintainer[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadMaintainer: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Maintainer');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteDisallows(s: String);
var
  k: String;
begin
  k := 'Disallow';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadDisallows: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Disallow');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteProfiles(lst: TStrings);
var
  k, s: String;
  i: Integer;
begin
  k := 'Profile[';
  for i := 0 to lst.Count - 1 do
  begin
    s := k + IntToStr(i) + ']: ' + lst[i];
    if SearchKeyIndex(k) > -1 then
      Text[SearchKeyIndex(k)] := s
    else
      Text.Add(s);
  end;
end;

procedure TIPKBasic.ReadProfiles(lst: TStrings);
var
  j: Integer;

  function GetProfileName(id: Integer): String;
  var
    i: Integer;
  begin
    Result := '';
    i := SearchKeyIndex('Profiles[' + IntToStr(id) + ']');
    if (id = 0) and (i < 0) then
      i := SearchKeyIndex('Profiles');
    if i > -1 then
      Result := GetValue(Text[i]);
  end;

begin
  j := 0;
  repeat
    lst.Add(GetProfileName(j));
    Inc(j);
  until GetProfileName(j) = '';
end;

procedure TIPKBasic.WriteAppCMD(s: String);
var
  k: String;
begin
  k := 'AppCMD';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadAppCMD: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('AppCMD');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteArchs(s: String);
var
  k: String;
begin
  k := 'Architecture';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadArchs: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Architecture', false);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WritePkgName(s: String);
var
  k: String;
begin
  k := 'PkName';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadPkgName: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('PkName');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteIPKName(s: String);
var
  k: String;
begin
  k := 'IPKName';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadIPKName: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('IPKName');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteDSupport(s: String);
var
  k: String;
begin
  k := 'DSupport';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadDSupport: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('DSupport');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.ReadDependencies(dname: String; info: TStringList);
var
  i: Integer;
  s: String;
begin
  if (dname = 'all') or (dname = '') then
    i := SearchKeyIndex('Dependencies', false)
  else
    i := SearchKeyIndex('Dependencies[' + dname + ']', false);

  s := '';
  if i > -1 then
    s := Text[i];
  info.Clear;

  if s = '' then
    exit;
  if pos('include:"', s) > 0 then
    info.LoadFromFile(SolveInclude(s))
  else
  begin
    info.Add(GetValue(Text[i]));
    Inc(i);
    repeat
      s := Text[i];
      if length(s) > 0 then
        if s[1] = ' ' then
        begin
          s := copy(s, 2, length(s));
          info.Add(s);
        end;
      Inc(i);
    until (length(Text[i]) = 0) or (i >= Text.Count) or (Text[i][1] <> ' ');
  end;
end;

procedure TIPKBasic.WriteDependencies(dname: String; path: String);
var
  s: String;
  i: Integer;
begin
  if (dname = 'all') or (dname = '') then
    s := 'Dependencies: include:"' + path + '"'
  else
    s := 'Dependencies[' + dname + ']: include:"' + path + '"';

  i := SearchKeyIndex('Dependencies');
  if i > 0 then
  begin
    Text.Delete(i);
    while (i < Text.Count) and (Text[i][1] = ' ') do
      Text.Delete(i);
  end;

  if i > -1 then
    Text[i] := s
  else
    Text.Add(s);
end;

procedure TIPKBasic.WriteDependencies(dname: String; info: TStringList);
var
  i: Integer;
  s: String;
begin
  if info.Count >= 0 then
  begin
    if (dname = 'all') or (dname = '') then
    begin
      s := 'Dependencies';
      i := SearchKeyIndex(s, false);
    end
    else
    begin
      s := 'Dependencies[' + dname + ']';
      i := SearchKeyIndex(s, false);
    end;

    if i > 0 then
    begin

      Text.Delete(i);
      while (i < Text.Count) and (Text[i] <> '') and (Text[i][1] = ' ') do
        Text.Delete(i);
    end;

    Text.Add(s + ': ' + info[0]);
    for i := 1 to info.Count - 1 do
      Text.Add(' ' + info[i]);

  end;
end;

procedure TIPKBasic.WriteWizImage(s: String);
var
  k: String;
begin
  k := 'WizImage';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadWizImage: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('WizImage');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteBinary(s: String);
begin
  WriteEntry('Binary', s);
end;

function TIPKBasic.ReadBinary: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Binary');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteUSource(s: String);
begin
  WriteEntry('USource', s);
end;

function TIPKBasic.ReadUSource: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('USource');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteDesktopFiles(s: String);
begin
  WriteEntry('Desktopfiles', s);
end;

function TIPKBasic.ReadDesktopFiles: String;
var
  j: Integer;
begin
  Result := '';
  j := SearchKeyIndex('Desktopfiles');
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteInTerminal(b: Boolean);
begin
  if b = true then
    WriteEntry('Desktopfiles', 'true')
  else
    WriteEntry('Desktopfiles', 'false');
end;

function TIPKBasic.ReadInTerminal: Boolean;
var
  j: Integer;
  s: String;
begin
  j := SearchKeyIndex('InTerminal');
  if j > -1 then
    s := GetValue(Text[j]);
  if LowerCase(s) = 'true' then
    Result := true
  else
    Result := false;
end;

{ TIPKScript }

constructor TIPKScript.Create;
begin
  inherited;
  Text.Add('IPK-Standard-Version: 1.0');
  Text.Add('');
  fname := '';
end;

destructor TIPKScript.Destroy;
begin
  inherited;
end;

function TIPKScript.SaveToFile(s: String): Boolean;
begin
  Result := true;
  try
    Text.SaveTofile(s);
    FBasePath := ExtractFilePath(s);
    fname := s;
  except
    Result := false;
  end;
end;

function TIPKScript.LoadFromFile(s: String): Boolean;
begin
  Result := true;
  if FileExists(s) then
  begin
    Text.LoadFromFile(s);
    if Text[0] <> 'IPK-Standard-Version: 1.0' then
    begin
      Result := false;
      Text.Clear;
      Text.Add('IPK-Standard-Version: 1.0');
      Text.Add('');
      exit;
    end;
    FBasePath := ExtractFilePath(s);
    fname := s;
  end
  else
    Result := false;
end;

function TIPKScript.LoadFromList(lst: TStrings): Boolean;
begin
  Result := true;
  writeLn(lst[0]);
  if lst[0] <> 'IPK-Standard-Version: 1.0' then
  begin
    Result := false;
    exit;
  end
  else
    Text.Assign(lst);
end;

function TIPKScript.FinalizeToControl: TIPKControl;
var
  i: Integer;
  cont: TIPKControl;

  procedure ProcessLine(ln: String);
  begin
    if length(ln) > 0 then
    begin
      if ln[1] = '#' then
        exit;
    end;

    if pos('#', ln) > 0 then
      ln := copy(ln, pos('#', ln) + 1, length(ln));

    if pos('IPKName:', ln) > 0 then
      exit;

    cont.RawText.Add(ln);
  end;

begin
  cont := TIPKControl.Create;

  for i := 0 to Text.Count - 1 do
    if pos('!-Files', Text[i]) <= 0 then
      ProcessLine(Text[i]);

  Result := cont;
end;

procedure TIPKScript.GetDirectFileList(id: Integer; lst: TStrings);
var
  i, j: Integer;
  s: String;
  fsec: TStringList;
begin
  fsec := TStringList.Create;
  for j := 0 to Text.Count - 1 do
    if pos('!-Files ~' + IntToStr(id), Text[j]) > 0 then
      break;

  for i := j + 1 to Text.Count - 1 do
    if pos('!-Files ~', Text[i]) > 0 then
      break
    else
      fsec.Add(Text[i]);

  i := 0;
  while i < fsec.Count - 1 do
  begin

    if fsec[i][1] = '>' then
      s := copy(fsec[i], 2, length(fsec[i]))
    else
    begin
      if (fsec[i][1] = '/') or (fsec[i][1] = '.') then
      begin
        lst.Add(s);
        if fsec[i][1] = '.' then
          lst.Add(FBasePath + fsec[i])
        else
          lst.Add(fsec[i]);
      end;
    end;
    Inc(i);
  end;
end;

procedure TIPKScript.GetFiles(id: Integer; lst: TStrings);
var
  i, j: Integer;
begin
  //Search for container-IPK files section
  j := SearchKeyIndex('Files');
  if j > -1 then
  begin
    ReadField('Files', lst);
  end
  else
  begin
    //Read normal files section
    for j := 0 to Text.Count - 1 do
      if pos('!-Files ~' + IntToStr(id), Text[j]) > 0 then
        break;

    for i := j + 1 to Text.Count - 1 do
      if pos('!-Files ~', Text[i]) > 0 then
        break
      else
        lst.Add(Text[i]);
  end;
end;

{ TIPKControl }

constructor TIPKControl.Create(path: String);
begin
  inherited Create;
  if FileExists(path) then
  begin
    Text.LoadFromFile(path);
    FBasePath := ExtractFilePath(path);
  end;
  fname := path;
end;

constructor TIPKControl.Create;
begin
  inherited Create;
  fname:='';
end;

destructor TIPKControl.Destroy;
begin
  inherited;
end;

function TIPKControl.SaveToFile(s: String): Boolean;
begin
  Result := true;
  try
    Text.SaveTofile(s);
    FBasePath := ExtractFilePath(s);
    fname := s;
  except
    Result := false;
  end;
end;

end.

