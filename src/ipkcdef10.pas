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
//** Contains classes to process IPK control and script files (version 1.0)
unit ipkcdef10;

{$mode objfpc}{$H+}

interface

uses
  Classes, GetText, LiTypes, LiUtils, SysUtils, LiFileUtil;

type

  //** Basic IPK reader class
  TIPKBasic = class
  private
    function GetValue(s: string): string;
    function SearchKeyIndex(s: string; localized: boolean = True): integer;
    function SolveInclude(s: string): string;
    function translate(s: string): string;
    procedure WriteEntry(k, s: string);

    procedure WriteType(atype: LiPkgType);
    function ReadType: LiPkgType;
    procedure WriteName(s: string);
    function ReadName: string;
    procedure WriteVersion(s: string);
    function ReadVersion: string;
    procedure WriteIcon(s: string);
    function ReadIcon: string;
    procedure WriteSDesc(s: string);
    function ReadSDesc: string;
    procedure WriteCategories(c: string);
    function ReadCategories: string;
    procedure WriteAuthor(s: string);
    function ReadAuthor: string;
    procedure WriteMaintainer(s: string);
    function ReadMaintainer: string;
    procedure WriteDisallows(s: string);
    function ReadDisallows: string;
    procedure WriteAppCMD(s: string);
    function ReadAppCMD: string;
    procedure WriteArchs(s: string);
    function ReadArchs: string;
    procedure WritePkgName(s: string);
    function ReadPkgName: string;
    procedure WriteIPKName(s: string);
    function ReadIPKName: string;
    procedure WriteDSupport(s: string);
    function ReadDSupport: string;
    procedure WriteWizImage(s: string);
    function ReadWizImage: string;
    procedure WriteBinary(s: string);
    function ReadBinary: string;
    procedure WriteUSource(s: string);
    function ReadUSource: string;
    procedure WriteDesktopFiles(s: string);
    function ReadDesktopFiles: string;
    procedure WriteInTerminal(b: boolean);
    function ReadInTerminal: boolean;
  protected
    Text: TStringList;
    FBasePath: string;
    clang: string;
    motrans: boolean;
    mofile: string;
    procedure WriteField(Name: string; info: TStrings);
    procedure ReadField(Name: string; info: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    property BasePath: string read FBasePath write FBasePath;
    property SType: LiPkgType read ReadType write WriteType;
    property AppName: string read ReadName write WriteName;
    property AppVersion: string read ReadVersion write WriteVersion;
    procedure ReadAppLicense(info: TStringList);
    procedure WriteAppLicense(path: string);
    procedure WriteAppLicense(info: TStringList);
    procedure ReadAppDescription(info: TStringList);
    procedure WriteAppDescription(path: string);
    procedure WriteAppDescription(info: TStringList);
    property Icon: string read ReadIcon write WriteIcon;
    property LangCode: string read clang write clang;
    property SDesc: string read ReadSDesc write WriteSDesc;
    property Categories: string read ReadCategories write WriteCategories;
    property Author: string read ReadAuthor write WriteAuthor;
    property Maintainer: string read ReadMaintainer write WriteMaintainer;
    property Disallows: string read ReadDisallows write WriteDisallows;
    procedure ReadProfiles(lst: TStrings);
    procedure WriteProfiles(lst: TStrings);
    procedure ReadBuildCMDs(lst: TStrings);
    procedure WriteBuildCMDs(lst: TStrings);
    property AppCMD: string read ReadAppCMD write WriteAppCMD;
    property Architecture: string read ReadArchs write WriteArchs;
    property PkName: string read ReadPkgName write WritePkgName;
    property IPKName: string read ReadIPKName write WriteIPKName;
    property DSupport: string read ReadDSupport write WriteDSupport;
    property WizImage: string read ReadWizImage write WriteWizImage;
    property Binary: string read ReadBinary write WriteBinary;
    property USource: string read ReadUSource write WriteUSource;
    property Desktopfiles: string read ReadDesktopFiles write WriteDesktopFiles;
    property InTerminal: boolean read ReadInTerminal write WriteInTerminal;
    procedure ReadDependencies(dname: string; info: TStringList);
    procedure WriteDependencies(dname: string; path: string);
    procedure WriteDependencies(dname: string; info: TStringList);
    function LoadFromFile(s: string): boolean; virtual; abstract;
    property UseMoTranslation: boolean read motrans write motrans;
    procedure GetMoFileList(list: TStringList);
    procedure SetMoFilesToDir(dir: string);
  end;

  TIPKControl = class;

  //** Class to handle IPK scripts
  TIPKScript = class(TIPKBasic)
  private
    fname: string;
  public
    constructor Create;
    destructor Destroy; override;

    function SaveToFile(s: string): boolean;
    function LoadFromFile(s: string): boolean; override;
    function LoadFromList(lst: TStrings): boolean;
    procedure GetFiles(id: integer; lst: TStrings);
    procedure GetDirectFileList(id: integer; lst: TStrings);
    function FinalizeToControl: TIPKControl;
  end;

  //** Class to read IPK control files
  TIPKControl = class(TIPKBasic)
  private
    fname: string;
  public
    constructor Create;
    constructor Create(path: string);
    destructor Destroy; override;

    function SaveToFile(s: string): boolean;
    procedure GetInternalFilesSection(lst: TStrings);
    function LoadFromFile(s: string): boolean; override;

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
  mofile := '';
  motrans := False;
end;

destructor TIPKBasic.Destroy;
begin
  Text.Free;
  inherited;
end;

procedure TIPKBasic.WriteEntry(k, s: string);
begin
  s := k + ': ' + s;
  if SearchKeyIndex(k) > -1 then
    Text[SearchKeyIndex(k)] := s
  else
    Text.Add(s);
end;

function TIPKBasic.GetValue(s: string): string;
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

function TIPKBasic.SearchKeyIndex(S: string; localized: boolean = True): integer;
var
  i: integer;
  h: string;
begin
  Result := -1;
  i := Text.Count;
  //First search for localized entry
  if (clang <> '') and (localized) then
  begin
    for i := 0 to Text.Count - 1 do
    begin
      if (length(Text[i]) > 0) and (Text[i][1] <> '#') and (Text[i][1] <> ' ') then
      begin
        h := copy(Text[i], 0, pos(':', Text[i]) - 1);
        if LowerCase(h) = LowerCase(s) + '[' + clang + ']' then
        begin
          Result := i;
          break;
        end;
      end;
    end;
  end;
  //Then search the general key
  if (not localized) or (Result < 0) then
    for i := 0 to Text.Count - 1 do
    begin
      if (length(Text[i]) > 0) and (Text[i][1] <> '#') and (Text[i][1] <> ' ') then
      begin
        h := copy(Text[i], 0, pos(':', Text[i]) - 1);
        if LowerCase(h) = LowerCase(s) then
        begin
          Result := i;
          break;
        end;
      end;
    end;
end;

function TIPKBasic.SolveInclude(s: string): string;
var
  h: string;
begin
  h := copy(s, pos('"', s) + 1, length(s));
  h := copy(h, 0, pos('"', h) - 1);
  if not FilenameIsAbsolute(h) then
    Result := AppendPathDelim(FBasePath) + h
  else
    Result := h;
end;

function TIPKBasic.Translate(s: string): string;
var
  i: integer;
  mo: TMoFile;
begin
  Result := s;
  if mofile = '~' then
    exit;
  if mofile = '' then
  begin
    mofile := '~';
    for i := 0 to Text.Count - 1 do
      if pos('include:', Text[i]) > 0 then
        if LowerCase(ExtractFileExt(SolveInclude(Text[i]))) = '.mo' then
        begin
          mofile := ExtractFileName(SolveInclude(Text[i]));
          if (mofile = GetLangId + '.mo') or
            (copy(mofile, pos('-', mofile) + 1, length(mofile)) = GetlangId + '.mo') then
            break
          else
            mofile := '~';
        end;
  end;
  if (mofile = '~') or (trim(mofile) = '') or (not FileExists(FBasePath + mofile)) then
    exit;
  mo := TMoFile.Create(FBasePath + mofile);
  Result := mo.Translate(s);
  mo.Free;
end;

procedure TIPKBasic.GetMoFileList(list: TStringList);
var
  i: integer;
begin
  for i := 0 to Text.Count - 1 do
    if pos('include:', Text[i]) > 0 then
    begin
      if (ExtractFileExt(SolveInclude(Text[i]))) = '.mo' then
        list.Add(SolveInclude(Text[i]));
    end;
end;

procedure TIPKBasic.SetMoFilesToDir(dir: string);
var
  list: TStringList;
  i: integer;
begin
  list := TStringList.Create;
  GetMoFileList(list);
  i := 0;
  while i < Text.Count do
  begin
    if (pos('include:', Text[i]) > 0) and (pos('.mo', Text[i]) > 0) then
    begin
      Text.Delete(i);
    end
    else
      Inc(i);
  end;
  Text.Insert(1, '');
  for i := 0 to list.Count - 1 do
    Text.Insert(1, 'include:"' + dir + '/' + ExtractFileName(list[i]) + '"');
  list.Free;
end;

procedure TIPKBasic.WriteField(Name: string; info: TStrings);
var
  i: integer;
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

procedure TIPKBasic.ReadField(Name: string; info: TStrings);
var
  i: integer;
  s: string;
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

procedure TIPKBasic.WriteType(atype: LiPkgType);
var
  h: string;
begin
  case AType of
    ptLinstall: h := 'Type: linstall';
    ptDLink: h := 'Type: dlink';
    ptContainer: h := 'Type: container';
  end;
  if SearchKeyIndex('Type', False) > -1 then
    Text[SearchKeyIndex('Type', False)] := h
  else
    Text.Add(h);
end;

function TIPKBasic.ReadType: LiPkgType;
var
  s: string;
  j: integer;
begin
  Result := ptUnknown;
  j := SearchKeyIndex('Type', False);

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

procedure TIPKBasic.WriteName(s: string);
var
  k: string;
begin
  if clang = '' then
    k := 'Name'
  else
    k := 'Name[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadName: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Name');
  if j > -1 then
    Result := GetValue(Text[j]);
  Result := translate(Result);
end;

procedure TIPKBasic.WriteVersion(s: string);
var
  k: string;
begin
  if clang = '' then
    k := 'Version'
  else
    k := 'Version[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadVersion: string;
var
  j: integer;
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

procedure TIPKBasic.WriteAppLicense(path: string);
var
  s: string;
  i: integer;
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

procedure TIPKBasic.WriteAppDescription(path: string);
var
  s: string;
  i: integer;
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

procedure TIPKBasic.WriteIcon(s: string);
begin
  WriteEntry('Icon', s);
end;

function TIPKBasic.ReadIcon: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Icon', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteSDesc(s: string);
var
  k: string;
begin
  if clang = '' then
    k := 'SDesc'
  else
    k := 'SDesc[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadSDesc: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('SDesc');
  if j > -1 then
    Result := GetValue(Text[j]);

  Result := translate(Result);
end;

procedure TIPKBasic.WriteCategories(c: string);
var
  s: string;
  //@DEPRECATED
{begin
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

  if SearchKeyIndex('Group', false) > -1 then
    Text[SearchKeyIndex('Group', false)] := s
  else
    Text.Add(s);
end;}
begin
  s := 'Categories: ' + c;

  if SearchKeyIndex('', False) > -1 then
    Text[SearchKeyIndex('Categories', False)] := s
  else
    Text.Add(s);
end;

function TIPKBasic.ReadCategories: string;
  //@DEPRECATED
{var
  j: Integer;
  s: String;
begin
  Result := gtUNKNOWN;
  j := SearchKeyIndex('Group', false);
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
end;}
var
  j: integer;
begin
  Result := 'unknown;';
  j := SearchKeyIndex('Categories', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.ReadBuildCMDs(lst: TStrings);
begin
  ReadField('Build', lst);
end;

procedure TIPKBasic.WriteBuildCMDs(lst: TStrings);
begin
  WriteField('Build', lst);
end;

procedure TIPKBasic.WriteAuthor(s: string);
var
  k: string;
begin
  if clang = '' then
    k := 'Author'
  else
    k := 'Author[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadAuthor: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Author');
  if j > -1 then
    Result := GetValue(Text[j]);
  Result := translate(Result);
end;

procedure TIPKBasic.WriteMaintainer(s: string);
var
  k: string;
begin
  if clang = '' then
    k := 'Maintainer'
  else
    k := 'Maintainer[' + clang + ']';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadMaintainer: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Maintainer');
  if j > -1 then
    Result := GetValue(Text[j]);
  Result := translate(Result);
end;

procedure TIPKBasic.WriteDisallows(s: string);
var
  k: string;
begin
  k := 'Disallow';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadDisallows: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Disallow', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteProfiles(lst: TStrings);
var
  k, s: string;
  i: integer;
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
  j: integer;

  function GetProfileName(id: integer): string;
  var
    i: integer;
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

procedure TIPKBasic.WriteAppCMD(s: string);
var
  k: string;
begin
  k := 'AppCMD';

  WriteEntry(k, s);
end;

function TIPKBasic.ReadAppCMD: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('AppCMD', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteArchs(s: string);
var
  k: string;
begin
  k := 'Architecture';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadArchs: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Architecture', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WritePkgName(s: string);
var
  k: string;
begin
  k := 'PkName';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadPkgName: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('PkName', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteIPKName(s: string);
var
  k: string;
begin
  k := 'IPKName';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadIPKName: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('IPKName', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteDSupport(s: string);
var
  k: string;
begin
  k := 'DSupport';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadDSupport: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('DSupport', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.ReadDependencies(dname: string; info: TStringList);
var
  i: integer;
  s: string;
begin
  if (dname = 'all') or (dname = '') then
    i := SearchKeyIndex('Dependencies', False)
  else
    i := SearchKeyIndex('Dependencies[' + dname + ']', False);

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

procedure TIPKBasic.WriteDependencies(dname: string; path: string);
var
  s: string;
  i: integer;
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

procedure TIPKBasic.WriteDependencies(dname: string; info: TStringList);
var
  i: integer;
  s: string;
begin
  if info.Count >= 0 then
  begin
    if (dname = 'all') or (dname = '') then
    begin
      s := 'Dependencies';
      i := SearchKeyIndex(s, False);
    end
    else
    begin
      s := 'Dependencies[' + dname + ']';
      i := SearchKeyIndex(s, False);
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

procedure TIPKBasic.WriteWizImage(s: string);
var
  k: string;
begin
  k := 'WizImage';
  WriteEntry(k, s);
end;

function TIPKBasic.ReadWizImage: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('WizImage', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteBinary(s: string);
begin
  WriteEntry('Binary', s);
end;

function TIPKBasic.ReadBinary: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Binary', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteUSource(s: string);
begin
  WriteEntry('USource', s);
end;

function TIPKBasic.ReadUSource: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('USource', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteDesktopFiles(s: string);
begin
  WriteEntry('Desktopfiles', s);
end;

function TIPKBasic.ReadDesktopFiles: string;
var
  j: integer;
begin
  Result := '';
  j := SearchKeyIndex('Desktopfiles', False);
  if j > -1 then
    Result := GetValue(Text[j]);
end;

procedure TIPKBasic.WriteInTerminal(b: boolean);
begin
  if b = True then
    WriteEntry('Desktopfiles', 'true')
  else
    WriteEntry('Desktopfiles', 'false');
end;

function TIPKBasic.ReadInTerminal: boolean;
var
  j: integer;
  s: string;
begin
  j := SearchKeyIndex('InTerminal', False);
  if j > -1 then
    s := GetValue(Text[j]);
  if LowerCase(s) = 'true' then
    Result := True
  else
    Result := False;
end;

{ TIPKScript }

constructor TIPKScript.Create;
begin
  inherited;
  Text.Add('IPK-Standard-Version: 1.1');
  Text.Add('');
  fname := '';
end;

destructor TIPKScript.Destroy;
begin
  inherited;
end;

function TIPKScript.SaveToFile(s: string): boolean;
begin
  Result := True;
  try
    Text.SaveTofile(s);
    FBasePath := ExtractFilePath(s);
    fname := s;
  except
    Result := False;
  end;
end;

function TIPKScript.LoadFromFile(s: string): boolean;
begin
  Result := True;
  if FileExists(s) then
  begin
    Text.LoadFromFile(s);
    if (Text[0] <> 'IPK-Standard-Version: 1.1') and
      (Text[0] <> 'IPK-Standard-Version: 1.0') then
    begin
      Result := False;
      Text.Clear;
      Text.Add('IPK-Standard-Version: 1.1');
      Text.Add('');
      exit;
    end;
    FBasePath := ExtractFilePath(s);
    fname := s;
  end
  else
    Result := False;
end;

function TIPKScript.LoadFromList(lst: TStrings): boolean;
begin
  Result := True;
  writeLn(lst[0]);
  if (lst[0] <> 'IPK-Standard-Version: 1.1') and
    (lst[0] <> 'IPK-Standard-Version: 1.0') then
  begin
    Result := False;
    exit;
  end
  else
    Text.Assign(lst);
end;

function TIPKScript.FinalizeToControl: TIPKControl;
var
  i: integer;
  cont: TIPKControl;

  procedure ProcessLine(ln: string);
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

procedure TIPKScript.GetDirectFileList(id: integer; lst: TStrings);
var
  i, j: integer;
  s: string;
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

procedure TIPKScript.GetFiles(id: integer; lst: TStrings);
var
  i, j: integer;
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

constructor TIPKControl.Create(path: string);
begin
  inherited Create;

  LoadFromFile(path);
  FBasePath := ExtractFilePath(path);

  fname := path;
end;

constructor TIPKControl.Create;
begin
  inherited Create;
  fname := '';
  FBasePath := '';
end;

destructor TIPKControl.Destroy;
begin
  inherited;
end;

function TIPKControl.SaveToFile(s: string): boolean;
begin
  Result := True;
  try
    Text.SaveTofile(s);
    FBasePath := ExtractFilePath(s);
    fname := s;
  except
    Result := False;
  end;
end;

function TIPKControl.LoadFromFile(s: string): boolean;
begin
  Result := True;
  if FileExists(s) then
  begin
    Text.LoadFromFile(s);
    if (Text[0] <> 'IPK-Standard-Version: 1.1') and
      (Text[0] <> 'IPK-Standard-Version: 1.0') then
    begin
      Result := False;
      exit;
    end;
    FBasePath := ExtractFilePath(s);
    fname := s;
  end
  else
    Result := False;

  UseMoTranslation := True;
end;

procedure TIPKControl.GetInternalFilesSection(lst: TStrings);
var
  j: integer;
begin
  //Search for container-IPK files section
  j := SearchKeyIndex('Files', False);
  if j > -1 then
  begin
    ReadField('Files', lst);
  end;
end;

end.

