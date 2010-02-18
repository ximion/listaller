(*
===============================================================================================
Name    : LibTar
===============================================================================================
Subject : Handling of "tar" files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tübingen
          GERMANY

E-Mail:   stefan@destructor.de
Web:      www.destructor.de

===============================================================================================
TTarArchive Usage
-----------------
- Choose a constructor
- Make an instance of TTarArchive                  TA := TTarArchive.Create (Filename);
- Scan through the archive                         TA.Reset;
                                                   WHILE TA.FindNext (DirRec) DO begin
- Evaluate the DirRec for each file                  ListBox.Items.Add (DirRec.Name);
- Read out the current file                          TA.ReadFile (DestFilename);
  (You can ommit this if you want to
  read in the directory only)                        end;
- You're done                                      TA.Free;


TTarWriter Usage
----------------
- Choose a constructor
- Make an instance of TTarWriter                   TW := TTarWriter.Create ('my.tar');
- Add a file to the tar archive                    TW.AddFile ('foobar.txt');
- Add a String as a file                           TW.AddString (SL.Text, 'joe.txt', Now);
- Destroy TarWriter instance                       TW.Free;
- Now your tar file is ready.


Source, Legals ("Licence")
--------------------------
The official site to get this code is http://www.destructor.de/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

IN SHORT: Usage and distribution of this source code is free.
          You use it completely on your own risk.

Donateware
----------
if you like this code, you are free to donate
http://www.destructor.de/donateware.htm

===============================================================================================
!!!  All parts of this code which are not finished or known to be buggy
     are marked with three exclamation marks
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
2001-04-26  HeySt  0.0.1 Start
2001-04-28  HeySt  1.0.0 First Release
2001-06-19  HeySt  2.0.0 Finished TTarWriter
2001-09-06  HeySt  2.0.1 Bugfix in TTarArchive.FindNext: FBytesToGo must sometimes be 0
2001-10-25  HeySt  2.0.2 Introduced the ClearDirRec procedure
2001-11-13  HeySt  2.0.3 Bugfix: Take out ClearDirRec call from WriteTarHeader
                         Bug Reported by Tony BenBrahim
2001-12-25  HeySt  2.0.4 WriteTarHeader: Fill Rec with zero bytes before filling it
2002-05-18  HeySt  2.0.5 Kylix awareness: Thanks to Kerry L. Davison for the canges
2005-09-03  HeySt  2.0.6 TTarArchive.FindNext: Don't access SourceStream.Size
                         (for compressed streams, which don't know their .Size)
2006-03-13  HeySt  2.0.7 Bugfix in ReadFile (Buffer : POINTER)
2007-05-16  HeySt  2.0.8 Bugfix in TTarWriter.AddFile (Convertfilename in the else branch)
                         Bug Reported by Chris Rorden
2010-01-18  Ximion 2.0.8 Some small compatibility fixes for newer FPC and use in Listaller
*)
//** Unit to work with TAR archives
unit libtar;

{$mode delphi} //Switch to Delphi mode

interface

uses
{$IFDEF LINUX}
   Unix, BaseUnix,
{$ENDIF}
{$IFDEF MSWINDOWS}
   Windows,
{$ENDIF}
  SysUtils, Classes, liBasic;

type
  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                     tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                     tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = set of TTarPermission;

  // --- Type of File
  TFileType = (ftNormal,          // Regular file
               ftLink,            // Link to another, previously archived, file (LinkName)
               ftSymbolicLink,    // Symbolic link to another file              (LinkName)
               ftCharacter,       // Character special files
               ftBlock,           // Block special files
               ftDirectory,       // Directory entry. Size is zero (unlimited) or max. number of bytes
               ftFifo,            // FIFO special file. No data stored in the archive.
               ftContiguous,      // Contiguous file, if supported by OS
               ftDumpDir,         // List of files
               ftMultiVolume,     // Multi-volume file part
               ftVolumeHeader);   // Volume header. Can appear only as first record in the archive

  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = SET OF TTarMode;

  // --- record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = record
                  Name        : String;            // File path and name
                  Size        : INT64;             // File size in Bytes
                  DateTime    : TDateTime;         // Last modification date and time
                  Permissions : TTarPermissions;   // Access permissions
                  FileType    : TFileType;         // Type of file
                  LinkName    : String;            // Name of linked file (for ftLink, ftSymbolicLink)
                  UID         : INTEGER;           // User ID
                  GID         : INTEGER;           // Group ID
                  UserName    : String;            // User name
                  GroupName   : String;            // Group name
                  ChecksumOK  : BOOLEAN;           // Checksum was OK
                  Mode        : TTarModes;         // Mode
                  Magic       : String;            // Contents of the "Magic" field
                  MajorDevNo  : INTEGER;           // Major Device No. for ftCharacter and ftBlock
                  MinorDevNo  : INTEGER;           // Minor Device No. for ftCharacter and ftBlock
                  FilePos     : INT64;             // Position in TAR file
                end;

  // --- The TAR Archive class
  TTarArchive = class
                protected
                  FStream     : TStream;   // Internal Stream
                  FOwnsStream : BOOLEAN;   // True if FStream is owned by the TTarArchive instance
                  FBytesToGo  : INT64;     // Bytes until the next Header record
                public
                  constructor Create (Stream   : TStream);                                OVERLOAD;
                  constructor Create (Filename : String;
                                      FileMode : WORD = fmOpenRead OR fmShareDenyWrite);  OVERLOAD;
                  DESTRUCTOR Destroy;                                       OVERRIDE;
                  procedure Reset;                                         // Reset File Pointer
                  function  FindNext (var DirRec : TTarDirRec) : BOOLEAN;  // Reads next Directory Info record. FALSE if EOF reached
                  procedure ReadFile (Buffer   : POINTER); OVERLOAD;       // Reads file data for last Directory record
                  procedure ReadFile (Stream   : TStream); OVERLOAD;       // -;-
                  procedure ReadFile (Filename : String);  OVERLOAD;       // -;-
                  function  ReadFile : String;           OVERLOAD;         // -;-

                  procedure GetFilePos (var Current, Size : INT64);        // Current File Position
                  procedure SetFilePos (NewPos : INT64);                   // Set new Current File Position
                end;

  // --- The TAR Archive Writer class
  TTarWriter = class
               protected
                 FStream      : TStream;
                 FOwnsStream  : BOOLEAN;
                 FFinalized   : BOOLEAN;
                                                   // --- Used at the next "Add" method call: ---
                 FPermissions : TTarPermissions;   // Access permissions
                 FUID         : INTEGER;           // User ID
                 FGID         : INTEGER;           // Group ID
                 FUserName    : String;            // User name
                 FGroupName   : String;            // Group name
                 FMode        : TTarModes;         // Mode
                 FMagic       : String;            // Contents of the "Magic" field
                 constructor CreateEmpty;
               public
                 constructor Create (TargetStream   : TStream);                            OVERLOAD;
                 constructor Create (TargetFilename : String; Mode : INTEGER = fmCreate);  OVERLOAD;
                 destructor  Destroy; OVERRIDE;                   // Writes end-Of-File Tag
                 procedure AddFile   (Filename : String;  TarFilename : String = '');
                 procedure AddStream (Stream   : TStream; TarFilename : String; FileDateGmt : TDateTime);
                 procedure AddString (Contents : String;  TarFilename : String; FileDateGmt : TDateTime);
                 procedure AddDir          (Dirname            : String; DateGmt : TDateTime; MaxDirSize : INT64 = 0);
                 procedure AddSymbolicLink (Filename, Linkname : String; DateGmt : TDateTime);
                 procedure AddLink         (Filename, Linkname : String; DateGmt : TDateTime);
                 procedure AddVolumeHeader (VolumeId           : String; DateGmt : TDateTime);
                 procedure Finalize;
                 PROPERTY Permissions : TTarPermissions READ FPermissions WRITE FPermissions;   // Access permissions
                 PROPERTY UID         : INTEGER         READ FUID         WRITE FUID;           // User ID
                 PROPERTY GID         : INTEGER         READ FGID         WRITE FGID;           // Group ID
                 PROPERTY UserName    : String          READ FUserName    WRITE FUserName;      // User name
                 PROPERTY GroupName   : String          READ FGroupName   WRITE FGroupName;     // Group name
                 PROPERTY Mode        : TTarModes       READ FMode        WRITE FMode;          // Mode
                 PROPERTY Magic       : String          READ FMagic       WRITE FMagic;         // Contents of the "Magic" field
               end;

// --- Some useful constants
CONST
  FILETYPE_NAME : Array [TFileType] OF String =
                  ('Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
                   'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol', 'Volume Header');

  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                         tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                         tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


function  PermissionString      (Permissions : TTarPermissions) : String;
function  ConvertFilename       (Filename    : String)          : String;
function  FileTimeGMT           (FileName    : String)          : TDateTime;  OVERLOAD;
function  FileTimeGMT           (SearchRec   : TSearchRec)      : TDateTime;  OVERLOAD;
procedure ClearDirRec           (var DirRec  : TTarDirRec);


{
===============================================================================================
IMPLEMENTATION
===============================================================================================
}

IMPLEMENTATION

function PermissionString (Permissions : TTarPermissions) : String;
begin
  Result := '';
  if tpReadByOwner    IN Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOwner   IN Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOwner IN Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByGroup    IN Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByGroup   IN Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByGroup IN Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByOther    IN Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOther   IN Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOther IN Permissions then Result := Result + 'x' else Result := Result + '-';
end;


function ConvertFilename  (Filename : String) : String;
         // Converts the filename to Unix conventions
begin
  {$IFDEF LINUX}
  Result := Filename;
  {$else}
  Result := StringReplace (Filename, '\', '/', [rfReplaceAll]);
  {$endIF}
end;


function FileTimeGMT (FileName: String): TDateTime;
         // Returns the Date and Time of the last modification of the given File
         // The Result is zero if the file could not be found
         // The Result is given in UTC (GMT) time zone
var
  SR : TSearchRec;
begin
  Result := 0.0;
  if FindFirst (FileName, faAnyFile, SR) = 0 THEN
    Result := FileTimeGMT (SR);
  FindClose (SR);
end;


function FileTimeGMT (SearchRec : TSearchRec) : TDateTime;
{$IFDEF MSWINDOWS}
var
  SystemFileTime: TSystemTime;
{$endIF}
{$IFDEF LINUX}
var
  TimeVal  : TTimeVal;
  TimeZone : TTimeZone;
{$endIF}
begin
  Result := 0.0;
  {$IFDEF MSWINDOWS} {$WARNINGS OFF}
    if (SearchRec.FindData.dwFileAttributes AND faDirectory) = 0 THEN
      if FileTimeToSystemTime (SearchRec.FindData.ftLastWriteTime, SystemFileTime) THEN
        Result := EncodeDate (SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
                + EncodeTime (SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  {$endIF} {$WARNINGS ON}
  {$IFDEF LINUX}
     if SearchRec.Attr AND faDirectory = 0 then begin
       Result := FileDateToDateTime (SearchRec.Time);
       fpGetTimeOfDay(@TimeVal, @TimeZone);
       Result := Result + TimeZone.tz_minuteswest / (60 * 24);
       end;
  {$endIF}
end;


procedure ClearDirRec (var DirRec : TTarDirRec);
          // This is included because a FillChar (DirRec, SizeOf (DirRec), 0)
          // will destroy the long String pointers, leading to strange bugs
begin
  WITH DirRec DO begin
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType (0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := FALSE;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
    FilePos     := 0;
    end;
end;

{
===============================================================================================
TAR format
===============================================================================================
}

const
  recordSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

type
  TTarHeader = packed record
                 Name     : Array [0..NAMSIZ-1] OF CHAR;
                 Mode     : Array [0..7] OF CHAR;
                 UID      : Array [0..7] OF CHAR;
                 GID      : Array [0..7] OF CHAR;
                 Size     : Array [0..11] OF CHAR;
                 MTime    : Array [0..11] OF CHAR;
                 ChkSum   : Array [0..7] OF CHAR;
                 LinkFlag : CHAR;
                 LinkName : Array [0..NAMSIZ-1] OF CHAR;
                 Magic    : Array [0..7] OF CHAR;
                 UName    : Array [0..TUNMLEN-1] OF CHAR;
                 GName    : Array [0..TGNMLEN-1] OF CHAR;
                 DevMajor : Array [0..7] OF CHAR;
                 DevMinor : Array [0..7] OF CHAR;
               end;

function ExtractText (P : PChar) : String;
begin
  Result := String (P);
end;


function ExtractNumber (P : PChar) : INTEGER; OVERLOAD;
var
  Strg : String;
begin
  Strg := Trim(StrPas (P));
  P := PChar(Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO begin
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    end;
end;


function ExtractNumber64 (P : PChar) : INT64; OVERLOAD;
var
  Strg : String;
begin
  Strg := Trim (StrPas (P));
  P := PChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO begin
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    end;
end;



function ExtractNumber (P : PChar; MaxLen : INTEGER) : INTEGER; OVERLOAD;
var
  S0   : Array [0..255] OF CHAR;
  Strg : String;
begin
  StrLCopy (S0, P, MaxLen);
  Strg := Trim (StrPas (S0));
  P := PChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO begin
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    end;
end;


function ExtractNumber64 (P : PChar; MaxLen : INTEGER) : INT64; OVERLOAD;
var
  S0   : Array [0..255] OF CHAR;
  Strg : String;
begin
  StrLCopy (S0, P, MaxLen);
  Strg := Trim (StrPas (S0));
  P := PChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO begin
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    end;
end;


function records (Bytes : INT64) : INT64;
begin
  Result := Bytes DIV recordSIZE;
  if Bytes MOD recordSIZE > 0 THEN
    INC (Result);
end;


procedure Octal (N : INTEGER; P : PChar; Len : INTEGER);
         // Makes a String of octal digits
         // The String will always be "Len" characters long
var
  I     : INTEGER;
begin
  FOR I := Len-2 DOWNTO 0 DO begin
    (P+I)^ := CHR (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    end;
  FOR I := 0 TO Len-3 DO
    if (P+I)^ = '0'
      then (P+I)^ := #32
      else BREAK;
  (P+Len-1)^ := #32;
end;


procedure Octal64 (N : INT64; P : PChar; Len : INTEGER);
         // Makes a String of octal digits
         // The String will always be "Len" characters long
var
  I     : INTEGER;
begin
  FOR I := Len-2 DOWNTO 0 DO begin
    (P+I)^ := CHR (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    end;
  FOR I := 0 TO Len-3 DO
    if (P+I)^ = '0'
      then (P+I)^ := #32
      else BREAK;
  (P+Len-1)^ := #32;
end;


procedure OctalN (N : INTEGER; P : PChar; Len : INTEGER);
begin
  Octal (N, P, Len-1);
  (P+Len-1)^ := #0;
end;


procedure WriteTarHeader (Dest : TStream; DirRec : TTarDirRec);
var
  Rec      : Array [0..recordSIZE-1] OF CHAR;
  TH       : TTarHeader absolute Rec;
  Mode     : INTEGER;
  NullDate : TDateTime;
  Checksum : CARDINAL;
  I        : INTEGER;
begin
  FillChar (Rec, recordSIZE, 0);

  if Length(DirRec.Name)>NAMSIZ then writeLn('E: Filename is too long!');





  StrLCopy(TH.Name, PChar(DirRec.Name), NAMSIZ);
  Mode := 0;
  if tmSaveText IN DirRec.Mode then Mode := Mode OR $0200;
  if tmSetGid   IN DirRec.Mode then Mode := Mode OR $0400;
  if tmSetUid   IN DirRec.Mode then Mode := Mode OR $0800;
  if tpReadByOwner    IN DirRec.Permissions then Mode := Mode OR $0100;
  if tpWriteByOwner   IN DirRec.Permissions then Mode := Mode OR $0080;
  if tpExecuteByOwner IN DirRec.Permissions then Mode := Mode OR $0040;
  if tpReadByGroup    IN DirRec.Permissions then Mode := Mode OR $0020;
  if tpWriteByGroup   IN DirRec.Permissions then Mode := Mode OR $0010;
  if tpExecuteByGroup IN DirRec.Permissions then Mode := Mode OR $0008;
  if tpReadByOther    IN DirRec.Permissions then Mode := Mode OR $0004;
  if tpWriteByOther   IN DirRec.Permissions then Mode := Mode OR $0002;
  if tpExecuteByOther IN DirRec.Permissions then Mode := Mode OR $0001;
  OctalN (Mode, @TH.Mode, 8);
  OctalN (DirRec.UID, @TH.UID, 8);
  OctalN (DirRec.GID, @TH.GID, 8);
  Octal64 (DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate (1970, 1, 1);
  if DirRec.DateTime >= NullDate
    then Octal (Trunc ((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
    else Octal (Trunc (                   NullDate  * 86400.0), @TH.MTime, 12);
  CASE DirRec.FileType OF
    ftNormal       : TH.LinkFlag := '0';
    ftLink         : TH.LinkFlag := '1';
    ftSymbolicLink : TH.LinkFlag := '2';
    ftCharacter    : TH.LinkFlag := '3';
    ftBlock        : TH.LinkFlag := '4';
    ftDirectory    : TH.LinkFlag := '5';
    ftFifo         : TH.LinkFlag := '6';
    ftContiguous   : TH.LinkFlag := '7';
    ftDumpDir      : TH.LinkFlag := 'D';
    ftMultiVolume  : TH.LinkFlag := 'M';
    ftVolumeHeader : TH.LinkFlag := 'V';
    end;
  StrLCopy (TH.LinkName, PChar (DirRec.LinkName), NAMSIZ);
  StrLCopy (TH.Magic, PChar (DirRec.Magic + #32#32#32#32#32#32#32#32), 8);
  StrLCopy (TH.UName, PChar (DirRec.UserName), TUNMLEN);
  StrLCopy (TH.GName, PChar (DirRec.GroupName), TGNMLEN);
  OctalN (DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN (DirRec.MinorDevNo, @TH.DevMinor, 8);
  StrMove (TH.ChkSum, CHKBLANKS, 8);

  CheckSum := 0;
  FOR I := 0 TO SizeOf (TTarHeader)-1 DO
    INC (CheckSum, INTEGER (ORD (Rec [I])));
  OctalN (CheckSum, @TH.ChkSum, 8);

  Dest.Write (TH, recordSIZE);
end;



{
===============================================================================================
TTarArchive
===============================================================================================
}

constructor TTarArchive.Create (Stream : TStream);
begin
  INHERITED Create;
  FStream     := Stream;
  FOwnsStream := FALSE;
  Reset;
end;


constructor TTarArchive.Create (Filename : String; FileMode : WORD);
begin
  INHERITED Create;
  FStream     := TFileStream.Create (Filename, FileMode);
  FOwnsStream := TRUE;
  Reset;
end;


DESTRUCTOR TTarArchive.Destroy;
begin
  if FOwnsStream THEN
    FStream.Free;
  INHERITED Destroy;
end;


procedure TTarArchive.Reset;
          // Reset File Pointer
begin
  FStream.Position := 0;
  FBytesToGo       := 0;
end;


function  TTarArchive.FindNext (var DirRec : TTarDirRec) : BOOLEAN;
          // Reads next Directory Info record
          // The Stream pointer must point to the first byte of the tar header
var
  Rec          : Array [0..recordSIZE-1] OF CHAR;
  CurFilePos   : INTEGER;
  Header       : TTarHeader ABSOLUTE Rec;
  I            : INTEGER;
  HeaderChkSum : WORD;
  Checksum     : CARDINAL;
begin
  // --- Scan until next pointer
  if FBytesToGo > 0 THEN
    FStream.Seek (records (FBytesToGo) * recordSIZE, soFromCurrent);

  // --- EOF reached?
  Result := FALSE;
  CurFilePos := FStream.Position;
  TRY
    FStream.ReadBuffer (Rec, recordSIZE);
    if Rec [0] = #0 then EXIT;   // EOF reached
  EXCEPT
    EXIT;   // EOF reached, too
    end;
  Result := TRUE;

  ClearDirRec (DirRec);

  DirRec.FilePos := CurFilePos;
  DirRec.Name := ExtractText (Header.Name);
  DirRec.Size := ExtractNumber64 (@Header.Size, 12);
  DirRec.DateTime := EncodeDate (1970, 1, 1) + (ExtractNumber (@Header.MTime, 12) / 86400.0);
  I := ExtractNumber (@Header.Mode);
  if I AND $0100 <> 0 then Include (DirRec.Permissions, tpReadByOwner);
  if I AND $0080 <> 0 then Include (DirRec.Permissions, tpWriteByOwner);
  if I AND $0040 <> 0 then Include (DirRec.Permissions, tpExecuteByOwner);
  if I AND $0020 <> 0 then Include (DirRec.Permissions, tpReadByGroup);
  if I AND $0010 <> 0 then Include (DirRec.Permissions, tpWriteByGroup);
  if I AND $0008 <> 0 then Include (DirRec.Permissions, tpExecuteByGroup);
  if I AND $0004 <> 0 then Include (DirRec.Permissions, tpReadByOther);
  if I AND $0002 <> 0 then Include (DirRec.Permissions, tpWriteByOther);
  if I AND $0001 <> 0 then Include (DirRec.Permissions, tpExecuteByOther);
  if I AND $0200 <> 0 then Include (DirRec.Mode, tmSaveText);
  if I AND $0400 <> 0 then Include (DirRec.Mode, tmSetGid);
  if I AND $0800 <> 0 then Include (DirRec.Mode, tmSetUid);
  CASE Header.LinkFlag OF
    #0, '0' : DirRec.FileType := ftNormal;
    '1'     : DirRec.FileType := ftLink;
    '2'     : DirRec.FileType := ftSymbolicLink;
    '3'     : DirRec.FileType := ftCharacter;
    '4'     : DirRec.FileType := ftBlock;
    '5'     : DirRec.FileType := ftDirectory;
    '6'     : DirRec.FileType := ftFifo;
    '7'     : DirRec.FileType := ftContiguous;
    'D'     : DirRec.FileType := ftDumpDir;
    'M'     : DirRec.FileType := ftMultiVolume;
    'V'     : DirRec.FileType := ftVolumeHeader;
    end;
  DirRec.LinkName   := ExtractText (Header.LinkName);
  DirRec.UID        := ExtractNumber (@Header.UID);
  DirRec.GID        := ExtractNumber (@Header.GID);
  DirRec.UserName   := ExtractText (Header.UName);
  DirRec.GroupName  := ExtractText (Header.GName);
  DirRec.Magic      := Trim (ExtractText (Header.Magic));
  DirRec.MajorDevNo := ExtractNumber (@Header.DevMajor);
  DirRec.MinorDevNo := ExtractNumber (@Header.DevMinor);

  HeaderChkSum := ExtractNumber (@Header.ChkSum);   // Calc Checksum
  CheckSum := 0;
  StrMove (Header.ChkSum, CHKBLANKS, 8);
  FOR I := 0 TO SizeOf (TTarHeader)-1 DO
    INC (CheckSum, INTEGER (ORD (Rec [I])));
  DirRec.CheckSumOK := WORD (CheckSum) = WORD (HeaderChkSum);

  if DirRec.FileType in [ftLink, ftSymbolicLink, ftDirectory, ftFifo, ftVolumeHeader]
    then FBytesToGo := 0
    else FBytesToGo := DirRec.Size;
end;


procedure TTarArchive.ReadFile (Buffer : POINTER);
          // Reads file data for the last Directory record. The entire file is read into the buffer.
          // The buffer must be large enough to take up the whole file.
var
  RestBytes : INTEGER;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := records (FBytesToGo) * recordSIZE - FBytesToGo;
  FStream.ReadBuffer (Buffer^, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile (Stream : TStream);
          // Reads file data for the last Directory record.
          // The entire file is written out to the stream.
          // The stream is left at its current position prior to writing
var
  RestBytes : INTEGER;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := records (FBytesToGo) * recordSIZE - FBytesToGo;
  Stream.CopyFrom (FStream, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile (Filename : String);
          // Reads file data for the last Directory record.
          // The entire file is saved in the given Filename
var
  FS : TFileStream;
begin
  FS := TFileStream.Create (Filename, fmCreate);
  TRY
    ReadFile (FS);
  FINALLY
    FS.Free;
    end;
end;


function  TTarArchive.ReadFile : String;
          // Reads file data for the last Directory record. The entire file is returned
          // as a large ANSI String.
var
  RestBytes : INTEGER;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := records (FBytesToGo) * recordSIZE - FBytesToGo;
  SetLength (Result, FBytesToGo);
  FStream.ReadBuffer (PChar (Result)^, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.GetFilePos (var Current, Size : INT64);
          // Returns the Current Position in the TAR stream
begin
  Current := FStream.Position;
  Size    := FStream.Size;
end;


procedure TTarArchive.SetFilePos (NewPos : INT64);                   // Set new Current File Position
begin
  if NewPos < FStream.Size THEN
    FStream.Seek (NewPos, soFrombeginning);
end;


{
===============================================================================================
TTarWriter
===============================================================================================
}


constructor TTarWriter.CreateEmpty;
var
  TP : TTarPermission;
begin
  inherited Create;
  FOwnsStream  := FALSE;
  FFinalized   := FALSE;
  FPermissions := [];
  FOR TP := Low (TP) TO High (TP) DO
    Include (FPermissions, TP);
  FUID       := 0;
  FGID       := 0;
  FUserName  := '';
  FGroupName := '';
  FMode      := [];
  FMagic     := 'ustar';
end;

constructor TTarWriter.Create (TargetStream   : TStream);
begin
  CreateEmpty;
  FStream     := TargetStream;
  FOwnsStream := FALSE;
end;


constructor TTarWriter.Create (TargetFilename : String; Mode : INTEGER = fmCreate);
begin
  CreateEmpty;
  FStream     := TFileStream.Create (TargetFilename, Mode);
  FOwnsStream := TRUE;
end;


destructor TTarWriter.Destroy;
begin
  if NOT FFinalized then
  begin
    Finalize;
    FFinalized := TRUE;
  end;
  if FOwnsStream then
    FStream.Free;
  INHERITED Destroy;
end;


procedure TTarWriter.AddFile   (Filename : String;  TarFilename : String = '');
var
  S    : TFileStream;
  Date : TDateTime;
begin
  Date := FileTimeGMT (Filename);
  if TarFilename = ''
    then TarFilename := ConvertFilename (Filename)
    else TarFilename := ConvertFilename (TarFilename);

  S := TFileStream.Create (Filename, fmOpenRead or fmShareDenyWrite);
  try
    AddStream (S, TarFilename, Date);
  finally
    S.Free
  end;
end;


procedure TTarWriter.AddStream (Stream : TStream; TarFilename : String; FileDateGmt : TDateTime);
var
  DirRec      : TTarDirRec;
  Rec         : Array [0..recordSIZE-1] OF CHAR;
  BytesToRead : INT64;      // Bytes to read from the Source Stream
  BlockSize   : INT64;      // Bytes to write out for the current record
begin
  ClearDirRec (DirRec);
  DirRec.Name        := TarFilename;
  DirRec.Size        := Stream.Size - Stream.Position;
  DirRec.DateTime    := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftNormal;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
  BytesToRead := DirRec.Size;
  while BytesToRead > 0 do begin
    BlockSize := BytesToRead;
    if BlockSize > recordSIZE then BlockSize := recordSIZE;
    FillChar (Rec, recordSIZE, 0);
    Stream.Read (Rec, BlockSize);
    FStream.Write (Rec, recordSIZE);
    DEC (BytesToRead, BlockSize);
    end;
end;


procedure TTarWriter.AddString (Contents : String; TarFilename : String; FileDateGmt : TDateTime);
var
  S : TStringStream;
begin
  S := TStringStream.Create (Contents);
  TRY
    AddStream (S, TarFilename, FileDateGmt);
  FINALLY
    S.Free
    end
end;


procedure TTarWriter.AddDir (Dirname : String; DateGmt : TDateTime; MaxDirSize : INT64 = 0);
var
  DirRec      : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Dirname;
  DirRec.Size        := MaxDirSize;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftDirectory;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddSymbolicLink (Filename, Linkname : String; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftSymbolicLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddLink (Filename, Linkname : String; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddVolumeHeader (VolumeId           : String; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := VolumeId;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftVolumeHeader;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.Finalize;
          // Writes the end-Of-File Tag
          // Data after this tag will be ignored
          // The destructor calls this automatically if you didn't do it before
var
  Rec : Array [0..recordSIZE-1] OF CHAR;
begin
  FillChar (Rec, SizeOf (Rec), 0);
  FStream.Write (Rec, recordSIZE);
  FFinalized := TRUE;
end;


end.

