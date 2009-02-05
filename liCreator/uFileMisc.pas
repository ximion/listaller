{-----------------------------------------------------------------------------
 Unit Name: uFileMisc
 Author:    Copyright (C) 2007  Andreas Frieﬂ and Erhard Kieling
 Date:      17-Apr-2007
 Purpose:
  based on ProjectIntf.pas of Lazarus from Mattias Gaertner

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

 History:
-----------------------------------------------------------------------------}
unit uFileMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls;

type

  TSampleSearchResults = procedure (ADir, AFileName: string) of object;

{ search a file in one directory }
function SearchFile(AInitDir,
                    AFileName  : string;
                    ShowContent: TSampleSearchResults): string; overload;
{ search a directory and a spezific file in this }
function SearchFile(AStartDir,
                    ASubDir,
                    AFileName  : string;
                    ShowContent: TSampleSearchResults): string; overload;

implementation

{-----------------------------------------------------------------------------
  Function : SearchFile
  Author:    Erhard Kieling
  Date:      17-Apr-2007
  Arguments: AInitDir, AFileName: string;
             ShowContent: TSampleSearchResults
  Result:    string
-----------------------------------------------------------------------------}
function SearchFile(AInitDir,
                    AFileName  : string;
                    ShowContent: TSampleSearchResults): string;
const
  cProcName = 'uFileMisc.SearchFile';

  function SetErrorParams: string;
  begin
    Result:= '';
    // 'AInitDir, AFileName: string; AFileBox : TListBox'
    // 'string';
  end; // of function SetErrorParams: string

var
  sr     : TSearchRec;
  DirBuf : string;
  ThisDir: string;
  fn     : string;
  fprefix,
  fsuffix: string;
begin
  { function body }
  Result:= '';
  if (AFileNAme <> '') then begin
    FillChar(sr, SizeOf(sr), #0);
    GetDir(0, DirBuf);
    {$I-}
    if (AInitDir <> '') then ChDir(AInitDir);
    if (IOResult = 0) then begin
      GetDir(0, ThisDir);
      if (pos('*', AFileName) = 1) then begin
        fsuffix:= ExtractFileExt(AFileName);
        if (fsuffix = '') then fsuffix:= '.*';
        fn:= '*' + fsuffix;
      end
      else fn:= AFileName;
      if (FindFirst(fn, faAnyFile, sr) = 0) then begin
        repeat
          if (AnsiCompareStr(sr.Name, AFileName) = 0) then begin
//          if (ExtractFileExt(sr.Name) = fsuffix) then begin
            Result:= '';
            if (ShowContent <> nil)
              then ShowContent('', ThisDir + PathDelim + sr.Name);
            if (AnsiCompareStr(sr.Name, AFileName) = 0) then begin
              Result:= ThisDir + PathDelim + sr.Name;
              Break;
            end; // of if good then begin
          end; // of if good then begin
        until (FindNext(sr) <> 0);
      end; // of if (FindFirst(fsuffix, faAnyFile, sr) = 0) then begin
      FindClose(sr);
    end; // of if (IOResult = 0) then begin
    {$I+}
    ChDir(DirBuf);
  end; // of if (AInitDir <> '') and (AFileNAme <> '') then begin
end; // of SearchFile

{-----------------------------------------------------------------------------
  Function : SearchFile
  Author:    Erhard Kieling
  Date:      17-Apr-2007
  Arguments: AStartDir, ASubDir,
             AFileName: string;
             ShowContent: TSampleSearchResults
  Result:    string
-----------------------------------------------------------------------------}
function SearchFile(AStartDir,
                    ASubDir,
                    AFileName  : string;
                    ShowContent: TSampleSearchResults): string;
const
  cProcName = 'uFileMisc.SearchFile';

  function SetErrorParams: string;
  begin
    Result:= '';
    // 'AStartDir, ASubDir, AFileName: string; ADirBox : TListBox; AFileBox: TListBox'
    // 'string';
  end; // of function SetErrorParams: string

var
  sr     : TSearchRec;
  ThisDir,
  DirBuf : string;

  function AttributeOK: boolean;
  begin
    Result:= (sr.Attr = faDirectory)
         and (sr.Name <> '.')
         and (sr.Name <> '..');
  end; // of AttributeOK
  
  function LookForSubDir: string;
  begin
    Result:= '';
    if (AsubDir <> '') then begin
      if (AnsiCompareStr(sr.Name, ASubDir) = 0)
        then Result:= SearchFile(sr.Name, AFileName, ShowContent);
    end; // of if (AsubDir <> '') then begin
  end; // of LookForSubDir
  
begin
  { function body }
 GetDir(0, DirBuf); Result:= '';
 FillChar(sr, SizeOf(sr), #0);
 if (AStartDir = '') then AStartDir:= PathDelim;
 {$I-}
 ChDir(AStartDir);
 if (IOResult = 0) then begin
   GetDir(0, ThisDir);
   Result:= SearchFile('', AFileName, ShowContent);
   if (Result = '') and (FindFirst('*.*', faDirectory, sr) = 0) then begin
     repeat
       if AttributeOK then begin
         Result:= '';
         if (ShowContent <> nil)
           then ShowContent(ThisDir + PathDelim + sr.Name, '');
         if (AsubDir <> '') and (AnsiCompareStr(sr.Name, ASubDir) = 0)
           then Result:= SearchFile(ASubDir, AFileName, ShowContent);
         if (Result = '')
           then Result:= SearchFile(sr.Name, ASubDir, AFileName, ShowContent);
         if (Result <> '') then Break;
       end; // of if AttributeOK then begin
     until (FindNext(sr) <> 0);
     FindClose(sr);
   end; // of if (Result = '') and (FindFirst('*.*', faDirectory, sr) = 0) then
 end; // of if (IOResult = 0) then begin
 {$I+}
 ChDir(DirBuf);
end; // of SearchFile

end.

