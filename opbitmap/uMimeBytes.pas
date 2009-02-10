{ *************************************************************************** }
{                                                                             }
{                                                                             }
{ Copyright © 2003 Theo Lustenberger                                          }
{                                                                             }
{ This program is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU General Public                         }
{ License as published by the Free Software Foundation; either                }
{ version 2 of the License, or (at your option) any later version.            }
{                                                                             }
{ This program is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ General Public License for more details.                                    }
{                                                                             }
{ You should have received a copy of the GNU General Public License           }
{ along with this program; see the file COPYING.  If not, write to            }
{ the Free Software Foundation, Inc., 59 Temple Place - Suite 330,            }
{ Boston, MA 02111-1307, USA.                                                 }
{                                                                             }
{ *************************************************************************** }

unit uMimeBytes;

interface
const MimeBufferSize = $C00;
type
  PByteArr = ^TByteArr;
  TByteArr = array[0..MimeBufferSize] of Byte;
const
  digits: string[16] = '0123456789ABCDEF';
function ByteArrToString(inp: TByteArr; Len: integer): string;
function StringToByteArr(inp: string; var outp: TByteArr): integer;
function CompareByteArr(A1, A2: TByteArr; Len: integer): boolean;
function CompareOffsetByteArr(Arr, SubArr: TByteArr; SubLen, Offset: integer): boolean;
function CompareByteArrRange(Arr, SubArr: TByteArr; SubLen, Start, Ende: integer): boolean;
function DecodeText(Text: string): string; overload;
function DecodeText(Text: string; var outp: TByteArr): integer; overload;
function MaskArr(Inp, Mask: TByteArr; Len: integer): TByteArr;
procedure GetOffset(inp: string; var s, e: integer);
function HexStringToByteArr(inp: string; var outp: TByteArr): integer;
function HTMLDecodeMagic(inp: string): string;

implementation

uses Sysutils;

function Convert_digit(c: char): integer;
var
  digit: integer;
begin
  digit := ord(upcase(c));
  if digit in [ord('0')..ord('9')] then
    digit := digit - ord('0')
  else if digit in [ord('A')..ord('F')] then
    digit := digit - ord('A') + 10
  else
    digit := -1;
  convert_digit := digit;
end;

function Convert(s: string; source, target: integer): string;
var
  decimal, place: longint;
  i, digit, d: integer;
  s2: string;
  msd: boolean;
  error: boolean;
begin
  place := 1;
  decimal := 0;
  s2 := '';
  i := length(s);
  error := false;
  while (i > 0) and (not error) do begin
    digit := convert_digit(s[i]);
    if (digit < 0) or (digit >= source) then
      error := true
    else begin
      decimal := decimal + place * digit;
      place := place * source;
      dec(i);
    end;
  end;
  if not error then begin
    msd := false;
    place := 1;
    while place < decimal do place := place * target;
    while place > 0 do begin
      d := decimal div place;
      if (d <> 0) or msd then begin
        s2 := s2 + digits[d + 1];
        msd := true;
      end;
      decimal := decimal mod place;
      place := place div target;
    end;
  end else
    s2 := '0';
  convert := s2;
end;

function HTMLDecodeMagic(inp: string): string;
begin
  result := StringReplace(inp, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
end;

function HexStrToInt(s1: string): Integer;
var d1: Cardinal; b1: Byte; i: Integer;
begin
  d1 := 0;
  for i := 1 to Length(s1) do begin
    if s1[i] <> ' ' then begin
      d1 := d1 shl 4;
      b1 := Ord((s1[i])) - 48;
      if b1 > 41 then b1 := b1 - 32;
      if b1 > 9 then b1 := b1 - 7;
      d1 := d1 or b1;
    end;
  end;
  Result := d1;
end;

function DecodeText(Text: string): string;
var count: integer;
  c1, c2, c3: char;
begin
  Result := '';
  count := 1;
  while count <= length(text) do
  begin
    if (Text[count] = '\') and (Text[count + 1] in ['0'..'9', 'x']) then
    begin
      inc(count);
      c1 := Text[count];
      inc(count);
      c2 := Text[count];
      inc(count);
      c3 := Text[count];
      if c1 = 'x' then
      begin
        Result := Result + Chr(HexStrToInt(c2 + c3));
      end else
        Result := Result + Chr(StrTointDef(Convert(c1 + c2 + c3, 8, 10), 0));
    end else Result := Result + Text[count];
    inc(count);
  end;
end;

function DecodeText(Text: string; var outp: TByteArr): integer;
var count: integer;
  c1, c2, c3: char;
begin
  Result := 0;
  count := 1;
  while count <= length(text) do
  begin
    if (Text[count] = '\') and (Text[count + 1] in ['0'..'9', 'x']) then
    begin
      inc(count);
      c1 := Text[count];
      inc(count);
      c2 := Text[count];
      inc(count);
      c3 := Text[count];
      if c1 = 'x' then
      begin
        outp[Result] := HexStrToInt(c2 + c3);
        inc(Result);
      end else
      begin
        outp[Result] := StrTointDef(Convert(c1 + c2 + c3, 8, 10), 0);
        inc(Result);
      end
    end else
    begin
      outp[Result] := Ord(Text[count]);
      inc(Result);
    end;
    inc(count);
  end;
end;

procedure GetOffset(inp: string; var s, e: integer);
var idpos: integer;
begin
  s := 0;
  e := 0;
  idpos := pos(':', inp);
  if idpos = 0 then s := strToIntDef(inp, 0) else
  begin
    s := strToIntDef(copy(inp, 1, idpos - 1), 0);
    e := strToIntDef(copy(inp, idpos + 1, length(inp)), 0);
  end;
end;

function MaskArr(Inp, Mask: TByteArr; Len: integer): TByteArr;
var i: integer;
begin
  if Len < High(TByteArr) then
    for i := 0 to Len do Result[i] := Inp[i] and Mask[i];
end;

function StringToByteArr(inp: string; var outp: TByteArr): integer;
var i: integer;
begin
  Result := Length(inp);
  if Result < High(TByteArr) then
    for i := 0 to Result - 1 do outp[i] := ord(inp[i + 1])
  else Result := -1;
end;

function HexStringToByteArr(inp: string; var outp: TByteArr): integer;
var i, Len: integer;
begin
  if copy(inp, 1, 2) = '0x' then
  begin
    Delete(inp, 1, 2);
    Len := Length(inp);
    Result := Len div 2;
    i := 0;
    while i < Len do
    begin
      outp[i div 2] := HexStrToInt(inp[i + 1] + inp[i + 2]);
      inc(i, 2);
    end;
  end else
  begin
    outp[0] := strtoint(inp);
    Result := 1;
  end;
end;

function ByteArrToString(inp: TByteArr; Len: integer): string;
var i: integer;
begin
  if Len < High(TByteArr) then
  begin
    SetLength(Result, Len);
    for i := 0 to Len - 1 do Result[i + 1] := chr(inp[i]);
  end else Result := '';
end;

function CompareByteArr(A1, A2: TByteArr; Len: integer): boolean;
begin
  if Len < High(TByteArr) then
    Result := CompareMem(@A1, @A2, Len) else
    Result := false;
end;

function CompareOffsetByteArr(Arr, SubArr: TByteArr; SubLen, offset: integer): boolean;
var a: Pointer;
begin
  if (SubLen + Offset) < High(TByteArr) then
  begin
    a := @Arr;
    if Offset > 0 then inc(ptruint(a), offset);
    Result := CompareMem(a, @SubArr, SubLen);
  end else Result := false;
end;

function CompareByteArrRange(Arr, SubArr: TByteArr; SubLen, Start, Ende: integer): boolean;
var
  i: integer;
  a: Pointer;
begin
  result := false;
  a := @Arr;
  inc(ptruint(a), Start);
  for i := Start to Ende do
  begin
    if CompareMem(a, @SubArr, SubLen) then
    begin
      result := true;
      break;
    end;
    inc(ptruint(a));
  end;
end;

end.
