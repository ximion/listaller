unit wincomp;

{d$O-}

interface

uses Types, Sysutils, opbitmap;

const
  WinColors16: array[0..15] of TColor =
  (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clSilver,
    clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

type
  TDeviceCap = (
    HORZSIZE,        {Horizontal size in millimeters}
    VERTSIZE,        {Vertical size in millimeters}
    HORZRES,         {Horizontal width in pixels}
    VERTRES,         {Vertical height in pixels}
    BITSPIXEL,       {Number of bits per pixel}
    PLANES,          {Number of planes}
    NUMCOLORS,
    LOGPIXELSX,      {Logical pixelsinch in X}
    LOGPIXELSY,      {Logical pixelsinch in Y}
    PHYSICALWIDTH,   {Physical Width in device units}
    PHYSICALHEIGHT,  {Physical Height in device units}
    PHYSICALOFFSETX, {Physical Printable Area X margin}
    PHYSICALOFFSETY  {Physical Printable Area Y margin}
  );

type

  TRGBQuad =
    packed record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
    rgbReserved: BYTE
  end;

  pRGBQuad = ^TRGBQuad;

  TRGBTriple =
    packed record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
  end;

  pRGBTriple = ^TRGBTriple;

  HPalette = cardinal;

  tagPALETTEENTRY = packed record
    peBlue: Byte; //swap
    peGreen: Byte;
    peRed: Byte; //swap
    peFlags: Byte;
  end;
  TPaletteEntry = tagPALETTEENTRY;

  PALETTEENTRY = tagPALETTEENTRY;

  TpalPalEntry = array[Byte] of tagPALETTEENTRY;

  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[Byte] of TPaletteEntry;
  end;
  PLogPalette = PMaxLogPalette;

function CreatePalette(const LogPalette: TMaxLogPalette): HPalette;
procedure ZeroMemory(Destination: Pointer; Length: DWORD);
function GetDeviceCaps(Handle: Cardinal; devcap: TDeviceCap): integer;
function RGB(r, g, b: byte): TColor;

threadvar SystemPalette16: cardinal;
threadvar GlPalette: TMaxLogPalette;

implementation

var Palette16: TMaxLogPalette;
var i: integer;

function CreatePalette(const LogPalette: TMaxLogPalette): HPalette;
var i: integer;
begin
  GlPalette.palVersion := LogPalette.palVersion;
  GlPalette.palNumEntries := LogPalette.palNumEntries;
  for i := 0 to GlPalette.palNumEntries - 1 do
    GlPalette.palPalEntry[i] := LogPalette.palPalEntry[i];
  Result := Cardinal(@GlPalette);
end;

procedure ZeroMemory(Destination: Pointer; Length: DWORD);
begin
  FillChar(Destination^, Length, 0);
end;

function GetDeviceCaps(Handle: Cardinal; devcap: TDeviceCap): integer;
begin
 Result:=32;
end;

function SwapD(Inp:SmallInt):Smallint;
begin
  Result:=Swap(Word(Inp));
end;


function RGB(r, g, b: byte): TColor;
begin
  result := (b) shl 16 + (g) shl 8 + (r);
end;

{.$O+}

initialization
  Palette16.palVersion := $300;
  Palette16.palNumEntries := 16;
  for i := 0 to 15 do
    cardinal(Palette16.palPalEntry[i]) := WinColors16[i];
  SystemPalette16 := Cardinal(@Palette16);
end.


