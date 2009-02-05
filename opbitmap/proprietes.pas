unit proprietes;

// this unit provides functions to determine the properties of
// a picture : number of colors, transparences and grey-scale

interface

uses
  {QGraphics,}  opbitmap,
  CommonPng;
  
  

function NbColors(Bitmap : TOPBitmap; Transparence, Remember : boolean) : integer;
// give the number of unique colors in the Bitmap
// if Transparence=true, take transparency into account
// if Remember=true, take into account color of fully transperent pixels
// if there are more then 256 colors, result is equal to 257

function GetPalette(Bitmap : TOPBitmap; Transparence, Remember : boolean) : TMyPalette;
// give a palette of unique colors in the Bitmap
// if Transparence=true, take transparency into account
// if Remember=true, take into account color of fully transperent pixels
// if there are more then 256 colors, it give the 257 first colors

function Transparence(Bitmap : TOPBitmap) : TTransparency;
// return trNo if there is no transparency
// return trSingle if there are only pixels with full transparency
// return trAlpha if there are pixels with partial transparency

function GreyScale(Bitmap : TOPBitmap; Remember : boolean) : boolean;
// return True if there is only grey colors
// if Remember=true, take into account color of fully transperent pixels

function TransparentColor(Bitmap : TOPBitmap; var TheColor : longword) : boolean;
// determine the transparent colors in a picture without partial transparency
// return True if there is only one fully transparent color
// return False if there are more transparent color
// return the first transparent color in TheColor
// result undefinied if there is no transparency or partial transparency

implementation


function NbColors(Bitmap : TOPBitmap; Transparence, Remember : boolean) : integer;
// give the number of unique colors in the Bitmap
// if Transparence=true, take transparency into account
// if Remember=true, take into account color of fully transperent pixels
// if there are more then 256 colors, result is equal to 257
begin
  result:=high(GetPalette(Bitmap, Transparence, Remember ))+1;
end;

function GetPalette(Bitmap : TOPBitmap; Transparence, Remember : boolean) : TMyPalette;
// give a palette of unique colors in the Bitmap
// if Transparence=true, take transparency into account
// if Remember=true, take into account color of fully transperent pixels
// if there are more then 256 colors, it give the 257 first colors
var
  I, J, Indice : integer;
  Ligne : PLongWordArray;
  LePoint, Masque : longword;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in NbColors');
  if Transparence then
    Masque:=$FFFFFFFF
  else
    Masque:=$00FFFFFF;
  SetLength(result,0);
  J:=0;
  while (J<Bitmap.Height) and (high(result)<256) do
  begin
    Ligne:=Bitmap.ScanLine[J];
    I:=0;
    while (I<Bitmap.Width) and (high(result)<256) do
    begin
      LePoint:=Ligne^[I] and Masque;
      if Transparence and not Remember and (LePoint shr 24=$00) then
        LePoint:=$FFFFFF; // if fully transparent and not Remember, set the color to White
      Indice:=0;
      while (Indice<=high(result)) and (LePoint<>result[Indice]) do
        inc(Indice);
      if Indice>high(result) then
      begin // new color
        SetLength(result,Indice+1);
        result[Indice]:=LePoint;
      end;
      inc(I);
    end;
    inc(J);
  end;
end;

function Transparence(Bitmap : TOPBitmap) : TTransparency;
// return trNo if there is no transparency
// return trSingle if there are only pixels with full transparency
// return trAlpha if there are pixels with partial transparency
var
  Opaque, Partiel : boolean;
  I, J : integer;
  Ligne : PLongWordArray;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in Transparence');
  Opaque:=true;
  Partiel:=false;
  J:=0;
  while (J<Bitmap.Height) and not Partiel do
  begin
    Ligne:=Bitmap.ScanLine[J];
    I:=0;
    while (I<Bitmap.Width) and not Partiel do
    begin
      if Opaque then
        Opaque:=((Ligne^[I] shr 24)=$FF);
      if not Opaque then
        Partiel:=(Ligne^[I] shr 24) in [$01..$FE];
      inc(I);
    end;
    inc(J);
  end;
  if Opaque then
    result:=trNo
  else
  if Partiel then
    result:=trAlpha
  else
    result:=trSingle;
end;

function GreyScale(Bitmap : TOPBitmap; Remember : boolean) : boolean;
// return True if there is only grey colors
// if Remember=true, take into account color of fully transperent pixels
var
  Gris : boolean;
  I, J  : integer;
  LePoint : LongWord;
  Ligne : PLongWordArray;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in GreyScale');
  Gris:=true;
  J:=0;
  while (J<Bitmap.Height) and Gris do
  begin
    Ligne:=Bitmap.ScanLine[J];
    I:=0;
    while (I<Bitmap.Width) and Gris do
    begin
      LePoint:=Ligne^[I];
      if not Remember and (LePoint shr 24=$00) then
        LePoint:=0; // if fully transparent and not Remember, set the color to Black
      Gris:=((LePoint and $FF)=((LePoint shr 8) and $FF)) and
            ((LePoint and $FF)=((LePoint shr 16) and $FF));
      inc(I);
    end;
    inc(J);
  end;
  result:=Gris;
end;

function TransparentColor(Bitmap : TOPBitmap; var TheColor : longword) : boolean;
// determine the transparent colors in a picture without partial transparency
// return True if there is only one fully transparent color
// return False if there are more transparent color
// return the first transparent color in TheColor
// result undefinied if there is no transparency or partial transparency
var
  I, J  : integer;
  Ligne : PLongWordArray;
  Rien : boolean;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in TransparentColor');
  Rien:=true;
  result:=true;
  I:=0;
  repeat
    Ligne:=Bitmap.ScanLine[I];
    J:=0;
    repeat
      if (Ligne^[J] shr 24) and $FF=0 then
      begin
        if Rien then
        begin
          TheColor:=Ligne^[J];
          Rien:=false;
        end
        else
          if Ligne^[J]<>TheColor then
            result:=false;
      end;
      inc(J);
    until (J>=Bitmap.Width) or not result;
    inc(I);
  until (I>=Bitmap.Height) or not result;
end;

end.
