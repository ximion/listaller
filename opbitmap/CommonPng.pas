unit CommonPng;

interface

uses
  Classes;

type
  TLongWordArray = array[0..10000] of LongWord;
  PLongWordArray = ^TLongWordArray;
  TMyPalette = array of LongWord;

type
  TFilter = (fiNone=0, fiDiffH=1, fiDiffV=2, fiAverage=3, fiPaeth=4, fiBest=5);
  TTransparency = (trNo, trSingle, trAlpha);

function PaethPredictor (a, b, c : integer): integer;

procedure FilterLine(CurrentLine, PreviousLine : array of byte; var Dessin : array of byte;
                       LineNumber, LineWidth : integer; Filter : Tfilter; BytePerPixel : integer);
// apply the Filter on CurrentLine (and PreviousLine if needed) and put the result
// in Dessin
// LineNumber : number of the CurrentLine in the initial Bitmap
// LineWidth : width of the initial Bitmap

procedure WriteHeader(LeFlux : TStream);
// write the header of a png file

implementation

function PaethPredictor (a, b, c : integer): integer;
// a = left, b = above, c = upper left
var
  p, pa, pb, pc :integer;
begin
  p := a + b - c; //initial estimate
  pa := abs(p - a); //distances to a, b, c
  pb := abs(p - b);
  pc := abs(p - c);
  // return nearest of a,b,c,
  // breaking ties in order a,b,c.
  if (pa <= pb) AND (pa <= pc) then
    result:=a
  else
    if pb <= pc then
      result:=b
    else result:=c;
end;

procedure FilterLine(CurrentLine, PreviousLine : array of byte; var Dessin : array of byte;
                      LineNumber, LineWidth : integer; Filter : Tfilter; BytePerPixel : integer);
// apply the Filter on CurrentLine (and PreviousLine if needed) and put the result
// in Dessin
// LineNumber : number of the CurrentLine in the initial Bitmap
// LineWidth : width of the initial Bitmap

  procedure CreateLine(var TheLine : array of byte; TheFilter : TFilter);
  var
    J : integer;
  begin
    for J:=0 to LineWidth*BytePerPixel-1 do
    case TheFilter of
      fiNone : TheLine[J]:=CurrentLine[J];
      fiDiffH : if J<BytePerPixel then
        TheLine[J]:=CurrentLine[J]
      else
        TheLine[J]:=(256+CurrentLine[J]-CurrentLine[J-BytePerPixel]) mod 256;
      fiDiffV : TheLine[J]:=(256+CurrentLine[J]-PreviousLine[J]) mod 256;
      fiAverage : if J<BytePerPixel then
        TheLine[J]:=(256+CurrentLine[J]-PreviousLine[J] div 2) mod 256
      else
        TheLine[J]:=(256+CurrentLine[J]-(CurrentLine[J-BytePerPixel]+PreviousLine[J]) div 2) mod 256;
      fiPaeth : if J<BytePerPixel then
        TheLine[J]:=(256+CurrentLine[J]-PaethPredictor(0,PreviousLine[J],0)) mod 256
      else
        TheLine[J]:=(256+CurrentLine[J]-PaethPredictor(CurrentLine[J-BytePerPixel],PreviousLine[J],PreviousLine[J-BytePerPixel])) mod 256;
    end;
  end;

  function EvaluateLine(TheLine : array of byte) : integer;
  var
    J : integer;
  begin
    result:=0;
    for J:=0 to LineWidth*BytePerPixel-1 do
      inc(result,TheLine[J]);
  end;

var
  J, LowerLine : integer;
  NewLine, NewLine2 : array of byte;
begin
  setLength(NewLine,LineWidth*BytePerPixel);
  if Filter=fiBest then
  begin
    setLength(NewLine2,LineWidth*BytePerPixel);

    CreateLine(NewLine,fiNone);
    LowerLine:=EvaluateLine(NewLine);
    Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(fiNone);

    CreateLine(NewLine2,fiDiffH);
    J:=EvaluateLine(NewLine2);
    if J<LowerLine then
    begin
      LowerLine:=J;
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(fiDiffH);
      move(NewLine2[0],NewLine[0],LineWidth*BytePerPixel);
    end;

    CreateLine(NewLine2,fiDiffV);
    J:=EvaluateLine(NewLine2);
    if J<LowerLine then
    begin
      LowerLine:=J;
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(fiDiffV);
      move(NewLine2[0],NewLine[0],LineWidth*BytePerPixel);
    end;

    CreateLine(NewLine2,fiAverage);
    J:=EvaluateLine(NewLine2);
    if J<LowerLine then
    begin
      LowerLine:=J;
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(fiAverage);
      move(NewLine2[0],NewLine[0],LineWidth*BytePerPixel);
    end;

    CreateLine(NewLine2,fiPaeth);
    J:=EvaluateLine(NewLine2);
    if J<LowerLine then
    begin
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(fiPaeth);
      move(NewLine2[0],NewLine[0],LineWidth*BytePerPixel);
    end;

  end
  else begin
    Dessin[LineNumber*(LineWidth*BytePerPixel+1)]:=ord(Filter);
    CreateLine(NewLine,Filter);
  end;
  for J:=0 to LineWidth*BytePerPixel-1 do
    Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=NewLine[J];
{  for J:=0 to LineWidth*BytePerPixel-1 do
  case Filter of
    fiNone : Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=CurrentLine[J];
    fiDiffH : if J<BytePerPixel then
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=CurrentLine[J]
    else
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-CurrentLine[J-BytePerPixel]) mod 256;
    fiDiffV : Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-PreviousLine[J]) mod 256;
    fiAverage : if J<BytePerPixel then
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-PreviousLine[J] div 2) mod 256
    else
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-(CurrentLine[J-BytePerPixel]+PreviousLine[J]) div 2) mod 256;
    fiPaeth : if J<BytePerPixel then
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-PaethPredictor(0,PreviousLine[J],0)) mod 256
    else
      Dessin[LineNumber*(LineWidth*BytePerPixel+1)+J+1]:=(256+CurrentLine[J]-PaethPredictor(CurrentLine[J-BytePerPixel],PreviousLine[J],PreviousLine[J-BytePerPixel])) mod 256;
  end; }
end;

procedure WriteHeader(LeFlux : TStream);
// write the header of a png file
var
  B : byte;
begin
  B:=137; LeFlux.Write(B,1);
  B:=80; LeFlux.Write(B,1);
  B:=78; LeFlux.Write(B,1);
  B:=71; LeFlux.Write(B,1);
  B:=13; LeFlux.Write(B,1);
  B:=10; LeFlux.Write(B,1);
  B:=26; LeFlux.Write(B,1);
  B:=10; LeFlux.Write(B,1);
end;

end.
