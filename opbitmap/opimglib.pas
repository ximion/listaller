unit opimglib;

{$MODE delphi}{$H+}

interface

uses
  Classes, SysUtils, OpBitmap;


type

  T3x3FloatArray = array[0..2] of array[0..2] of Extended;

function Convolve(ABitmap: TOPBitmap; AMask: T3x3FloatArray; ABias: Integer): TCanvasOPBitmap;
procedure RotateBitmap_ads(SourceBitmap: TOPBitmap; out DestBitmap: TOPBitmap; Center: TPoint; Angle: Double; BGColor: TColor = clPurple);
procedure FTFloodFill(Bitmap: TOPBitmap; Pos: TPoint; FillColor: TColor; Tolerance: byte = 15);
procedure Soften(Bitmap: TCanvasOPBitmap);
procedure Sharpen(Bitmap: TCanvasOPBitmap);
procedure ReplaceColor(Bitmap: TCanvasOPBitmap; Pos: TPoint; NewColor: TColor; Tolerance: byte = 35);
procedure ReplaceColorRGB(Bitmap: TCanvasOPBitmap; Pos: TPoint; NewR, NewG, NewB: Byte; Tolerance: byte = 0);
procedure ReplaceNotColor(Bitmap: TCanvasOPBitmap; NonColor, NewColor: TColor; Tolerance: byte);
procedure BlackToAlpha(Bitmap: TCanvasOPBitmap);
procedure WhiteToAlpha(Bitmap: TCanvasOPBitmap);
function FindFirstPixelTop(Bitmap: TCanvasOPBitmap): integer;
function FindFirstPixelLeft(Bitmap: TCanvasOPBitmap): integer;

implementation

function Convolve(ABitmap: TOPBitmap; AMask: T3x3FloatArray;
  ABias: Integer): TCanvasOPBitmap;
var
  LRow1, LRow2, LRow3, LRowOut: PRGBTripleArray;
  LRow, LCol: integer;
  LNewBlue, LNewGreen, LNewRed: Extended;
  LCoef: Extended;
begin
  LCoef := 0;
  for LRow := 0 to 2 do
    for LCol := 0 to 2 do
      LCoef := LCoef + AMask[LCol, LRow];
  if LCoef = 0 then LCoef := 1;

  Result := TCanvasOPBitmap.Create;

  Result.Width := ABitmap.Width - 2;
  Result.Height := ABitmap.Height - 2;
  Result.PixelFormat := pf24bit;

  LRow2 := ABitmap.ScanLine[0];
  LRow3 := ABitmap.ScanLine[1];

  for LRow := 1 to ABitmap.Height - 2 do
  begin
    LRow1 := LRow2;
    LRow2 := LRow3;
    LRow3 := ABitmap.ScanLine[LRow + 1];

    LRowOut := Result.ScanLine[LRow - 1];

    for LCol := 1 to ABitmap.Width - 2 do
    begin
      LNewBlue :=
        (LRow1^[LCol - 1].rgbtBlue * AMask[0, 0]) + (LRow1^[LCol].rgbtBlue * AMask[1, 0]) +
        (LRow1^[LCol + 1].rgbtBlue * AMask[2, 0]) +
        (LRow2^[LCol - 1].rgbtBlue * AMask[0, 1]) + (LRow2^[LCol].rgbtBlue * AMask[1, 1]) +
        (LRow2^[LCol + 1].rgbtBlue * AMask[2, 1]) +
        (LRow3^[LCol - 1].rgbtBlue * AMask[0, 2]) + (LRow3^[LCol].rgbtBlue * AMask[1, 2]) +
        (LRow3^[LCol + 1].rgbtBlue * AMask[2, 2]);
      LNewBlue := (LNewBlue / LCoef) + ABias;
      if LNewBlue > 255 then
        LNewBlue := 255;
      if LNewBlue < 0 then
        LNewBlue := 0;

      LNewGreen :=
        (LRow1^[LCol - 1].rgbtGreen * AMask[0, 0]) + (LRow1^[LCol].rgbtGreen * AMask[1, 0]) +
        (LRow1^[LCol + 1].rgbtGreen * AMask[2, 0]) +
        (LRow2^[LCol - 1].rgbtGreen * AMask[0, 1]) + (LRow2^[LCol].rgbtGreen * AMask[1, 1]) +
        (LRow2^[LCol + 1].rgbtGreen * AMask[2, 1]) +
        (LRow3^[LCol - 1].rgbtGreen * AMask[0, 2]) + (LRow3^[LCol].rgbtGreen * AMask[1, 2]) +
        (LRow3^[LCol + 1].rgbtGreen * AMask[2, 2]);
      LNewGreen := (LNewGreen / LCoef) + ABias;
      if LNewGreen > 255 then
        LNewGreen := 255;
      if LNewGreen < 0 then
        LNewGreen := 0;

      LNewRed :=
        (LRow1^[LCol - 1].rgbtRed * AMask[0, 0]) + (LRow1^[LCol].rgbtRed * AMask[1, 0])
        + (LRow1^[LCol + 1].rgbtRed * AMask[2, 0]) +
        (LRow2^[LCol - 1].rgbtRed * AMask[0, 1]) + (LRow2^[LCol].rgbtRed * AMask[1, 1])
        + (LRow2^[LCol + 1].rgbtRed * AMask[2, 1]) +
        (LRow3^[LCol - 1].rgbtRed * AMask[0, 2]) + (LRow3^[LCol].rgbtRed * AMask[1, 2])
        + (LRow3^[LCol + 1].rgbtRed * AMask[2, 2]);
      LNewRed := (LNewRed / LCoef) + ABias;
      if LNewRed > 255 then
        LNewRed := 255;
      if LNewRed < 0 then
        LNewRed := 0;

      LRowOut^[LCol - 1].rgbtBlue := trunc(LNewBlue);
      LRowOut^[LCol - 1].rgbtGreen := trunc(LNewGreen);
      LRowOut^[LCol - 1].rgbtRed := trunc(LNewRed);
    end;
  end;
end;

procedure RotateBitmap_ads(
  SourceBitmap: TOPBitmap;
  out DestBitmap: TOPBitmap;
  Center: TPoint;
  Angle: Double;
  BGColor: TColor);
var
  cosRadians: Double;
  inX: Integer;
  inXOriginal: Integer;
  inXPrime: Integer;
  inXPrimeRotated: Integer;
  inY: Integer;
  inYOriginal: Integer;
  inYPrime: Integer;
  inYPrimeRotated: Integer;
  OriginalRow: pRGBTripleArray;
  Radians: Double;
  RotatedRow: pRGBTripleArray;
  sinRadians: Double;
begin
  SourceBitmap.PixelFormat := pf24bit;
  DestBitmap.Width := SourceBitmap.Width;
  DestBitmap.Height := SourceBitmap.Height;
  DestBitmap.PixelFormat := pf24bit;
  Radians := -(Angle) * PI / 180;
  sinRadians := Sin(Radians);
  cosRadians := Cos(Radians);
  for inX := DestBitmap.Height - 1 downto 0 do
  begin
    RotatedRow := DestBitmap.Scanline[inX];
    inXPrime := 2 * (inX - Center.y) + 1;
    for inY := DestBitmap.Width - 1 downto 0 do
    begin
      inYPrime := 2 * (inY - Center.x) + 1;
      inYPrimeRotated := Round(inYPrime * CosRadians - inXPrime * sinRadians);
      inXPrimeRotated := Round(inYPrime * sinRadians + inXPrime * cosRadians);
      inYOriginal := (inYPrimeRotated - 1) div 2 + Center.x;
      inXOriginal := (inXPrimeRotated - 1) div 2 + Center.y;
      if
        (inYOriginal >= 0) and
        (inYOriginal <= SourceBitmap.Width - 1) and
        (inXOriginal >= 0) and
        (inXOriginal <= SourceBitmap.Height - 1)
        then
      begin
        OriginalRow := SourceBitmap.Scanline[inXOriginal];
        RotatedRow[inY] := OriginalRow[inYOriginal]
      end
      else
      begin
        RotatedRow^[inY].rgbtBlue := Byte(BGColor shr 16);
        RotatedRow^[inY].rgbtGreen := Byte(BGColor shr 8);
        RotatedRow^[inY].rgbtRed := Byte(BGColor);
      end;
    end;
  end;
end;

procedure FTFloodFill(Bitmap: TOPBitmap; Pos: TPoint; FillColor: TColor; Tolerance: byte);
var
  X, Y: Integer;
  ReplaceColor: TColor;
  Stack: array of TPoint;
  procedure PutInStack(X, Y: Integer);
  begin
    SetLength(Stack, Length(Stack) + 1);
    Stack[Length(Stack) - 1] := Point(X, Y);
  end;
  procedure GetFromStack(var X, Y: Integer);
  begin
    X := Stack[Length(Stack) - 1].X;
    Y := Stack[Length(Stack) - 1].Y;
    SetLength(Stack, Length(Stack) - 1);
  end;
begin
  X := Pos.X;
  Y := Pos.Y;
  if (X >= Bitmap.Width) or (Y >= Bitmap.Height) then
    Exit;
  ReplaceColor := Bitmap.Pixels[X, Y];
  if ReplaceColor = FillColor then
    Exit;
  PutInStack(X, Y);
  while Length(Stack) > 0 do
    with Bitmap do
    begin
      GetFromStack(X, Y);
      while (X > 0) and ColorInRange(Pixels[X - 1, Y], ReplaceColor, Tolerance) do
        Dec(X);
      while (X < Bitmap.Width) and ColorInRange(Pixels[X, Y], ReplaceColor, Tolerance) do
      begin
        if Y > 0 then
          if ColorInRange(Pixels[X, Y - 1], ReplaceColor, Tolerance) then
            PutInStack(X, Y - 1);
        if Y + 1 < Bitmap.Height then
          if ColorInRange(Pixels[X, Y + 1], ReplaceColor, Tolerance) then
            PutInStack(X, Y + 1);
        Pixels[X, Y] := FillColor;
        Inc(X);
      end;
    end;
end;

procedure Soften(Bitmap: TCanvasOPBitmap);
const val = 0.0009;
var
  LMask: T3x3FloatArray;
  OPB: TCanvasOPBitmap;
begin
  LMask[0, 0] := val;
  LMask[1, 0] := val;
  LMask[2, 0] := val;
  LMask[0, 1] := val;
  LMask[1, 1] := 0.0123;
  LMask[2, 1] := val;
  LMask[0, 2] := val;
  LMask[1, 2] := val;
  LMask[2, 2] := val;
  Bitmap.PixelFormat := pf24bit;
  OPB := Convolve(Bitmap, LMask, 0);
  Bitmap.canvas.Draw(1, 1, OPB);
  OPB.free;
end;

procedure Sharpen(Bitmap: TCanvasOPBitmap);
const val = -0.08;
var
  LMask: T3x3FloatArray;
  OPB: TCanvasOPBitmap;
begin
  LMask[0, 0] := val;
  LMask[1, 0] := val;
  LMask[2, 0] := val;
  LMask[0, 1] := val;
  LMask[1, 1] := 2;
  LMask[2, 1] := val;
  LMask[0, 2] := val;
  LMask[1, 2] := val;
  LMask[2, 2] := val;
  Bitmap.PixelFormat := pf24bit;
  OPB := Convolve(Bitmap, LMask, 0);
  Bitmap.canvas.Draw(1, 1, OPB);
  OPB.free;
end;

procedure ReplaceColor(Bitmap: TCanvasOPBitmap; Pos: TPoint; NewColor: TColor; Tolerance: byte);
var X, Y: integer;
  FindColor: TColor;
begin
  FindColor := Bitmap.Pixels[Pos.X, Pos.Y];
  for Y := 0 to Bitmap.Height do
    for X := 0 to Bitmap.Width do
      if ColorInRange(Bitmap.Pixels[X, Y], FindColor, Tolerance) then Bitmap.Pixels[X, Y] := NewColor;
end;

procedure ReplaceNotColor(Bitmap: TCanvasOPBitmap; NonColor, NewColor: TColor; Tolerance: byte);
var X, Y: integer;
  FindColor: TColor;
begin
  for Y := 0 to Bitmap.Height do
    for X := 0 to Bitmap.Width do
      if not ColorInRange(Bitmap.Pixels[X, Y], NonColor, Tolerance) then Bitmap.Pixels[X, Y] := NewColor;
end;

procedure ReplaceColorRGB(Bitmap: TCanvasOPBitmap; Pos: TPoint; NewR, NewG, NewB: Byte; Tolerance: byte);
var X, Y: integer;
  Value, FindC: Pixel32;
begin
  Bitmap.PixelFormat := pf32bit;
  Value.Red := NewR;
  Value.Green := NewG;
  Value.Blue := NewB;

  FindC := TBitmapData32(Bitmap.Data).NativePixels[Pos.x, Pos.y];
  for Y := 0 to Bitmap.Height - 1 do
    for X := 0 to Bitmap.Width - 1 do
      if (TBitmapData32(Bitmap.Data).NativePixels[x, y].Red = FindC.Red) and
        (TBitmapData32(Bitmap.Data).NativePixels[x, y].Green = FindC.Green) and
        (TBitmapData32(Bitmap.Data).NativePixels[x, y].Blue = FindC.Blue) then
      begin
        Value.Alpha := TBitmapData32(Bitmap.Data).NativePixels[x, y].Alpha;
        TBitmapData32(Bitmap.Data).NativePixels[x, y] := Value;
      end;
end;

procedure BlackToAlpha(Bitmap: TCanvasOPBitmap);
var Value: Pixel32;
  x, y: integer;
begin
  Bitmap.PixelFormat := pf32bit;
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin
      Value := TBitmapData32(Bitmap.Data).NativePixels[x, y];
      Value.Alpha := 255 - (Value.Red + Value.Green + Value.Blue) div 3;
      Value.Red := 0;
      Value.Green := 0;
      Value.Blue := 0;
      TBitmapData32(Bitmap.Data).NativePixels[x, y] := Value;
    end;
end;

procedure WhiteToAlpha(Bitmap: TCanvasOPBitmap);
var Value: Pixel32;
  x, y: integer;
begin
  Bitmap.PixelFormat := pf32bit;
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin
      Value := TBitmapData32(Bitmap.Data).NativePixels[x, y];
      Value.Alpha := (Value.Red + Value.Green + Value.Blue) div 3;
      Value.Red := 0;
      Value.Green := 0;
      Value.Blue := 0;
      TBitmapData32(Bitmap.Data).NativePixels[x, y] := Value;
    end;
end;

function FindFirstPixelTop(Bitmap: TCanvasOPBitmap): integer;
var findcolor: TColor;
var x, y: integer;
var done: boolean;
begin
  done := false;
  findcolor := Bitmap.Canvas.Pixels[1, 1];
  for y := 0 to Bitmap.Height - 1 do
  begin
    for x := 0 to Bitmap.Width - 1 do
    begin
      if not ColorInRange(Bitmap.Canvas.Pixels[x, y], findColor, 5) then
      begin
        Result := y;
        done := true;
        break;
      end;
    end;
    if done then break;
  end;
end;

function FindFirstPixelLeft(Bitmap: TCanvasOPBitmap): integer;
var findcolor: TColor;
var x, y: integer;
var done: boolean;
begin
  done := false;
  findcolor := Bitmap.Canvas.Pixels[1, 1];
  for x := 0 to Bitmap.Width - 1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      if not ColorInRange(Bitmap.Canvas.Pixels[x, y], findColor, 5) then
      begin
        Result := x;
        done := true;
        break;
      end;
    end;
    if done then break;
  end;
end;

end.
