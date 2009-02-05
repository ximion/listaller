unit ftbresenham;
(******************************************************************************
Delphi implementation of Bresenham's Line/Circle/Ellipse algorithm.
Author: Finn Tolderlund
        Denmark
Date: 17.10.2002

homepage:
http://home20.inet.tele.dk/tolderlund/
http://finn.mobilixnet.dk/
e-mail:
finn@mail.tdcadsl.dk
finn.tolderlund@mobilixnet.dk

This unit can freely be used and distributed.

Disclaimer:
Use of this unit is on your own responsibility.
I will not under any circumstance be hold responsible for anything
which may or may not happen as a result of using this unit.
******************************************************************************)

interface

uses opbitmap;


procedure BresenhamLine(FromX, FromY, ToX, ToY: Integer; Canvas: TOPBitmapCanvas; Color: TColor);
procedure BresenhamCircle(CenterX, CenterY, Radius: Integer; Canvas: TOPBitmapCanvas; Color: TColor);
procedure BresenhamEllipse(CenterX, CenterY, XRadius, YRadius: Integer; Canvas: TOPBitmapCanvas);

implementation

procedure BresenhamLine(FromX, FromY, ToX, ToY: Integer; Canvas: TOPBitmapCanvas; Color: TColor);
{
Bresenham's Line Algorithm
Bresenham's Circle Algorithm
fra denne side:
http://www.funducode.com/freec/graphics/graphics2.htm
}
const
  INCR = 1;
  DECR = -1;
  PREDX = 1;
  PREDY = 0;
var
  dx, dy, e, e_inc, e_noinc: Integer;
procedure DrawLine(x1, y1, x2, y2, pred, incdec: Integer);
var
  i, istart, iend, ivar: Integer;
begin
  if ( pred = PREDX ) then
    begin
      istart := x1 ;
      iend := x2 ;
      ivar := y1 ;
    end
  else
    begin
      istart := y1 ;
      iend := y2 ;
      ivar := x1 ;
    end;
  for i := istart to iend do
    begin
      if ( pred = PREDY ) then
        Canvas.Pixels[ivar, i] := Color
      else
        Canvas.Pixels[i, ivar] := Color;
      if ( e < 0 ) then
        e := e + e_noinc
      else
        begin
          ivar := ivar + incdec ;
          e := e + e_inc ;
        end;
    end;
end;
var
  t, i: Integer;
  x1, y1, x2, y2: Integer;
begin
  x1 := FromX;
  y1 := FromY;
  x2 := ToX;
  y2 := ToY;

  if ( x1 > x2 ) then
    begin
      t := x1 ; x1 := x2 ; x2 := t ;
      t := y1 ; y1 := y2 ; y2 := t ;
    end;

  dx := x2 - x1 ; dy := y2 - y1 ;

  if ( dx = 0 ) then //* vertical line */
  begin
    if ( y1 > y2 ) then
      begin
        t := y1 ; y1 := y2 ; y2 := t ;
      end;
    for i := y1 to y2 do
      Canvas.Pixels[x1, i] := Color;
    Exit;
  end;

  if ( dy = 0 ) then  //* horizontal line */
    begin
      for i := x1 to x2-1 do
        Canvas.Pixels[i, y1] := Color;
      Exit;
    end;

  //* 0 < m < 1 */
  if ( dy < dx) and (dy > 0 ) then
    begin
      e_noinc := 2 * dy ;
      e := 2 * dy - dx ;
      e_inc := 2 * ( dy - dx ) ;
      drawline ( x1, y1, x2, y2, PREDX, INCR ) ;
    end;

  //* m = 1 */
  if ( dy = dx) and (dy > 0 ) then
    begin
      e_noinc := 2 * dy ;
      e := 2 * dy - dx ;
      e_inc := 2 * ( dy - dx ) ;
      drawline ( x1, y1, x2, y2, PREDX, INCR ) ;
    end;

  //* 1 < m < infinity */
  if ( dy > dx) and (dy > 0 ) then
    begin
      e_noinc := 2 * dx ;
      e := 2 * dx - dy ;
      e_inc := 2 * ( dx - dy ) ;
      drawline ( x1, y1, x2, y2, PREDY, INCR ) ;
    end;

  //* 0 > m > -1 */
  if ( -dy < dx) and (dy < 0 ) then
    begin
      dy := -dy ;
      e_noinc := 2 * dy ;
      e := 2 * dy - dx ;
      e_inc := 2 * ( dy - dx ) ;
      drawline ( x1, y1, x2, y2, PREDX, DECR ) ;
    end;

  //* m = -1 */
  if ( dy = -dx) and (dy < 0 ) then
    begin
      dy := -dy ;
      e_noinc := ( 2 * dy ) ;
      e := 2 * dy - dx ;
      e_inc := 2 * ( dy - dx ) ;
      drawline ( x1, y1, x2, y2, PREDX, DECR ) ;
    end;

  //* -1 > m > 0 */
  if ( -dy > dx) and (dy < 0 ) then
    begin
      dx := -dx ;
      e_noinc := - ( 2*dx ) ; e := 2 * dx - dy ;
      e_inc := - 2 * ( dx - dy ) ;
      drawline ( x2, y2, x1, y1, PREDY, DECR ) ;
    end;

end;

procedure BresenhamCircle(CenterX, CenterY, Radius: Integer; Canvas: TOPBitmapCanvas; Color: TColor);
{
Bresenham's Line Algorithm
Bresenham's Circle Algorithm
fra denne side:
http://www.funducode.com/freec/graphics/graphics2.htm
}
procedure PlotCircle(x, y, x1, y1: Integer);
begin
  Canvas.Pixels[x + x1, y + y1] := Color;
  Canvas.Pixels[x - x1, y + y1] := Color;
  Canvas.Pixels[x + x1, y - y1] := Color;
  Canvas.Pixels[x - x1, y - y1] := Color;
  Canvas.Pixels[x + y1, y + x1] := Color;
  Canvas.Pixels[x - y1, y + x1] := Color;
  Canvas.Pixels[x + y1, y - x1] := Color;
  Canvas.Pixels[x - y1, y - x1] := Color;
end;
var
  x, y, r: Integer;
  x1, y1, p: Integer;
begin
  x := CenterX;
  y := CenterY;
  r := Radius;
  x1 := 0;
  y1 := r;
  p := 3 - 2 * r;
  while ( x1 < y1 ) do
  begin
    plotcircle ( x, y, x1, y1) ;
    if ( p < 0 ) then
      p := p + 4 * x1 + 6
    else
      begin
        p := p + 4 * ( x1 - y1 ) + 10 ;
        y1 := y1 - 1 ;
      end;
    x1 := x1 + 1 ;
  end;
  if ( x1 = y1 ) then
    plotcircle ( x, y, x1, y1) ;
end;

procedure BresenhamEllipse(CenterX, CenterY, XRadius, YRadius: Integer; Canvas: TOPBitmapCanvas);
var PenCol:TColor;
{ http://homepage.smc.edu/kennedy_john/BELIPSE.PDF
A Fast Bresenham Type Algorithm For Drawing Ellipses
by
John Kennedy
Mathematics Department
Santa Monica College
1900 Pico Blvd.
Santa Monica, CA 90405
rkennedy@ix.netcom.com }
{ The subroutine called takes advantage of the symmetry in the ellipse.
We only calculate the points in the first quadrant,
but for each such point we actually plot other points at the
same time as indicated in the figure below.
The subroutine would normally be defined inside the above procedure.
Also note that and refer to the ellipse's center point. }

procedure Fill33(X,Y:integer);
begin
Canvas.Pixels[X-1, Y] := PenCol;
Canvas.Pixels[X, Y-1] := PenCol;
Canvas.Pixels[X, Y] := PenCol;
Canvas.Pixels[X+1, Y] := PenCol;
Canvas.Pixels[X, Y+1] := PenCol;
end;

procedure Plot4EllipsePoints(X,Y : longint);
begin
  if Canvas.Brush.Style=bsSolid then
  begin
  Canvas.MoveTo(CenterX+X-1, CenterY+Y-1);
  Canvas.LineTo(CenterX-X+1, CenterY+Y-1);
  Canvas.MoveTo(CenterX-X+1, CenterY-Y+1);
  Canvas.LineTo(CenterX+X-1, CenterY-Y+1);
  end;
{
  Canvas.Pixels[CenterX+X, CenterY+Y] := clyellow; {point in quadrant 1}
  Canvas.Pixels[CenterX-X, CenterY+Y] := clgreen; {point in quadrant 2}
  Canvas.Pixels[CenterX-X, CenterY-Y] := clblue; {point in quadrant 3}
  Canvas.Pixels[CenterX+X, CenterY-Y] := PenCol; {point in quadrant 4}
}
  Canvas.Pixels[CenterX+X, CenterY+Y] := PenCol; {point in quadrant 1}
  Canvas.Pixels[CenterX-X, CenterY+Y] := PenCol; {point in quadrant 2}
  Canvas.Pixels[CenterX-X, CenterY-Y] := PenCol; {point in quadrant 3}
  Canvas.Pixels[CenterX+X, CenterY-Y] := PenCol; {point in quadrant 4}

{
  Fill33(CenterX+X, CenterY+Y); {point in quadrant 1}
  Fill33(CenterX-X, CenterY+Y); {point in quadrant 2}
  Fill33(CenterX-X, CenterY-Y); {point in quadrant 3}
  Fill33(CenterX+X, CenterY-Y); {point in quadrant 4}
}
end;
var
  X, Y : longint;
  XChange, YChange : longint;
  EllipseError : longint;
  TwoASquare, TwoBSquare : longint;
  StoppingX, StoppingY : longint;
begin
  PenCol:=Canvas.Pen.Color;
  Canvas.Pen.Color:=Canvas.Brush.Color;
  TwoASquare := 2*XRadius*XRadius;
  TwoBSquare := 2*YRadius*YRadius;
  X := XRadius;
  Y := 0;
  XChange := YRadius*YRadius*(1 - 2*XRadius);
  YChange := XRadius*XRadius;
  EllipseError := 0;
  StoppingX := TwoBSquare*XRadius;
  StoppingY := 0;
  while ( StoppingX >= StoppingY ) do {1st set of points, y' > -1}
    begin
      Plot4EllipsePoints(X,Y); {subroutine appears later}
      inc(Y);
      inc(StoppingY, TwoASquare);
      inc(EllipseError, YChange);
      inc(YChange,TwoASquare);
      if ((2*EllipseError + XChange) > 0 ) then
        begin
          dec(X);
          dec(StoppingX, TwoBSquare);
          inc(EllipseError, XChange);
          inc(XChange,TwoBSquare);
        end;
    end;
  { 1st point set is done; start the 2nd set of points }
  X := 0;
  Y := YRadius;
  XChange := YRadius*YRadius;
  YChange := XRadius*XRadius*(1 - 2*YRadius);
  EllipseError := 0;
  StoppingX := 0;
  StoppingY := TwoASquare*YRadius;
  while ( StoppingX <= StoppingY ) do {2nd set of points, y' < -1}
    begin
      Plot4EllipsePoints(X,Y); {subroutine appears later}
      inc(X);
      inc(StoppingX, TwoBSquare);
      inc(EllipseError, XChange);
      inc(XChange,TwoBSquare);
      if ((2*EllipseError + YChange) > 0 ) then
        begin
          dec(Y);
          dec(StoppingY, TwoASquare);
          inc(EllipseError, YChange);
          inc(YChange,TwoASquare);
        end;
    end;
    Canvas.Pen.Color:=PenCol;
end;

end.
