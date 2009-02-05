unit pngallstream;

//Note: These procedures were in separate units in the original version
//Get the orginal from the author Eric Sibert: http://esibert.developpez.com/delphi/png/
//Changed for saving to stream and using with TOPBitmap 2007 Theo


interface

uses
  Classes, opbitmap, CommonPng;

procedure SaveToStreamPng(Bitmap : TOpBitmap; Stream : TStream;
                        Filter : TFilter; Remember : boolean);
// remember indicates if we MUST remeber the colors of fully transparent areas

procedure SaveToStreamGreyTransparent(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter;
                                Transparency : boolean;
                                TransGrey : byte);
// Transparency indicates if the TransGrey level is set to full transparency
// otherwise TransGrey is ignored and picture is recorded without transparency


procedure SaveToStreamPalette(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter; Alpha, Remember : boolean);
// Alpha indicates if Alpha Channel must be took into account
// otherwise picture is recorded without transparency
// if Remember=true, take into account color of fully transparent pixels

procedure SaveToStream16MTransparent(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter;
                                Transparency : boolean; TransColor : LongWord);
// Transparency indicates if the TransColor is set to full transparency
// otherwise TransColor is ignored and picture is recorded without transparency

procedure SaveToStreamGreyAlpha(Bitmap : TOPBitmap; MonFlux : TStream; Filter : TFilter);

procedure SaveToStream16MAlpha(Bitmap : TOPBitmap; MonFlux : TStream; Filter : TFilter);

implementation

uses
  proprietes, UnitIDAT, UnitIEND, UnitIHDR, UnittRNS, UnitPLTE, UnitBloc;


procedure SaveToStreamPng(Bitmap : TOPBitmap; Stream : TStream;
                        Filter : TFilter; Remember : boolean);
var
  Point : longword;
  Transparency : TTransparency;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStreamPng');
  Bitmap.OPProgress(Bitmap,opsStarting,0,true,Rect(0,0,0,0),'');
  Transparency:=Transparence(Bitmap);
  if Transparency=trNo then
  begin // no transparency
    if GreyScale(Bitmap,false) then
      SaveToStreamGreyTransparent(Bitmap, Stream, Filter, false, 0)
    else begin
      if NbColors(Bitmap, false, false)<=256 then
        SaveToStreamPalette(Bitmap, Stream, Filter, false, false)
      else
        SaveToStream16MTransparent(Bitmap, Stream, Filter, false, 0);
    end;
  end
  else begin // some transparency
    if NbColors(Bitmap, true, Remember)<=256 then // palette is favored versus greyscale in this case
      SaveToStreamPalette(Bitmap, Stream, Filter, True, Remember)
    else begin
      if GreyScale(Bitmap, Remember) then
        SaveToStreamGreyAlpha(Bitmap, Stream, Filter)
      else begin
        if Transparency=trAlpha then
          SaveToStream16MAlpha(Bitmap, Stream, Filter)
        else begin // only full transparency
          if TransparentColor(Bitmap, Point) then // only one transparent color
            SaveToStream16MTransparent(Bitmap, Stream, Filter, True, Point)
          else // not optimal, if not Remember, we can switch to the previous case
               // but we need to modify the Bitmap
            SaveToStream16MAlpha(Bitmap, Stream, Filter);
        end;
      end;
    end;
  end;
  Bitmap.OPProgress(Bitmap,opsEnding,100,true,Rect(0,0,0,0),'');
end;



procedure SaveToStreamGreyTransparent(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter;
                                Transparency : boolean;
                                TransGrey : byte);
var
  CurrentLine, PreviousLine : array of byte;
  BlocIDAT : TBlocIDAT;
  BlocIEND : TBlocIEND;
  BlocIHDR : TBlocIHDR;
  BloctRNS : TBloctRNS;
  I, J : integer;
  Dessin : array of byte;
  Ligne : PLongWordArray;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStreamGreyTransparent');
//  MonFlux:=TFileStream.Create(FileName,fmCreate);
  WriteHeader(MonFlux);

// IHDR bloc
// 8 : sampling deeph (256 levels grey levels)
// 0 : model 0 (GreyScale)
// 0 : compression method (only 0 defined)
// 0 : filtering method (only 0 defined)
// 0 : interlassing (0 none, 1 Adam7 not programmed)
  BlocIHDR:=TBlocIHDR.Create(MonFlux, Bitmap.Width,Bitmap.Height,8,0,0,0,0);
  BlocIHDR.Free;

  if Transparency then
  begin
    BloctRNS:=TBloctRNS.Create(MonFlux,TransGrey);
    BloctRNS.Free;
  end;

  SetLength(Dessin,(Bitmap.Width+1)*Bitmap.Height); // 1 bytes per pixel
  SetLength(CurrentLine,Bitmap.Width);
  SetLength(PreviousLine,Bitmap.Width);
  FillChar(CurrentLine[0],Bitmap.Width,0);
  for I:=0 to Bitmap.Height-1 do
  begin
    Ligne:=Bitmap.ScanLine[I];
    Bitmap.OPProgress(Bitmap,opsRunning,Round(I/Bitmap.Height*100),true,Rect(0,0,0,0),'');
    for J:=0 to Bitmap.Width-1 do
    begin
      PreviousLine[J]:=CurrentLine[J];
      CurrentLine[J]:=Ligne^[J] and $FF;
    end;

//    Dessin[I*(Bitmap.Width+1)]:=ord(Filter);
    FilterLine(CurrentLine, PreviousLine, Dessin, I, Bitmap.Width, Filter, 1);

  end;

  BlocIDAT:=TBlocIDAT.Create(MonFlux);
  BlocIDAT.AddUncompressed(Dessin);
  BlocIDAT.Free;

  BlocIEND:=TBlocIEND.Create(MonFlux);
  BlocIEND.Free;

//  MonFlux.Free;
end;

procedure SaveToStreamPalette(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter; Alpha, Remember : boolean);
var
  CurrentLine, PreviousLine : array of byte;
  BlocIDAT : TBlocIDAT;
  BlocIEND : TBlocIEND;
  BlocIHDR : TBlocIHDR;
  BloctRNS : TBloctRNS;
  BlocPLTE : TBlocPLTE;
//  MonFlux : TFileStream;
  I, J : integer;
  Indice : byte;
  Dessin : array of byte;
  Ligne : PLongWordArray;
  FullPalette : TMyPalette;
  AlphaPalette : array of byte;
  ColorPalette : array of TRGB;
  LePoint, Masque : longword;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStreamPalette');
  if Alpha then
    Masque:=$FFFFFFFF
  else
    Masque:=$00FFFFFF;

//  MonFlux:=TFileStream.Create(FileName,fmCreate);
  WriteHeader(MonFlux);

// IHDR bloc
// 8 : sampling deeph (256 levels per color channel)
// 3 : model 3 (256 color palette)
// 0 : compression method (only 0 defined)
// 0 : filtering method (only 0 defined)
// 0 : interlassing (0 none, 1 Adam7 not programmed)
  BlocIHDR:=TBlocIHDR.Create(MonFlux, Bitmap.Width,Bitmap.Height,8,3,0,0,0);
  BlocIHDR.Free;

  FullPalette:=GetPalette(Bitmap, Alpha, Remember);
  assert(high(FullPalette)<256,'Number of unique colors exceded capability of palette (256) in SaveToStreamPalette');

  if Alpha then  // palette ordering versus alpha
  begin
    for I:=low(FullPalette) to high(FullPalette)-1 do
    begin
      for J:=high(FullPalette)-1 downto I do
      begin
        if (FullPalette[J] shr 24) and $FF > (FullPalette[J+1] shr 24) and $FF then
        begin
          LePoint:=FullPalette[J];
          FullPalette[J]:=FullPalette[J+1];
          FullPalette[J+1]:=LePoint;
        end;
      end;
    end;
  end;

  SetLength(ColorPalette,high(FullPalette)+1);
  for I:=0 to high(FullPalette) do
  begin
    ColorPalette[I].Red:=(FullPalette[I] shr 16) and $FF;
    ColorPalette[I].Green:=(FullPalette[I] shr 8) and $FF;
    ColorPalette[I].Blue:=FullPalette[I] and $FF;
  end;
  BlocPLTE:=TBlocPLTE.Create(MonFlux,ColorPalette);
  BlocPLTE.Free;

  if Alpha then
  begin
    // we start by excluding colors without transparency
    J:=high(FullPalette);
    while (J>=0) and ((FullPalette[J] shr 24) and $FF=$FF) do
      dec(J);
    if J>=0 then // if J=-1, there is no transparency
    begin
      SetLength(AlphaPalette,J+1);
      for I:=0 to J do
        AlphaPalette[I]:=(FullPalette[I] shr 24) and $FF;
      BloctRNS:=TBloctRNS.Create(MonFlux,AlphaPalette);
      BloctRNS.Free;
    end;
  end;

  SetLength(Dessin,(Bitmap.Width+1)*Bitmap.Height); // 1 bytes per pixel
  SetLength(CurrentLine,Bitmap.Width);
  SetLength(PreviousLine,Bitmap.Width);
  FillChar(CurrentLine[0],Bitmap.Width,0);
  for I:=0 to Bitmap.Height-1 do
  begin
    Ligne:=Bitmap.ScanLine[I];
    Bitmap.OPProgress(Bitmap,opsRunning,Round(I/Bitmap.Height*100),true,Rect(0,0,0,0),'');
    for J:=0 to Bitmap.Width-1 do
    begin
      PreviousLine[J]:=CurrentLine[J];
      // the following lines must be in accordance to the one in proprietes.GetPalette
      LePoint:=Ligne^[J] and Masque;
      if Alpha and not Remember and (LePoint shr 24=$00) then
        LePoint:=$FFFFFF; // if fully transparent and not Remember, set the color to White
      Indice:=0;
      while LePoint<>FullPalette[Indice] do
        inc(Indice);
      CurrentLine[J]:=Indice;
    end;

    FilterLine(CurrentLine, PreviousLine, Dessin, I, Bitmap.Width, Filter, 1);

  end;

  BlocIDAT:=TBlocIDAT.Create(MonFlux);
  BlocIDAT.AddUncompressed(Dessin);
  BlocIDAT.Free;

  BlocIEND:=TBlocIEND.Create(MonFlux);
  BlocIEND.Free;

//  MonFlux.Free;
end;

procedure SaveToStream16MTransparent(Bitmap : TOPBitmap; MonFlux : TStream;
                                Filter : TFilter;
                                Transparency : boolean; TransColor : LongWord);
var
  CurrentLine, PreviousLine : array of byte;
  BlocIDAT : TBlocIDAT;
  BlocIEND : TBlocIEND;
  BlocIHDR : TBlocIHDR;
  BloctRNS : TBloctRNS;
//  MonFlux : TFileStream;
  I, J : integer;
  Dessin : array of byte;
  Ligne : PLongWordArray;
  TheColor : TRGB;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStream16Transparent');
//  MonFlux:=TFileStream.Create(FileName,fmCreate);
  WriteHeader(MonFlux);

// IHDR bloc
// 8 : sampling deeph (256 levels for each color channel)
// 2 : model 2 (Red Green Blue)
// 0 : compression method (only 0 defined)
// 0 : filtering method (only 0 defined)
// 0 : interlassing (0 none, 1 Adam7 not programmed)
  BlocIHDR:=TBlocIHDR.Create(MonFlux,Bitmap.Width,Bitmap.Height,8,2,0,0,0);
  BlocIHDR.Free;

  if Transparency then
  begin
    TheColor.Red:=(TransColor shr 16) and $FF;
    TheColor.Green:=(TransColor shr 8) and $FF;
    TheColor.Blue:=TransColor and $FF;
    BloctRNS:=TBloctRNS.Create(MonFlux,TheColor);
    BloctRNS.Free;
  end;

  SetLength(Dessin,(Bitmap.Width*3+1)*Bitmap.Height); // 3 bytes per pixel
  SetLength(CurrentLine,Bitmap.Width*3);
  SetLength(PreviousLine,Bitmap.Width*3);
  FillChar(CurrentLine[0],Bitmap.Width*3,0);
  for I:=0 to Bitmap.Height-1 do
  begin
    Ligne:=Bitmap.ScanLine[I];
   Bitmap.OPProgress(Bitmap,opsRunning,Round(I/Bitmap.Height*100),true,Rect(0,0,0,0),'');
    for J:=0 to Bitmap.Width-1 do
    begin
      PreviousLine[3*J]:=CurrentLine[3*J];
      CurrentLine[3*J]:=(Ligne^[J] shr 16) and $FF;
      PreviousLine[3*J+1]:=CurrentLine[3*J+1];
      CurrentLine[3*J+1]:=(Ligne^[J] shr 8) and $FF;
      PreviousLine[3*J+2]:=CurrentLine[3*J+2];
      CurrentLine[3*J+2]:=Ligne^[J] and $FF;
    end;

//    Dessin[I*(Bitmap.Width*3+1)]:=ord(Filter);

    FilterLine(CurrentLine, PreviousLine, Dessin, I, Bitmap.Width, Filter, 3);
  end;

  BlocIDAT:=TBlocIDAT.Create(MonFlux);
  BlocIDAT.AddUncompressed(Dessin);
  BlocIDAT.Free;

  BlocIEND:=TBlocIEND.Create(MonFlux);
  BlocIEND.Free;

//  MonFlux.Free;
end;

procedure SaveToStreamGreyAlpha(Bitmap : TOPBitmap; MonFlux : TStream; Filter : TFilter);
var
  CurrentLine, PreviousLine : array of byte;
  BlocIDAT : TBlocIDAT;
  BlocIEND : TBlocIEND;
  BlocIHDR : TBlocIHDR;
//  MonFlux : TFileStream;
  I, J : integer;
  Dessin : array of byte;
  Ligne : PLongWordArray;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStreamGreyAlpha');
//  MonFlux:=TFileStream.Create(FileName,fmCreate);
  WriteHeader(MonFlux);

// IHDR bloc
// 8 : sampling deeph (256 levels grey levels)
// 4 : model 4 (Grey Alpha)
// 0 : compression method (only 0 defined)
// 0 : filtering method (only 0 defined)
// 0 : interlassing (0 none, 1 Adam7 not programmed)
  BlocIHDR:=TBlocIHDR.Create(MonFlux,Bitmap.Width,Bitmap.Height,8,4,0,0,0);
  BlocIHDR.Free;

  SetLength(Dessin,(Bitmap.Width*2+1)*Bitmap.Height); // 2 bytes per pixel
  SetLength(CurrentLine,Bitmap.Width*2);
  SetLength(PreviousLine,Bitmap.Width*2);
  FillChar(CurrentLine[0],Bitmap.Width*2,0);
  for I:=0 to Bitmap.Height-1 do
  begin
    Ligne:=Bitmap.ScanLine[I];
    Bitmap.OPProgress(Bitmap,opsRunning,Round(I/Bitmap.Height*100),true,Rect(0,0,0,0),'');
    for J:=0 to Bitmap.Width-1 do
    begin
      PreviousLine[2*J]:=CurrentLine[2*J];
      CurrentLine[2*J]:=Ligne^[J] and $FF;
      PreviousLine[2*J+1]:=CurrentLine[2*J+1];
      CurrentLine[2*J+1]:=(Ligne^[J] shr 24) and $FF;
    end;

//    Dessin[I*(Bitmap.Width*2+1)]:=ord(Filter);
    FilterLine(CurrentLine, PreviousLine, Dessin, I, Bitmap.Width, Filter,2);

  end;

  BlocIDAT:=TBlocIDAT.Create(MonFlux);
  BlocIDAT.AddUncompressed(Dessin);
  BlocIDAT.Free;

  BlocIEND:=TBlocIEND.Create(MonFlux);
  BlocIEND.Free;

//  MonFlux.Free;
end;

procedure SaveToStream16MAlpha(Bitmap : TOPBitmap; MonFlux : TStream; Filter : TFilter);
var
  CurrentLine, PreviousLine : array of byte;
  BlocIDAT : TBlocIDAT;
  BlocIEND : TBlocIEND;
  BlocIHDR : TBlocIHDR;
//  MonFlux : TFileStream;
  I, J : integer;
  Dessin : array of byte;
  Ligne : PLongWordArray;
begin
  assert(Bitmap.PixelFormat=pf32bit,'Bad Bitmap format (not pf32bit) in SaveToStream16MAlpha');
//  MonFlux:=TFileStream.Create(FileName,fmCreate);
  WriteHeader(MonFlux);

// IHDR bloc
// 8 : sampling deeph (256 levels for each color channel)
// 6 : model 6 (Red Green Blue Alpha)
// 0 : compression method (only 0 defined)
// 0 : filtering method (only 0 defined)
// 0 : interlassing (0 none, 1 Adam7 not programmed)
  BlocIHDR:=TBlocIHDR.Create(MonFlux,Bitmap.Width,Bitmap.Height,8,6,0,0,0);
  BlocIHDR.Free;

  SetLength(Dessin,(Bitmap.Width*4+1)*Bitmap.Height); // 4 bytes per pixel
  SetLength(CurrentLine,Bitmap.Width*4);
  SetLength(PreviousLine,Bitmap.Width*4);
  FillChar(CurrentLine[0],Bitmap.Width*4,0);
  for I:=0 to Bitmap.Height-1 do
  begin
    Ligne:=Bitmap.ScanLine[I];
    Bitmap.OPProgress(Bitmap,opsRunning,Round(I/Bitmap.Height*100),true,Rect(0,0,0,0),'');
    for J:=0 to Bitmap.Width-1 do
    begin
      PreviousLine[4*J]:=CurrentLine[4*J];
      CurrentLine[4*J]:=(Ligne^[J] shr 16) and $FF;
      PreviousLine[4*J+1]:=CurrentLine[4*J+1];
      CurrentLine[4*J+1]:=(Ligne^[J] shr 8) and $FF;
      PreviousLine[4*J+2]:=CurrentLine[4*J+2];
      CurrentLine[4*J+2]:=Ligne^[J] and $FF;
      PreviousLine[4*J+3]:=CurrentLine[4*J+3];
      CurrentLine[4*J+3]:=(Ligne^[J] shr 24) and $FF;
    end;

//    Dessin[I*(Bitmap.Width*4+1)]:=ord(Filter);
    FilterLine(CurrentLine, PreviousLine, Dessin, I, Bitmap.Width, Filter, 4);
  end;

  BlocIDAT:=TBlocIDAT.Create(MonFlux);
  BlocIDAT.AddUncompressed(Dessin);
  BlocIDAT.Free;

  BlocIEND:=TBlocIEND.Create(MonFlux);
  BlocIEND.Free;

//  MonFlux.Free;
end;





end.
