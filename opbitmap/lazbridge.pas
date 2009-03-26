unit lazbridge;

{ *************************************************************************** }
{ Copyright (c) 2007 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

{$MODE objfpc}{$H+}
{_$DEFINE VER_MINIMAL}

interface

uses Classes, SysUtils, Graphics, GraphType, InterfaceBase, LCLType,
  IntfGraphics, FPimage, LCLIntf, ExtDlgs, FileUtil, ExtCtrls,
  opbitmap{$IFNDEF VER_MINIMAL}, opbitmapformats{$ENDIF} ;

type

  { TOPOpenDialog }

{$IFNDEF VER_MINIMAL}

  TOPOpenDialog = class(TOpenPictureDialog)
  private
    FPreviewFilename: string;
  protected
    procedure UpdatePreview; override;
    function Execute: boolean; override;
  end;


  { TLazOPPicture }

  TLazOPPicture = class(TOPPicture)
  private
    fImage: TImage;
    fUpdateImageSize: Boolean;
  public
    constructor Create(Image: TImage);
    procedure DrawImage;
    property UpdateImageSize: Boolean read fUpdateImageSize write fUpdateImageSize;
  end;
{$ENDIF}

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap);
procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);

implementation

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
var int: TLazIntfImage;
  x, y, mx, my: integer;
  haveMaskPix: boolean;
begin
  haveMaskPix:=not Bitmap.Transparent;
  int := Bitmap.CreateIntfImage;
  writeln(int.Height,int.Width);
  OpBitmap.Width := int.Width;
  OpBitmap.Height := int.Height;
  OpBitmap.Pixelformat := PixelFormatFromBPP(Int.DataDescription.BitsPerPixel);
  for y := 0 to OpBitmap.Height - 1 do
    for x := 0 to OpBitmap.Width - 1 do
      begin
      OpBitmap.Pixels[X, Y] := Int.TColors[X, Y];
      if (not haveMaskPix) then
       if (Int.Masked[x,y]) then
       begin
        haveMaskPix:=true;
        mx:=x; my:=y;
        writeln(mx,my);
       end;
      end;

  if Bitmap.Transparent and haveMaskPix then
    OpBitmap.TransparentColor :=OpBitmap.Pixels[mx,my] else
    OPBitmap.Transparent := false;
    
  int.free;
end;

function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;

function Pixel24ToFPColor(const a: Pixel24): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := fpImage.alphaOpaque;
end;

procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap);
var int: TLazIntfImage;
  x, y: integer;
  tempColor: TColor;
  tempFPColor: TFPColor;
begin
//Int:=Bitmap.CreateIntfImage;
  Int := TLazIntfImage.Create(0, 0);
  Int.DataDescription := GetDescriptionFromDevice(0);
  Int.SetSize(SourceBitmap.Width, SourceBitmap.Height);

  if SourceBitmap.Transparent then
  begin
    if Int.DataDescription.BitsPerPixel = 32 then //Mainly for Qt which does currently (rev 11907) not respect Mask but Alpha Channel.
    begin
      for y := 0 to SourceBitmap.Height - 1 do
        for x := 0 to SourceBitmap.Width - 1 do
        begin
          tempColor := SourceBitmap.Pixels[x, y];
          Int.TColors[x, y] := tempColor;
          if tempColor = SourceBitmap.TransparentColor then
          begin
            TempFPColor := Int.Colors[x, y];
            TempFPColor.alpha := FPImage.alphaTransparent;
            Int.Colors[x, y] := TempFPColor;
            Int.Masked[x, y] := true;
          end else
          begin
            TempFPColor := Int.Colors[x, y];
            TempFPColor.alpha := FPImage.alphaOpaque;
            Int.Colors[x, y] := TempFPColor;
            Int.Masked[x, y] := false;
          end;
        end;

    end else

      for y := 0 to SourceBitmap.Height - 1 do
        for x := 0 to SourceBitmap.Width - 1 do
        begin
          tempColor := SourceBitmap.Pixels[x, y];
          Int.TColors[x, y] := tempColor;
          Int.Masked[x, y] := tempColor = SourceBitmap.TransparentColor;
        end;
    Bitmap.Transparent := true;

  end else
  begin

    case SourceBitmap.Pixelformat of
      pf32bit:
        begin
          for y := 0 to SourceBitmap.Height - 1 do
            for x := 0 to SourceBitmap.Width - 1 do
              Int.Colors[x, y] := Pixel32ToFPColor(TBitmapData32(SourceBitmap.Data).NativePixels[x, y]);
        end;

      pf24bit:
        begin
          for y := 0 to SourceBitmap.Height - 1 do
            for x := 0 to SourceBitmap.Width - 1 do
              Int.Colors[x, y] := Pixel24ToFPColor(TBitmapData24(SourceBitmap.Data).NativePixels[x, y]);
        end;

    else
      begin
        for y := 0 to SourceBitmap.Height - 1 do
          for x := 0 to SourceBitmap.Width - 1 do
          begin
            Int.TColors[x, y] := SourceBitmap.Pixels[x, y];
          end;
      end;
    end;

    Bitmap.Transparent := false;
  end;
  Bitmap.LoadFromIntfImage(Int);
  Int.free;
end;

procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.create;
  AssignOpBitmapToBitmap(OpBitmap, Bmp);
  aCanvas.Draw(X, Y, bmp);
  Bmp.free;
end;


{$IFNDEF VER_MINIMAL}

{ TOPOpenDialog }

procedure TOPOpenDialog.UpdatePreview;
var
  CurFilename: string;
  FileIsValid: boolean;
  OP: TOPPicture;
  LBPP: Integer;
begin
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExists(FPreviewFilename)
    and (not DirPathExists(FPreviewFilename))
    and FileIsReadable(FPreviewFilename);
  if FileIsValid then
  try
    OP := TOPPicture.create;
    try
      OP.LoadFromFile(FPreviewFilename);
      LBPP := OP.Bitmap.BPP;
      OP.Bitmap.Transparent := false;
      AssignOpBitmapToBitmap(Op.Bitmap, ImageCtrl.Picture.Bitmap);
      PictureGroupBox.Caption := Format('(%dx%d BPP:%d)',
        [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height, LBPP]);
    finally
      OP.free;
    end;
  except
    FileIsValid := False;
  end;
  if not FileIsValid then
    ClearPreview;
end;

function TOPOpenDialog.Execute: boolean;
begin
  Filter := OPGLoadFilters;
  result := inherited Execute;
end;

{$ENDIF}


{$IFNDEF VER_MINIMAL}

{ TLazOPPicture }

constructor TLazOPPicture.Create(Image: TImage);
begin
  inherited Create;
  fImage := Image;
  fUpdateImageSize := true;
end;

procedure TLazOPPicture.DrawImage;
begin
  if fImage <> nil then
  begin
    if fUpdateImageSize then fImage.SetBounds(fImage.Left, fImage.Top, Bitmap.Width, Bitmap.Height);
    AssignOpBitmapToBitmap(Bitmap, fImage.Picture.Bitmap);
    fImage.invalidate;
  end;
end;

{$ENDIF}

end.
