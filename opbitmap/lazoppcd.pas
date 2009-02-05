unit LazOpPCD;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedPCDImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderPCD }

  TFPReaderPCD = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPPCDImage }

  TOPPCDImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
  end;


procedure Register;
procedure UnRegister;

implementation

{ TOPPCDImage }


class function TOPPCDImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPCD;
end;

class function TOPPCDImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPPCDImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedPCDImage;
end;


class function TOPPCDImage.GetFileExtensions: string;
begin
  Result:='pcd';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.pcd');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('pcd', mime.UserLangName, TOPPCDImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPPCDImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPPCDImage);
end;

{ TFPReaderPCD }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderPCD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TPCDGraphic;
  x, y: integer;
begin
  op := TPCDGraphic.Create;
  op.LoadFromStream(Stream);
  Img.SetSize(op.Width, op.Height);
  op.PixelFormat := opbitmap.pf32bit;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel32ToFPColor(TBitmapData32(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;



function TFPReaderPCD.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderPCD.Create;
begin
  inherited Create;
end;

destructor TFPReaderPCD.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
