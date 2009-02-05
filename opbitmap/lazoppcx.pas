unit LazOpPCX;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedPCXImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderPCX }

  TFPReaderPCX = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPPCXImage }

  TOPPCXImage = class(TFPImageBitmap)
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

{ TOPPCXImage }


class function TOPPCXImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPCX;
end;

class function TOPPCXImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPPCXImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedPCXImage;
end;



class function TOPPCXImage.GetFileExtensions: string;
begin
  Result:='pcx';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.pcx');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('pcx', mime.UserLangName, TOPPCXImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPPCXImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPPCXImage);
end;

{ TFPReaderPCX }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderPCX.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TPCXGraphic;
  x, y: integer;
begin
  op := TPCXGraphic.Create;
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



function TFPReaderPCX.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderPCX.Create;
begin
  inherited Create;
end;

destructor TFPReaderPCX.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
