unit LazOpJPEG;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, Graphics, ClipBrd;

type

  TSharedJPEGImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderJPEG }

  TFPReaderJPEG = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterJPEG }

  TFPWriterJPEG = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  { TOPJPEGImage }

  TOPJPEGImage = class(TFPImageBitmap)
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

{ TOPJPEGImage }




class function TOPJPEGImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderJPEG;
end;

class function TOPJPEGImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterJPEG;
end;

class function TOPJPEGImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedJPEGImage;
end;



class function TOPJPEGImage.GetFileExtensions: string;
begin
  Result:='jpg;jpeg';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  cls:=GetGraphicClassForFileExtension('.jpg');
  if cls<>nil then TPicture.UnregisterGraphicClass(cls);
  mime:=MimeMagic.GetMimeInfoFromPattern('*.jpg');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('jpg', mime.UserLangName, TOPJPEGImage);
   TPicture.RegisterFileFormat('jpeg', mime.UserLangName, TOPJPEGImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPJPEGImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPJPEGImage);
end;

{ TFPReaderJPEG }


function Pixel24ToFPColor(const a: Pixel24): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := fpImage.alphaOpaque;
end;


procedure TFPReaderJPEG.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: opbitmapformats.TJPEGImage;
  x, y: integer;
begin
  op := opbitmapformats.TJPEGImage.Create;
  op.LoadFromStream(Stream);
  Img.SetSize(op.Width, op.Height);
  op.PixelFormat := opbitmap.pf24bit;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel24ToFPColor(TBitmapData24(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;



function TFPReaderJPEG.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderJPEG.Create;
begin
  inherited Create;
end;

destructor TFPReaderJPEG.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterJPEG }

procedure TFPWriterJPEG.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: opbitmapformats.TJPEGImage;
  x, y: integer;
begin
  op := opbitmapformats.TJPEGImage.Create;
  op.PixelFormat:=opbitmap.pf24bit;
  op.Width := Img.Width;
  op.Height := Img.Height;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      op.Pixels[x, y] := FPColorToTColor(Img.Colors[x, y]);
    end;
  op.SaveToStream(Stream);
  op.free;
end;

initialization
  Register;

finalization
  UnRegister;

end.
