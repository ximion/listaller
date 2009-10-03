//** Image formats OPBitmap can handle
unit opbitmapformats;

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

interface

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}


uses Classes, Types, SysUtils, opbitmap, bmp, bitmapimage, jpegdecoder, jpegencoder, Bmp2Tiff,
     gifdecoder, gif, pngdecoder, gifwrite, CommonPng, pngallstream,
  {$ifndef OpbCompat}GraphicEx,{$endif} XPMime;


type

  TSaveDetailEvent = procedure(Sender: TObject; Bitmap: TCanvasOpBitmap; var Continue: Boolean) of object;

  TOpBmpClass = class of TCanvasOPBitmap;


  TBMPImage = class(TCanvasOPBitmap)
  private
    fDecoder: TBmpDecoder;
    fEncoder: TBmpEncoder;
    fUseRLE: Boolean;
  protected

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property UseRLE: Boolean read fUseRLE write fUseRLE;
  end;



{ TGIFImage }

  TGIFImage = class(TCanvasOPBitmap)
  private
    fDecoder: TGifDecoder;
    fEncoder: TGif;
    fInterlaced: Boolean;
  protected

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property Interlaced: Boolean read fInterlaced write fInterlaced;
  end;

  { TJPEGImage }

  TJPEGImage = class(TCanvasOPBitmap)

  private
    fDecoder: TJpegDecoder;
    fEncoder: TJpegEncoder;
    fGrayscale: Boolean;
    fProgressive: Boolean;
    fComment: string;
    fQuality: integer;
  protected

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property Grayscale: Boolean read fGrayscale write fGrayscale;
    property Progressive: Boolean read fProgressive write fProgressive;
    property Comment: string read fComment write fComment;
    property Quality: integer read fQuality write fQuality;

  end;


{ TPNGImage }

  TPNGImage = class(TCanvasOPBitmap)
  private
    fDecoder: TPNGDecoder;
    fFilter: integer;
  protected

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
 //   procedure SaveToFile(const Filename: string); override;
    procedure SaveToStream(Stream: TStream); override;

  published
    property Filter: integer read fFilter write fFilter;

  end;

  { TTIFFImage }
 {$ifndef OpbCompat}
  TTIFFImage = class(TTIFFGraphic)
  public
    procedure SaveToStream(Stream: TStream); override;
  end;
  {$endif}

{ TOPGraphic }

  { TOPPicture }

  TOPPicture = class(TPersistent)
  private
    fCurrentFormat: TCanvasOPBitmap;
    fOnProgress: TOPProgressEvent;
    fCurrentSaveFormat: TCanvasOPBitmap;
    fExtension: string;
    fOnSaveDetails: TSaveDetailEvent;
    fLastMime: string;
    FMimeType: string;
    function GetClassFromMimeFile(FileName: string): TOpBmpClass;
    function GetClassFromMimeStream(Stream: TStream): TOpBmpClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream; Mime: string = '');
    procedure SaveToStream(Stream: TStream; Mime: string = '');
    property Bitmap: TCanvasOPBitmap read FCurrentFormat;
    property MimeType: string read FMimeType;
    property OnSaveDetails: TSaveDetailEvent read fOnSaveDetails write fOnSaveDetails;
    property OnProgress: TOPProgressEvent read fOnProgress write fOnProgress;
  end;

var OPGLoadFilters, OPGSaveFilters: WideString;

threadvar oldprogress: Cardinal;

implementation


procedure MianoProgressSinglePass(code: TBitmapImageCoder; adata: PROGRESSDATA;
  currentpass: Cardinal; passcount: Cardinal; description: string;
  progress: Cardinal; var cancel: Boolean);
var Rec: TRect;
begin
  if progress <> oldprogress then
  begin
    TOPBitmap(Adata).OPProgress(TOPBitmap(Adata), opsRunning, progress, true, Rec, description);
    oldprogress := progress;
  end;
end;

procedure MianoProgressMulitPass(code: TBitmapImageCoder; adata: PROGRESSDATA;
  currentpass: Cardinal; passcount: Cardinal; description: string;
  progress: Cardinal; var cancel: Boolean);
var Rec: TRect;
  progr: Cardinal;
begin
  if progress <> oldprogress then
  begin
    progr := Round((currentpass - 1) / passcount * 100 + (progress / passcount));
    TOPBitmap(Adata).OPProgress(TOPBitmap(Adata), opsRunning, progr, true, Rec, description);
    oldprogress := progress;
  end;
end;


procedure MianoProgressBmp(code: TBitmapImageCoder; adata: PROGRESSDATA;
  currentpass: Cardinal; passcount: Cardinal; description: string;
  progress: Cardinal; var cancel: Boolean);
var Rec: TRect;
  pgress: Cardinal;
begin
  if progress <> oldprogress then
  begin
    if progress < 100 then
    begin
      pgress := 100 - progress;
      TOPBitmap(Adata).OPProgress(TOPBitmap(Adata), opsRunning, pgress, true, Rec, description)
    end else
      TOPBitmap(Adata).OPProgress(TOPBitmap(Adata), opsEnding, 100, true, Rec, description);
    oldprogress := progress;
  end;
end;


function GetClassFromMime(MimeType: string): TOpBmpClass;
begin
  Result := nil;
  if MimeType = MIME_JPG then Result := TJPEGImage else
    if MimeType = MIME_PNG then Result := TPNGImage else
      if MimeType = MIME_GIF then Result := TGIFImage else
        if MimeType = MIME_BMP then Result := TBMPImage else
         if MimeType = MIME_OPB then Result := TCanvasOPBitmap {$ifndef OpbCompat} else
          if MimeType = MIME_TIF then Result := TTIFFImage else
            if MimeType = MIME_PCX then Result := TPCXGraphic else
              if MimeType = MIME_PCD then Result := TPCDGraphic else
                if MimeType = MIME_PBM then Result := TPPMGraphic else
                  if MimeType = MIME_PGM then Result := TPPMGraphic else
                    if MimeType = MIME_PPM then Result := TPPMGraphic else
                      if MimeType = MIME_PSD then Result := TPSDGraphic else
                        if MimeType = MIME_PSP then Result := TPSPGraphic else
                          if MimeType = MIME_CUT then Result := TCUTGraphic else
                            if MimeType = MIME_RGB then Result := TSGIGraphic else
                              if MimeType = MIME_SGI then Result := TSGIGraphic else
                                  if MimeType = MIME_TGA then Result := TTargaGraphic {$endif};
end;

{ TOPPicture }

function TOPPicture.GetClassFromMimeFile(FileName: string): TOpBmpClass;
var mo: TMimeObject;
begin
  Result := nil;
  mo := MimeMagic.GetMimeInfoFromPattern('*' + LowerCase(ExtractFileExt(FileName)));
  if mo <> nil then
  begin
    fLastMime := mo.MimeType;
    Result := GetClassFromMime(mo.MimeType);
  end;
end;

function TOPPicture.GetClassFromMimeStream(Stream: TStream): TOpBmpClass;
var mo: TMimeObject;
begin
  Result := nil;
  mo := MimeMagic.GetMagicMimeInfo(Stream);
  if mo <> nil then
  begin
    fLastMime := mo.MimeType;
    Result := GetClassFromMime(mo.MimeType);
  end;
end;

constructor TOPPicture.Create;
begin
  fCurrentFormat := TBmpImage.create;
end;

destructor TOPPicture.Destroy;
begin
  if fCurrentFormat <> nil then FreeAndNil(fCurrentFormat);
  if fCurrentSaveFormat <> nil then FreeAndNil(fCurrentSaveFormat);
  inherited Destroy;
end;

procedure TOPPicture.LoadFromFile(const FileName: string);
var cls: TOpBmpClass;
fs:TFileStream;
begin
  cls := GetClassFromMimeFile(FileName);
  //NEU
  if cls=nil then
  begin
   fs:=TFileStream.Create(FileName,fmOpenRead);
   cls:=GetClassFromMimeStream(fs);
   fs.free;
  end;
  //END NEU
  
  if cls <> nil then
  begin
    if fCurrentFormat <> nil then FreeAndNil(fCurrentFormat);
    fCurrentFormat := cls.create;
    if Assigned(fOnProgress) then fCurrentFormat.OnOPProgress := fOnProgress;
    (fCurrentFormat as cls).LoadFromFile(FileName);
    fMimeType := fLastMime;
  end else raise EPasBitMapError.Create('File Format not supported');
end;


procedure TOPPicture.SaveToFile(const FileName: string);
var cls: TOpBmpClass;
  Continue: Boolean;
begin
  cls := GetClassFromMimeFile(FileName);
  if cls <> nil then
  begin
    if not ((cls = TJPEGImage) or (cls = TBMPImage) or (cls = TGIFImage) or
    (cls = TCanvasOPBitmap) or (cls = TPNGImage) {$ifndef OpbCompat} or
    (cls = TTargaGraphic) or (cls = TTIFFImage){$endif}) then
      raise EPasBitMapError.Create('File Format not supported for saving') else
    begin
      if fCurrentSaveFormat <> nil then FreeAndNil(fCurrentSaveFormat);
      fCurrentSaveFormat := cls.create;
      if Assigned(fOnProgress) then fCurrentSaveFormat.OnOPProgress := fOnProgress;
      fCurrentSaveFormat.Assign(fCurrentFormat);

      Continue := True;

      if (cls = TJPEGImage) or (cls = TPNGImage) or (cls = TBMPImage) {$ifndef OpbCompat}or (cls = TTargaGraphic){$endif}
      or (cls = TGIFImage) then
      begin
        if Assigned(fOnSaveDetails) then OnSaveDetails(Self, fCurrentSaveFormat as cls, Continue);
      end;

      try
        if Continue then (fCurrentSaveFormat as cls).SaveToFile(FileName);
      finally
        if fCurrentSaveFormat <> nil then FreeAndNil(fCurrentSaveFormat);
      end;
    end;
  end else raise EPasBitMapError.Create('File Format not supported for saving');
end;

procedure TOPPicture.LoadFromStream(Stream: TStream; Mime: string);
var cls: TOpBmpClass;
begin
  if Mime = '' then
    cls := GetClassFromMimeStream(Stream) else
  begin
    cls := GetClassFromMime(Mime);
    fLastMime := Mime;
  end;
  if cls <> nil then
  begin
    if fCurrentFormat <> nil then FreeAndNil(fCurrentFormat);
    fCurrentFormat := cls.create;
    if Assigned(fOnProgress) then fCurrentFormat.OnOPProgress := fOnProgress;
    Stream.Position := 0;
    (fCurrentFormat as cls).LoadFromStream(Stream);
    fMimeType := fLastMime;
  end else raise EPasBitMapError.Create('Stream Format not supported');
end;

procedure TOPPicture.SaveToStream(Stream: TStream; Mime: string);
var cls: TOpBmpClass;
  Continue: Boolean;
begin
  cls := GetClassFromMime(Mime);
  if cls <> nil then
  begin
    if not ((cls = TJPEGImage) or (cls = TBMPImage) or
      (cls = TGIFImage) or (cls = TPNGImage) or (cls = TCanvasOPBitmap)
      {$ifndef OpbCompat}or (cls = TTIFFImage) or (cls = TTargaGraphic){$endif}) then
      raise EPasBitMapError.Create('Format not supported for saving to stream') else
    begin

      if fCurrentSaveFormat <> nil then FreeAndNil(fCurrentSaveFormat);
      fCurrentSaveFormat := cls.create;
      if Assigned(fOnProgress) then fCurrentSaveFormat.OnOPProgress := fOnProgress;
      fCurrentSaveFormat.Assign(fCurrentFormat);

      Continue := True;

      if (cls = TJPEGImage) or (cls = TBMPImage) {$ifndef OpbCompat}or (cls = TTargaGraphic){$endif} or (cls = TGIFImage) then
      begin
        if Assigned(fOnSaveDetails) then OnSaveDetails(Self, fCurrentSaveFormat as cls, Continue);
      end;

      try
        Stream.Position := 0;
        if Continue then (fCurrentSaveFormat as cls).SaveToStream(Stream);
      finally
        if fCurrentSaveFormat <> nil then FreeAndNil(fCurrentSaveFormat);
      end;
    end;
  end else raise EPasBitMapError.Create('Format not supported for saving to stream');
end;


{ TBMPImage }

constructor TBMPImage.Create;
begin
  inherited;
  fUseRLE := True;
end;

destructor TBMPImage.Destroy;
begin
  inherited;
end;

procedure TBMPImage.LoadFromStream(Stream: TStream);
begin
  Stream.Position := 0;
  Clear;
  fDecoder := TBmpDecoder.Create;
  fDecoder.SetProgressFunction(@MianoProgressBMP, Self);
  fDecoder.readImage(Stream, Self);
  fDecoder.free;
end;

procedure TBMPImage.SaveToStream(Stream: TStream);
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  Stream.Position := 0;
  fEncoder := TBmpEncoder.Create;
  if PixelFormat = pf15bit then
    fEncoder.BitsPerPixel := 15 else
    fEncoder.BitsPerPixel := BPP;
  if fUseRLE then
    fEncoder.RLECompress := (BPP = 4) or (BPP = 8) else
    fEncoder.RLECompress := false;
  fEncoder.WriteImage(Stream, Self);
  fEncoder.free;
end;


{ TJPEGImage }


constructor TJPEGImage.Create;
begin
  fProgressive := false;
  fGrayScale := false;
  fComment := '';
  fQuality := 70;
  inherited;
end;

destructor TJPEGImage.Destroy;
begin
  inherited;
end;


procedure TJPEGImage.LoadFromStream(Stream: TStream);
begin
  Clear;
  PixelFormat := pf24bit;
  fDecoder := TJPEGDecoder.Create;
  fDecoder.SetProgressFunction(@MianoProgressMulitPass, Self);
  fDecoder.readImageStream(Stream, Self);
  fProgressive := fDecoder.isProgressive;
  fComment := fDecoder.comment;
  fDecoder.free;
end;


procedure TJPEGImage.SaveToStream(Stream: TStream);
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  PixelFormat := pf24bit;
  fEncoder := TJPEGEncoder.Create;
  fEncoder.SetProgressFunction(@MianoProgressMulitPass, Self);
  if fGrayScale then fProgressive := false;
  if fQuality > 90 then fQuality := 90;
  FEncoder.Quality := fQuality div 10;
  FEncoder.Grayscale := fGrayScale;
  FEncoder.Comment := fComment;
  Fencoder.Progressive := fProgressive;
  fEncoder.writeImageStream(Stream, Self);
  fEncoder.free;
end;


{ TGIFImage }

constructor TGIFImage.Create;
begin
  inherited;
end;

destructor TGIFImage.Destroy;
begin
  inherited;
end;


procedure TGIFImage.LoadFromStream(Stream: TStream);
begin
  Clear;
  Transparent := false;
  fDecoder := TGifDecoder.Create;
  fDecoder.SetProgressFunction(@MianoProgressMulitPass, Self);
  fDecoder.readImageStream(Stream, Self);
  if TransparentColorFlag(fDecoder.graphicCtrlblock) then
  begin
    Self.TransparentColor := Self.ColorTable^[fDecoder.GraphicCtrlBlock.transparent_color];
  end;
  fDecoder.free;
end;

procedure TGIFImage.SaveToStream(Stream: TStream);
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  PixelFormat := pf8bit;
  Stream.Position := 0;
  fEncoder := TGif.Create;
  fEncoder.AddBitmap(self);
  if Transparent then
    fEncoder.TransparentColor := Transparentcolor;
  fEncoder.Interlaced := fInterlaced;
  fEncoder.SaveToStream(Stream);
  fEncoder.free;
end;


{ TPNGImage }

constructor TPNGImage.Create;
begin
  inherited Create;
  fFilter := 0;
end;

destructor TPNGImage.Destroy;
begin
  inherited Destroy;
end;

procedure TPNGImage.LoadFromStream(Stream: TStream);
begin
  Clear;
  fDecoder := TPNGDecoder.Create;
  fDecoder.SetProgressFunction(@MianoProgressSinglePass, Self);
  fDecoder.readImageStream(Stream, Self);
  if fDecoder.Transparent then
    TransparentColor := ColorTable^[fDecoder.TransparentIndex];
  fDecoder.free;
end;


procedure TPNGImage.SaveToStream(Stream: TStream);
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  PixelFormat := pf32bit;
  Stream.Position := 0;
  SaveToStreamPng(self, Stream, TFilter(fFilter), true);
end;

{ TTIFFImage }
{$ifndef OpbCompat}
procedure TTIFFImage.SaveToStream(Stream: TStream);
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  if (PixelFormat = pf1Bit) then PixelFormat := pf4bit;
  if (PixelFormat = pf15Bit) or (PixelFormat = pf16Bit) then PixelFormat := pf24bit;
  if (PixelFormat > pf32bit) then PixelFormat := pf32bit;
  Stream.Position := 0;
  WriteTiffToStream(Stream, Self);
end;
{$endif}

initialization
  OPGLoadFilters := MimeMagic.MakeLoadFilter;
  OPGSaveFilters := MimeMagic.MakeSaveFilter;

end.
