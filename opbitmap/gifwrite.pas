unit gifwrite;

interface

uses
   Types, Classes, Sysutils, giftype, opbitmap;

{ ============================================================================

TGif.pas    copyright (C) 2000   R. Collins
rlcollins@ksaits.com

LEGAL STUFF:

This software is provided "as-is".  This software comes without warranty 
or garantee, explicit or implied.  Use this software at your own risk.  
The author will not be liable for any damage to equipment, data, or information
that may result while using this software.

By using this software, you agree to the conditions stated above.

This software may be used freely, provided the author's name and copyright
statement remain a part of the source code.

NOTE:  CompuServe, Inc. holds the patent to the compression algorithym
used in creating a GIF file.  Before you save a GIF file (using LZW
compression) that may be distributed for profit, make sure you understand
the full implications and legal ramifications of using the LZW compression.


2007 Theo: A lot of changes to make it an opbitmap reader. Strippped reading code.

============================================================================ }

{ ---------------------------------------------------------------------------- }

// define a GIF

type




    TGif = class(TPersistent)
    private
        fIOStream:              TMemoryStream;      // read or write the image
        fDataStream:            TMemoryStream;      // temp storage for LZW
        fExtension:             TList;              // latest extensions read/written

        fSignature:             PGifSignature;      // version of GIF
        fScreenDescriptor:      PGifScreenDescriptor;   // logical screen descriptor
        fImageDescriptorList:   TList;              // list of all images
        fColorTableList:        TList;              // list of all color tables
        fPaletteList:           TList;              // list of palettes from color tables
        fDraw:                  TGifDrawData;       // where to draw the image
        fZipData:               PGifZip;            // for encode/decode image
        fProgress:              TGifProgress;       // progress reports
        fLastBitmap:            TOpBitmap;          //theo: only for progress, maybe not the best way.

// functions that override TGraphic items

        procedure SetHeight(height: integer);
        function  GetHeight: integer;
        procedure SetWidth(width: integer);
        function  GetWidth: integer;
        function  GetEmpty: boolean;
        function  GetTransparent: boolean;
        procedure SetTransparent(t: boolean);


// write a GIF file

        procedure WriteSignature;
        procedure WriteScreenDescriptor;
        procedure WriteColorTable(Table: integer);
        procedure WriteExtension(eb: PGifExtension);
        procedure WriteDataBlockList(List: TList);
        procedure WriteImageDescriptor(id: PGifImageDescriptor);
        procedure WriteSourceInteger(size: integer; var value: integer);

// LZW encode and decode

        procedure LZWEncode(pID: PGifImageDescriptor);
        procedure LZWInit(pID: PGifImageDescriptor);
        procedure LZWFinit;
        procedure LZWReset;
        function  LZWGetCode: integer;
        procedure LZWSaveCode(Code: integer);
        procedure LZWSaveSlot(Prefix, Suffix: integer);
        procedure LZWIncrPosition;
        procedure LZWCheckSlot;
        procedure LZWPutCode(code: integer);
        procedure LZWPutClear;
        function  LZWReadBitmap: integer;

// alternative to LZW compression

        procedure SimpleEncode(pID: PGifImageDescriptor);
    //    procedure SimpleDecode(pID: PGifImageDescriptor);

// procedures used to implement the PROPERTIES

        function  GetSignature: string;
        function  GetScreenDescriptor: PGifScreenDescriptor;
        function  GetImageCount: integer;
        function  GetImageDescriptor(image: integer): PGifImageDescriptor;
        function  GetColorTableCount: integer;
        function  GetColorTable(table: integer): PGifColorTable;
        function  GetInterlaced: boolean;
        procedure SetInterlaced(ilace: boolean);
        function  GetLZWSize: integer;
        procedure SetLZWSize(size: integer);
        function  GetColorIndex(image, x, y: integer): integer;
        procedure SetColorIndex(image, x, y, color: integer);
        function  GetColor(image, x, y: integer): TColor;
        procedure SetColor(image, x, y: integer; color: TColor);
        function  GetTransparentIndex(image: integer): integer;
        procedure SetTransparentIndex(image, color: integer);
        function  GetTransparentColor: TColor;
        procedure SetTransparentColor(color: TColor);

        function  GetImageWidth(image: integer): integer;
        function  GetImageHeight(image: integer): integer;
        function  GetImageDepth(image: integer): integer;


// generally usefull routines

        procedure FreeDataBlockList(var list: TList);
        procedure FreeExtensionList(var list: TList);
        function  FindGraphicExtension(image: integer): PGifExtensionGraphic;
        procedure MakeGraphicExtensions;
        procedure ProgressInit(maxdata: integer; title: string);
        procedure ProgressFinit;
        procedure ProgressUpdate(value: integer);
        function  FindColorIndex(c: TColor; ct: PGifColorTable): integer;

    public

        property ScreenDescriptor:                  PGifScreenDescriptor    read GetScreenDescriptor;
        property ImageDescriptor[Image: integer]:   PGifImageDescriptor     read GetImageDescriptor;
        property ColorTable[Table: integer]:        PGifColorTable          read GetColorTable;
        property ColorIndex[Image, X, Y: integer]:  integer                 read GetColorIndex          write SetColorIndex;
        property Color[Image, X, Y: integer]:       TColor                  read GetColor               write SetColor;
        property TransparentIndex[Image: integer]:  integer                 read GetTransparentIndex    write SetTransparentIndex;
        property ImageWidth[Image: integer]:        integer                 read GetImageWidth;
        property ImageHeight[Image: integer]:       integer                 read GetImageHeight;
        property ImageDepth[Image: integer]:        integer                 read GetImageDepth;

    protected
    public
        constructor Create;
        constructor CreateFromBitmap(ABitmap: TOPBitmap);
        procedure Free;
        procedure FreeImage;


        procedure SaveToStream(Destination: TStream);
        procedure AddBitmap(Abitmap: TOPBitmap);
        property Signature:                         string                  read GetSignature;
        property ImageCount:                        integer                 read GetImageCount;
        property ColorTableCount:                   integer                 read GetColorTableCount;
        property Height:                            integer                 read GetHeight              write SetHeight;
        property Width:                             integer                 read GetWidth               write SetWidth;
        property Interlaced:                        boolean                 read GetInterlaced          write SetInterlaced;
        property LZWSize:                           integer                 read GetLZWSize             write SetLZWSize;
        property Empty:                             boolean                 read GetEmpty;
        property Transparent:                       boolean                 read GetTransparent         write SetTransparent;
        property TransparentColor:                  TColor                  read GetTransparentColor    write SetTransparentColor;

    end;



{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }


implementation



{ ---------------------------------------------------------------------------- }
{ EXPORTED PROCEDURES  ------------------------------------------------------- }

{ ---------------------------------------------------------------------------- }

constructor TGif.Create;
begin
inherited Create;

// nothing defined yet

fIOStream            := nil;
fDataStream          := nil;
fExtension           := nil;
fSignature           := nil;
fScreenDescriptor    := nil;
fImageDescriptorList := nil;
fColorTableList      := nil;
fPaletteList         := nil;
fZipData             := nil;

// some things, though, will always be needed

new(fSignature);
if (fSignature = nil) then OutOfMemoryError;
fSignature^.rSignature := '------';

new(fScreenDescriptor);
if (fScreenDescriptor = nil) then OutOfMemoryError;
fillchar(fScreenDescriptor^, sizeof(TGifScreenDescriptor), 0);

fImageDescriptorList := TList.Create;
fColorTableList      := TList.Create;
fPaletteList         := TList.Create;

fillchar(fDraw, sizeof(TGifDrawData), 0);
end;


{ ---------------------------------------------------------------------------- }

constructor TGif.CreateFromBitmap(ABitmap: TOPBitmap);
begin
inherited Create;

// nothing defined yet

fIOStream            := nil;
fDataStream          := nil;
fExtension           := nil;
fSignature           := nil;
fScreenDescriptor    := nil;
fImageDescriptorList := nil;
fColorTableList      := nil;
fPaletteList         := nil;
fZipData             := nil;

// some things, though, will always be needed

new(fSignature);
if (fSignature = nil) then OutOfMemoryError;
fSignature^.rSignature := '------';

new(fScreenDescriptor);
if (fScreenDescriptor = nil) then OutOfMemoryError;
fillchar(fScreenDescriptor^, sizeof(TGifScreenDescriptor), 0);

fImageDescriptorList := TList.Create;
fColorTableList      := TList.Create;
fPaletteList         := TList.Create;

// when the user wants to draw the image ...

fillchar(fDraw, sizeof(TGifDrawData), 0);

// create the first image

AddBitmap(ABitmap);
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.Free;
begin

FreeImage;

dispose(fSignature);
dispose(fScreenDescriptor);

fImageDescriptorList.Free;
fColorTableList.Free;
fPaletteList.Free;


inherited Free;
end;

{ ---------------------------------------------------------------------------- }
{ release all memory used to store image data }

procedure TGif.FreeImage;
var
    i,n:    integer;
    ex:     PGifExtension;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
begin

// temp input/output stream

if (fIOStream <> nil) then fIOStream.Free;
fIOStream := nil;

// temp encoded data

if (fDataStream <> nil) then fDataStream.Free;
fDataStream:= nil;

// temp list of image extensions

if (fExtension <> nil) then FreeExtensionList(fExtension);
fExtension := nil;

// signature record stays, but is cleared

if (fSignature = nil) then new(fSignature);
fSignature^.rSignature := '------';

// ditto the screen descriptor

if (fScreenDescriptor = nil) then new(fScreenDescriptor);
fillchar(fScreenDescriptor^, sizeof(TGifScreenDescriptor), 0);

// delete all items from image list, but leave the list

if (fImageDescriptorList = nil) then fImageDescriptorList := TList.Create;
for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    if (id <> nil) then
        begin
        if (id^.rExtensionList <> nil) then FreeExtensionList(id^.rExtensionList);
        if (id^.rPixelList     <> nil) then freemem(id^.rPixelList);
        if (id^.rBitmap        <> nil) then id^.rBitmap.Free;

        dispose(id);
        end;
    end;
fImageDescriptorList.Clear;

// release color tables, but keep the list

if (fColorTableList = nil) then fColorTableList := TList.Create;
for i := 0 to (fColorTableList.Count - 1) do
    begin
    ct := fColorTableList.Items[i];
    if (ct <> nil) then dispose(ct);
    end;
fColorTableList.Clear;

// once again, keep the palette list object, but not the data

if (fPaletteList = nil) then fPaletteList := TList.Create;
fPaletteList.Clear;

// don't need the zip/unzip data block

if (fZipData <> nil) then dispose(fZipData);
fZipData := nil;
end;





{ ---------------------------------------------------------------------------- }
{ save the current GIF definition to a stream object }
{ at first, just write it to our memory stream fSOURCE }

procedure TGif.SaveToStream(Destination: TStream);
var
    i,n:    integer;
    j,k:    integer;
    id:     PGifImageDescriptor;
    eb:     PGifExtension;
begin

// no error yet

GIF_ErrorCode := 0;
GIF_ErrorString := '';

// init temp vars

fIOStream   := TMemoryStream.Create;
fDataStream := TMemoryStream.Create;
fExtension  := nil;

// write the GIF signature

WriteSignature;

// overall screen description

WriteScreenDescriptor;

// write data for each screen descriptor

for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];

// write out extensions for this image

    n := 0;
    if (id^.rExtensionList <> nil) then n := id^.rExtensionList.Count;
    for j := 0 to (n - 1) do
        begin
        eb := id^.rExtensionList.Items[j];
        fIOStream.Write(kGifExtensionSeparator, 1);
        WriteExtension(eb);
        end;

// write actual image data

    fIOStream.Write(kGifImageSeparator, 1);
    WriteImageDescriptor(id);
    end;    // write images

// done with writing

fIOStream.Write(kGifTerminator, 1);

// write to destination stream

Destination.CopyFrom(fIOStream, 0);

// done with temp data

fIOStream.Free;
fIOStream := nil;
end;


{ ---------------------------------------------------------------------------- }
{ create a new image record from a given bitmap }

procedure TGif.AddBitmap(Abitmap: TOPBitmap);
const
    mask1       = $00E0E0E0;
    mask2       = $00C0E0E0;
var
    i,n, ci:        integer;
    x,y:        integer;
    ok:         boolean;
    ix:         integer;
    cx:         integer;
    cc:         TColor;
    r,g,b:      byte;
    global:     boolean;
    id:         PGifImageDescriptor;
    ct:         PGifColorTable;
    gx:         PGifExtensionGraphic;
    pptr:       PChar;
begin
id := nil;
ct := nil;

// valid bitmap

if (Abitmap = nil) then GIF_Error(22);
fLastBitmap:=ABitmap;  //theo only for progress, maybe not the best way...

// make a descriptor record, color map for this image, and space for a pixel list

new(id);
if (id = nil) then OutOfMemoryError;
fillchar(id^, sizeof(TGifImageDescriptor), 0);
ix := fImageDescriptorList.Add(id);
id^.rIndex := ix;

with id^ do
    begin
    rLeft := 0;
    rTop := 0;
    rWidth := Abitmap.Width;
    rHeight := Abitmap.Height;
    rInterlaced := false;
    rSorted := false;
    rLocalColorValid := false;
    rLocalColorSize := 0;
    rLocalColorTable := 0;
    rLZWSize := 8;
    rExtensionList := nil;
    rPixelList := nil;
    rPixelCount := 0;

// make empty pixel list

    rPixelCount := rWidth * rHeight;
    rPixelList := allocmem(rPixelCount);
    if (rPixelList = nil) then OutOfMemoryError;

// and the color table
// the first call attempts to use all colors in the bitmap
// if too many colors, the 2nd call uses only most significat 8 bits of color

    new(ct);
    if (ct = nil) then OutOfMemoryError;
    fillchar(ct^, sizeof(TGifColorTable), 0);


    ct^.rSize:=ABitmap.ColorTableSize;
    for ci:=0 to ct^.rSize-1 do
    ct^.rColors[ci]:=ABitmap.ColorTable^[ci];

    for ci:=0 to rPixelCount-1 do
    begin
    pptr := rPixelList + ci;
    pptr^ := Chr(TBitmapData8(Abitmap.Data).RawArray^[ci]);
    end;

// color count must be a power of 2

    if      (ct^.rSize <=   2) then ct^.rSize :=   2
    else if (ct^.rSize <=   4) then ct^.rSize :=   4
    else if (ct^.rSize <=   8) then ct^.rSize :=   8
    else if (ct^.rSize <=  16) then ct^.rSize :=  16
    else if (ct^.rSize <=  32) then ct^.rSize :=  32
    else if (ct^.rSize <=  64) then ct^.rSize :=  64
    else if (ct^.rSize <= 128) then ct^.rSize := 128
    else if (ct^.rSize <= 256) then ct^.rSize := 256
    else                            ct^.rSize := 256;

// add this color table to master list

    if (fColorTableList = nil) then fColorTableList := TList.Create;
    if (fPaletteList    = nil) then fPaletteList    := TList.Create;
    cx := fColorTableList.Add(ct);


// if no other global color table, use this one

    global := false;
    if (not fScreenDescriptor^.rGlobalColorValid) then
    with fScreenDescriptor^ do
        begin
        global := true;

        if      (ct^.rSize =   2) then rColorResolution := 1
        else if (ct^.rSize =   4) then rColorResolution := 2
        else if (ct^.rSize =   8) then rColorResolution := 3
        else if (ct^.rSize =  16) then rColorResolution := 4
        else if (ct^.rSize =  32) then rColorResolution := 5
        else if (ct^.rSize =  64) then rColorResolution := 6
        else if (ct^.rSize = 128) then rColorResolution := 7
        else if (ct^.rSize = 256) then rColorResolution := 8
        else                           rColorResolution := 8;

        rSorted           := false;
        rGlobalColorSize  := ct^.rSize;
        rBackGroundIndex  := 0;
        rGlobalColorTable := cx;
        rGlobalColorValid := true;
        end;    // with

// local color table info

    rLocalColorValid := (not global);
    rLocalColorSize := ct^.rSize;
    rLocalColorTable := cx;

// set transparency for this image

    MakeGraphicExtensions;
    gx := FindGraphicExtension(ix);

    if (Abitmap.Transparent) then
        begin
        cc := Abitmap.TransparentColor;
        n := FindColorIndex(cc, ct);
        if (n < 0) then n := FindColorIndex((cc and mask1), ct);
        if (n < 0) then n := FindColorIndex((cc and mask2), ct);
        if (n < 0) then n := 0;
        gx^.rTransparentIndex := n;
        end;
    end;    // with

// we just added an extension block; the signature must be version 89

fSignature^.rSignature := 'GIF89a';
end;



{ ---------------------------------------------------------------------------- }
{ PROCEDURE THAT OVERRIDE TGRAPHIC PROCEDURES -------------------------------- }

{ ---------------------------------------------------------------------------- }
{ this only gets/sets the size in the screen descriptor record }
{ this DOES NOT change the size of any individual image }

procedure TGif.SetHeight(height: integer);
begin
fScreenDescriptor^.rHeight := height;
end;

function TGif.GetHeight: integer;
begin
GetHeight := fScreenDescriptor^.rHeight;
end;

procedure TGif.SetWidth(width: integer);
begin
fScreenDescriptor^.rWidth := width;
end;

function TGif.GetWidth: integer;
begin
GetWidth := fScreenDescriptor^.rWidth;
end;

{ ---------------------------------------------------------------------------- }
{ a GIF is empty if no images are defined }

function TGif.GetEmpty: boolean;
var
    a:      boolean;
begin
a := true;
if ((fImageDescriptorList <> nil) and (fImageDescriptorList.Count > 0)) then a := false;
GetEmpty := a;
end;

{ ---------------------------------------------------------------------------- }
{ TRANSPARENT is assument to be the same for all images; i.e., if the first }
{ image is transparent, they they are all transparent }
{ if SetTransparent(TRUE) then set default color index for transparent color }
{ this can be changed with TransparentColor after this call }

function TGif.GetTransparent: boolean;
var
    b:      boolean;
    gx:     PGifExtensionGraphic;
begin
b := false;
gx := FindGraphicExtension(0);
if (gx <> nil) then b := gx^.rTransparentValid;

GetTransparent := b;
end;

procedure TGif.SetTransparent(t: boolean);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    gx:     PGifExtensionGraphic;
    p:      PChar;
begin
MakeGraphicExtensions;

for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    gx := FindGraphicExtension(i);
    if (gx <> nil) then gx^.rTransparentValid := t;
    if (t) then
        begin
        n := GetColorIndex(i, 0, (id^.rHeight-1));
        gx^.rTransparentIndex := n;
        end;
    end;
end;

{ ---------------------------------------------------------------------------- }
{ FUNCTIONS TO WRITE A GIF FILE ---------------------------------------------- }


{ ---------------------------------------------------------------------------- }
{ if only one image, and no image extensions, then GIF is GIF87a, else }
{ use the updated version GIF98a }

procedure TGif.WriteSignature;
var
    id:     PGifImageDescriptor;
begin
fSignature^.rSignature := 'GIF89a';
if (fImageDescriptorList.Count = 1) then
    begin
    id := fImageDescriptorList.Items[0];
    if (id^.rExtensionList = nil) then fSignature^.rSignature := 'GIF87a';
    end;

fIOStream.Write(fSignature^.rSignature, 6);
end;


{ ---------------------------------------------------------------------------- }
{ write the GIF logical screen descriptor to the source stream }

procedure TGif.WriteScreenDescriptor;
var
    i,n:    integer;
begin
with fScreenDescriptor^ do
    begin
    WriteSourceInteger(2, rWidth);                  // logical screen width
    WriteSourceInteger(2, rHeight);                 // logical screen height

    n := 0;                                         // packed bit fields
    if (rGlobalColorValid) then n := (n or $80);
    n := (n or (((rColorResolution - 1) and $07) shl 4));
    if (rSorted) then n := (n or $08);

    if      (rGlobalColorSize <=   2) then i := 0
    else if (rGlobalColorSize <=   4) then i := 1
    else if (rGlobalColorSize <=   8) then i := 2
    else if (rGlobalColorSize <=  16) then i := 3
    else if (rGlobalColorSize <=  32) then i := 4
    else if (rGlobalColorSize <=  64) then i := 5
    else if (rGlobalColorSize <= 128) then i := 6
    else if (rGlobalColorSize <= 256) then i := 7
    else                                   i := 7;

    n := (n or i);

    WriteSourceInteger(1, n);

    WriteSourceInteger(1, rBackgroundIndex);        // background color
    WriteSourceInteger(1, rAspectRatio);            // pixel aspect ratio

// write the global color table to the source stream

    if (rGlobalColorValid) then  WriteColorTable(rGlobalColorTable);
    end;
end;


{ ---------------------------------------------------------------------------- }
{ write out any color table, specified by it's index in the main list }
{ the size of the color table is adjusted to the next size of a power of 2 }

procedure TGif.WriteColorTable(Table: integer);
var
    i,n:        integer;
    r,g,b:      byte;
    c:          TColor;
    ct:         PGifColorTable;
begin
if ((Table < 0) or (Table >= fColorTableList.Count)) then GIF_Error(15);
ct := fColorTableList.Items[Table];

// for strange-sized tables, go to the next power of 2

n := ct^.rSize;
if      (n <=   2) then n :=   2
else if (n <=   4) then n :=   4
else if (n <=   8) then n :=   8
else if (n <=  16) then n :=  16
else if (n <=  32) then n :=  32
else if (n <=  64) then n :=  64
else if (n <= 128) then n := 128
else if (n <= 256) then n := 256
else                    n := 256;

// write the size

//WriteSourceInteger(1, n);

// write RGB values

for i := 0 to (n-1) do
    begin
    c := ct^.rColors[i];

    r := (c and $ff);
    g := ((c shr 8) and $ff);
    b := ((c shr 16) and $ff);

    fIOStream.Write(r, 1);    //theo swapped
    fIOStream.Write(g, 1);
    fIOStream.Write(b, 1);
    end;
end;


{ ---------------------------------------------------------------------------- }
{ write out any type of extension record }

procedure TGif.WriteExtension(eb: PGifExtension);
var
    i,n:    integer;
    w:      word;
    b:      byte;
begin

// write the extension label

b := eb^.rLabel;
fIOStream.Write(b, 1);

// "with eb^" gives us access to rGraphic, rText, rComment, and rApp

with eb^ do
    begin

// a graphic extension

    if (rLabel = kGifLabelGraphic) then
        begin
        WriteSourceInteger(1, rGraphic.rBlockSize);     // block size (always 4)

        n := 0;                                         // packed bit field
        n := (n or ((rGraphic.rDisposal and $07) shl 2));
        if (rGraphic.rUserInputValid)   then n := (n or $02);
        if (rGraphic.rTransparentValid) then n := (n or $01);
        WriteSourceInteger(1, n);

        WriteSourceInteger(2, rGraphic.rDelayTime);     // delay time
        WriteSourceInteger(1, rGraphic.rTransparentIndex);   // transparent color
        n := 0;
        WriteSourceInteger(1, n);                       // block terminator
        end

// a comment extension

    else if (rLabel = kGifLabelComment) then
        begin
        WriteDataBlockList(rComment.rDataBlockList);
        end

// a plain text extension

    else if (rLabel = kGifLabelText) then
        begin
        WriteSourceInteger(1, rText.rBlockSize);        // block size (always 12)
        WriteSourceInteger(2, rText.rGridLeft);         // grid position
        WriteSourceInteger(2, rText.rGridTop);          // grid position
        WriteSourceInteger(2, rText.rGridWidth);        // grid size
        WriteSourceInteger(2, rText.rGridHeight);       // grid size
        WriteSourceInteger(2, rText.rCellWidth);        // character cell size
        WriteSourceInteger(2, rText.rCellHeight);       // character cell size
        WriteSourceInteger(2, rText.rForegroundIndex);  // foreground color
        WriteSourceInteger(2, rText.rBackgroundIndex);  // background color
        WriteDataBlockList(rText.rDataBlockList);       // the text data
        end

// an application extension

    else if (rLabel = kGifLabelApplication) then
        begin
        WriteSourceInteger(1, rApp.rBlockSize);         // block size (always 11)
        fIOStream.Write(rApp.rIdentifier, 8);           // application identifier
        fIOStream.Write(rApp.rAuthentication, 3);       // authentication code
        WriteDataBlockList(rApp.rDataBlockList);        // misc data
        end

// unknown type

    else
        begin
        GIF_ErrorMessage('unknown extension: ' + IntToHex(rLabel, 4));
        end;
    end; // with eb^
end;

{ ---------------------------------------------------------------------------- }
{ read in a group of data blocks until a zero-length block is found }
{ store the data on the give TList }

procedure TGif.WriteDataBlockList(List: TList);
var
    i,n:    integer;
    b:      byte;
    db:     PGifDataBlock;
begin

// write out the blocks that actually contain some data

for i := 0 to (List.Count - 1) do
    begin
    db := List.Items[i];
    b  := db^.rSize;
    if (b > 0) then
        begin
        fIOStream.Write(b, 1);
        fIOStream.Write(db^.rData, b);
        end;
    end;

// then write an end-of-block

b := 0;
fIOStream.Write(b, 1);
end;


{ ---------------------------------------------------------------------------- }
{ write the next image descriptor }

procedure TGif.WriteImageDescriptor(id: PGifImageDescriptor);
var
    i,n:    integer;
    ix:     integer;
    eb:     PGifExtension;
    db:     TGifDataBlock;
begin

// init the sotrage for compressed data

fDataStream.Clear;

// shortcut to the record fields

with id^ do
    begin


// write the basic descriptor record

    WriteSourceInteger(2, rLeft);           // left position
    WriteSourceInteger(2, rTop);            // top position
    WriteSourceInteger(2, rWidth);          // size of image
    WriteSourceInteger(2, rHeight);         // size of image

    n := 0;                                 // packed bit field
    if      (rLocalColorSize <=   2) then i := 0
    else if (rLocalColorSize <=   4) then i := 1
    else if (rLocalColorSize <=   8) then i := 2
    else if (rLocalColorSize <=  16) then i := 3
    else if (rLocalColorSize <=  32) then i := 4
    else if (rLocalColorSize <=  64) then i := 5
    else if (rLocalColorSize <= 128) then i := 6
    else if (rLocalColorSize <= 256) then i := 7
    else                                  i := 7;

    n := (n or i);
    if (rLocalColorValid) then n := (n or $80);
    if (rInterlaced)      then n := (n or $40);
    if (rSorted)          then n := (n or $20);

    WriteSourceInteger(1, n);

// if a local color table is defined, write it

    if (rLocalColorValid) then WriteColorTable(rLocalColorTable);

// the LZW minimum code size

    WriteSourceInteger(1, rLZWSize);

// encode the image and save it in DATASTREAM

    if (rLZWSize = 0) then SimpleEncode(id)
    else                   LZWEncode(id);

// write out the data stream as a series of data blocks

    fDataStream.Position := 0;
    while (fDataStream.Position < fDataStream.Size) do
        begin
        n := fDataStream.Size - fDataStream.Position;
     //   writeln(n);
        if (n > 255) then n := 255;
        db.rSize := n;
        fDataStream.Read(db.rData, n);
        fIOStream.Write(db.rSize, 1);
        fIOStream.Write(db.rData, n);
        end;

// block terminator

    n := 0;
    WriteSourceInteger(1, n);
    end; // with id^
end;


{ ---------------------------------------------------------------------------- }
{ write a 1 or 2-byte integer to the source stream }

procedure TGif.WriteSourceInteger(size: integer; var value: integer);
var
    b:  byte;
    w:  word;
begin
if (size = 1) then
    begin
    b := value;
    fIOStream.Write(b, 1);
    end
else if (size = 2) then
    begin
    w := value;
    fIOStream.Write(w, 2);
    end
else
    begin
    GIF_Error(8);
    end;
end;



{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }
{ these are the LZW encode/decode routines .. i.e., ZIP and UNZIP }


{ ---------------------------------------------------------------------------- }
{ sqrunch a bitmap into a memory stream }

procedure TGif.LZWEncode(pID: PGifImageDescriptor);
var
    i,n:            integer;
    j,k:            integer;
    cc:             integer;        // current code to translate
    oc:             integer;        // last code encoded
    found:          boolean;        // decoded string in prefix table?
    pixel:          byte;           // lowest code to search for
    ldx:            integer;        // last index found
    fdx:            integer;        // current index found
begin

// allocate stack space

LZWInit(pID);
LZWReset;

// all within the data record

with fZipData^ do
    begin

// start progress bar

    ProgressInit(rID^.rPixelCount, 'LZW encode image');

// reset output data stream

    fDataStream.Clear;

// always save the clear code first ...

    LZWPutCode(rClearCode);

// and first pixel

    oc := LZWReadBitmap;
    LZWPutCode(oc);

// nothing found yet (but then, we haven't searched)

    ldx := 0;
    fdx := 0;

// and the rest of the pixels

    rCount := 1;
    while (rCount <= rID^.rPixelCount) do
        begin
        ProgressUpdate(rCount);

// empty the stack of old data

        rSP := 0;

// next pixel from the bitmap

        n := LZWReadBitmap;
        LZWSaveCode(n);
        cc := rCodeStack[0];        // beginning of the string

// add new encode table entry

        rPrefix[rNextSlot] := oc;
        rSuffix[rNextSlot] := cc;
        rNextSlot := rNextSlot + 1;
        if      (rNextSlot >= kGifCodeTableSize) then rMaxVal := true
        else if (rNextSlot > (1 shl rCurSize))   then rCurSize := rCurSize + 1;

// find the running string of matching codes

        ldx := cc;
        found := true;
        while ((found) and (rCount <= rID^.rPixelCount)) do
            begin
            n := LZWReadBitmap;
            LZWSaveCode(n);
            cc := rCodeStack[0];

            if (ldx < rFirstSlot) then i := rFirstSlot
            else                       i := ldx + 1;
            pixel := rCodeStack[rSP - 1];
            found := false;
            while ((not found) and (i < rNextSlot)) do
                begin
                found := ((rPrefix[i] = ldx) and (rSuffix[i] = pixel));
                i := i + 1;
                end;
            if (found) then
                begin
                ldx := i - 1;
                fdx := i - 1;
                end;
            end; // while found

// if not found, save this index, and get the same code again

        if (not found) then
            begin
            rUnget := true;
            rLast := rCodeStack[rSP-1];
            rSP := rSP - 1;
            cc := ldx;
            end
        else
            begin
            cc := fdx;
            end;

// whatever we got, write it out as current table entry

        LZWPutCode(cc);

        if ((rMaxVal) and (rCount <= rID^.rPixelCount)) then
            begin
            LZWPutCode(rClearCode);
            LZWReset;

            cc := LZWReadBitmap;
            LZWPutCode(cc);
            oc := cc;
            end
        else
            begin
            oc := cc;
            end;
        end; // while pixelcount

    LZWPutCode(rEndCode);
    LZWPutClear;

// finish the progress bar

    ProgressFinit;
    end;    // with

// done with stack space

LZWFinit;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWInit(pID: PGifImageDescriptor);
var
    i,n:    integer;
begin

// get a valid record?

if (pID = nil) then GIF_Error(11);

// make sure we can actually decode this turkey

// if ((pID^.rLZWSize < 2) or (pID^.rLZWSize > 9)) then GIF_Error(12);

// allocate stack space

new(fZipData);
if (fZipData = nil) then OutOfMemoryError;

// init data block

fillchar(fZipData^, sizeof(TGifZip), 0);
fZipData^.rID := pID;
fZipData^.rCT := fColorTableList.Items[pID^.rLocalColorTable];

// reset data stream

fDataStream.Position := 0;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWFinit;
begin
if (fZipData <> nil) then dispose(fZipData);
fZipData := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWReset;
var
    i,n:    integer;
begin
with fZipData^ do
    begin
    for i := 0 to (kGifCodeTableSize - 1) do
        begin
        rPrefix[i] := 0;
        rSuffix[i] := 0;
        end;

    rCurSize   := rID^.rLZWSize + 1;
    rClearCode := (1 shl rID^.rLZWSize);
    rEndCode   := rClearCode + 1;
    rHighCode  := rClearCode - 1;
    rFirstSlot := (1 shl (rCurSize - 1)) + 2;
    rNextSlot  := rFirstSlot;
    rMaxVal    := false;
    end;    // with
end;


{ ---------------------------------------------------------------------------- }
{ get the next code from the BitString }
{ CurrentSize specifies the number of bits to get }

function TGif.LZWGetCode: integer;
var
    i,n:    integer;
    cc:     integer;
    mask:   integer;
    b:      byte;
begin
with fZipData^ do
    begin

// make sure we have enough bits

    while (rCurSize > rBits) do
        begin
        if (fDataStream.Position >= fDataStream.Size) then b := 0
        else                                fDataStream.Read(b, 1);
        n := b;
        n := (n shl rBits);                 // scoot bits over to avoid previous data
        rBitString := (rBitString or n);    // put bits in the BitString
        rBits := rBits + 8;                 // number of bits in a byte
        end;


// get the code, then dump the bits we used from the BitString

    case rCurSize of
         0:     mask := 0;
         1:     mask := $0001;
         2:     mask := $0003;
         3:     mask := $0007;
         4:     mask := $000f;
         5:     mask := $001f;
         6:     mask := $003f;
         7:     mask := $007f;
         8:     mask := $00ff;
         9:     mask := $01ff;
        10:     mask := $03ff;
        11:     mask := $07ff;
        12:     mask := $0fff;
        else    GIF_Error(12);
        end;

    cc := (rBitString and mask);                // mask off bits wanted
    rBitString := (rBitString shr rCurSize);    // delete bits we just took
    rBits  := rBits - rCurSize;                 // number of bits left in BitString
    end;    // with

// done

LZWGetCode := cc;
end;

{ ---------------------------------------------------------------------------- }
{ save a code value on the code stack }

procedure TGif.LZWSaveCode(Code: integer);
begin
with fZipData^ do
    begin
    rCodeStack[rSP] := Code;
    rSP := rSP + 1;
    end;
end;


{ ---------------------------------------------------------------------------- }
{ save a new prefix/suffix pair }

procedure TGif.LZWSaveSlot(Prefix, Suffix: integer);
begin
with fZipData^ do
    begin
    rPrefix[rCurSlot] := Prefix;
    rSuffix[rCurSlot] := Suffix;
    rCurSlot := rCurSlot + 1;
    end;
end;



{ ---------------------------------------------------------------------------- }
{ given current line number, compute the next line to be filled }
{ this gets a little tricky if an interlaced image }
{ what is the purpose of this interlace, anyway?  it doesn't save space, }
{ and I can't imagine it makes for any faster image disply or loading }

procedure TGif.LZWIncrPosition;
var
    i,n:    integer;
begin
with fZipData^ do
    begin

// if first pass, make sure CurPass was initialized

    if (rCurPass = 0) then rCurPass := 1;

// incr X position

    rCurX := rCurX + 1;

// bumping Y ?

    if (rCurX >= rID^.rWidth) then
        begin
        rCurX := 0;

// if not interlaced image, then just move down the page

        if (not  rID^.rInterlaced) then
            begin
            rCurY := rCurY + 1;
            end

// interlaced images select the next line by some archane black-magical sheme

        else
            begin
            case rCurPass of                // delta to next row on this pass
                1:      n := 8;
                2:      n := 8;
                3:      n := 4;
                4:      n := 2;
                else    GIF_Error(21);
                end;

            rCurY := rCurY + n;

// if past the end of the bitmap, start next pass

            if (rCurY >= rID^.rHeight) then
                begin
                rCurPass := rCurPass + 1;
                if (rCurPass = 5) then rCurPass := 1;
                case rCurPass of            // first line for given pass
                    1:      n := 0;
                    2:      n := 4;
                    3:      n := 2;
                    4:      n := 1;
                    else    GIF_Error(21);
                    end;

                rCurY := n;
                end;
            end;
        end;
    end;    // with
end;

{ ---------------------------------------------------------------------------- }
{ see if it is time to add a new slot to the decoder tables }

procedure TGif.LZWCheckSlot;
begin
with fZipData^ do
    begin
    if (rCurSlot >= rTopSlot) then
        begin
        if (rCurSize < 12) then
            begin
            rTopSlot := (rTopSlot shl 1);
            rCurSize := rCurSize + 1;
            end
        else
            begin
            rMaxVal := true;
            end;
        end;
    end;
end;


{ ---------------------------------------------------------------------------- }
{ save the code in the output data stream }

procedure TGif.LZWPutCode(code: integer);
var
    i,n:    integer;
    b:      byte;
begin
with fZipData^ do
    begin

// write out finished bytes
// a literal "8" for 8 bits per byte

    while (rBits >= 8) do
        begin
        b := (rBitString and $ff);
        rBitString := (rBitString shr 8);
        rBits := rBits - 8;

        fDataStream.Write(b, 1);
        end;

// make sure no junk bits left above the first byte

    rBitString := (rBitString and $ff);

// and save out-going code

    n := (code shl rBits);
    rBitString := (rBitString or n);
    rBits := rBits + rCurSize;
    end;    // with
end;

{ ---------------------------------------------------------------------------- }
{ write out the rest of the bit string }

procedure TGif.LZWPutClear;
var
    i,n:    integer;
    b:      byte;
begin
with fZipData^ do
    begin
    while (rBits > 0) do
        begin
        b := (rBitString and $ff);
        rBitString := (rBitString shr 8);
        rBits := rBits - 8;

        fDataStream.Write(b, 1);
        end;
    end;
end;

{ ---------------------------------------------------------------------------- }
{ get the next pixel from the bitmap, and return it as an index into }
{ the colormap }

function TGif.LZWReadBitmap: integer;
var
    i,n:    integer;
    j:      longint;
    p:      PChar;
begin
with fZipData^ do
    begin
    if (rUnget) then
        begin
        n := rLast;
        rUnget := false;
        end
    else
        begin
        rCount := rCount + 1;
        j := (rCurY * rID^.rWidth) + rCurX;
        if ((0 <= j) and (j < rID^.rPixelCount)) then
            begin
            p := rID^.rPixelList + j;
            n := ord(p^);
            end
        else
            begin
            n := 0;
            end;

        LZWIncrPosition;
        end;

    rLast := n;
    end;    // with

LZWReadBitmap := n;
end;


{ ---------------------------------------------------------------------------- }
{ STORE GIF WITHOUT LZW COMPRESSION ------------------------------------------ }

{ ===
CompuServe, Inc. has copyrighted the LZW compression algorithym used in GIF
images.  However, you can store GIF images without the LZW compression and
do not need to pay royalties to CompuServe

The LZW minimum code size supported by LZW is 2..9 bits; if you se the
LZW size to 0 bits, then the following simple encode and decode routines
will be used instead of the LZW encode/decode.  Although you can save the
resultant data to a file named something.GIF, it is NOT a real GIF file; it
only has the structure of a GIF file.  The something.GIF can be read back
in and used by this module, but not necessarily by any other software that
reads GIF files.

Before you save files using GIF compression, be sure you research and
understand the full implications of the CompuServe copyright.
=== }

{ ---------------------------------------------------------------------------- }
{ skip LZW encode/decode, just put bits into output stream }

procedure TGif.SimpleEncode(pID: PGifImageDescriptor);
var
    i,n:    integer;
    r,k:    integer;
    b:      byte;
    p:      PChar;
begin

// make a data block

LZWInit(pID);

// use zip data block

with fZipData^ do
    begin

// no data to write yet

    rBitString := 0;
    rBits := 0;

// nothing saved

    fDataStream.Clear;

// make no mistake, this is NOT an interlaced image

    rID^.rInterlaced := false;

// determine color resolution
// this may not be the same as ColorResolution in ScreenDescriptor

    if      (rCT^.rSize <=   2) then r := 1
    else if (rCT^.rSize <=   4) then r := 2
    else if (rCT^.rSize <=   8) then r := 3
    else if (rCT^.rSize <=  16) then r := 4
    else if (rCT^.rSize <=  32) then r := 5
    else if (rCT^.rSize <=  64) then r := 6
    else if (rCT^.rSize <= 128) then r := 7
    else if (rCT^.rSize <= 256) then r := 8
    else                             r := 8;

// save color resolution as first byte of output stream

    rBitString := r;
    rBits := 8;

// start progress bar

    ProgressInit(rID^.rPixelCount, 'simple encode image');

// save each pixel

    for i := 0 to (rID^.rPixelCount - 1) do
        begin
        ProgressUpdate(i);

// get pixel value

        p := rID^.rPixelList + i;
        n := ord(p^);

// only the bits we need to keep

        if      (r = 1) then n := (n and $01)
        else if (r = 2) then n := (n and $03)
        else if (r = 3) then n := (n and $07)
        else if (r = 4) then n := (n and $0f)
        else if (r = 5) then n := (n and $1f)
        else if (r = 6) then n := (n and $3f)
        else if (r = 7) then n := (n and $7f)
        else if (r = 8) then n := (n and $ff);

// store in BitString

        n := (n shl rBits);
        rBitString := (rBitString or n);
        rBits := rBits + r;

// write out full bytes

        while (rBits >= 8) do
            begin
            b := (rBitString and $ff);
            rBitString := (rBitString shr 8);
            rBits := rBits - 8;

            fDataStream.Write(b, 1);
            end;
        end;    // for

// last of the BitString

    while (rBits > 0) do
        begin
        b := (rBitString and $ff);
        rBitString := (rBitString shr 8);
        rBits := rBits - 8;

        fDataStream.Write(b, 1);
        end;
    end;

// done with progres

ProgressFinit;

// done

LZWFinit;
end;


{ ---------------------------------------------------------------------------- }
{ PROCEDURES TO IMPLEMENT PROPERTIES ----------------------------------------- }

{ ---------------------------------------------------------------------------- }

function TGif.GetSignature: string;
var
    i,n:    integer;
    s:      string;
begin
s := fSignature^.rSignature;
GetSignature := s;
end;


{ ---------------------------------------------------------------------------- }
{ return screen descriptor data pointer, or set a new record block }

function TGif.GetScreenDescriptor: PGifScreenDescriptor;
begin
GetScreenDescriptor := fScreenDescriptor;
end;


{ ---------------------------------------------------------------------------- }

function TGif.GetImageCount: integer;
begin
GetImageCount := fImageDescriptorList.Count;
end;


function TGif.GetImageDescriptor(image: integer): PGifImageDescriptor;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
GetImageDescriptor := fImageDescriptorList.Items[image];
end;


{ ---------------------------------------------------------------------------- }

{
function TGif.GetBitmap(image: integer): TOPBitmap;
var
    p:      PGifImageDescriptor;
    b:      TOPBitmap;
begin
p := GetImageDescriptor(image);
if (p^.rBitmap = nil) then MakeBitmaps;
b := p^.rBitmap;

GetBitmap := b;
end;
}



{ ---------------------------------------------------------------------------- }

function TGif.GetColorTableCount: integer;
begin
GetColorTableCount := fColorTableList.Count;
end;


function TGif.GetColorTable(table: integer): PGifColorTable;
begin
if ((table < 0) or (table >= fColorTableList.Count)) then GIF_Error(15);
GetColorTable := fColorTableList.Items[table];
end;



{ ---------------------------------------------------------------------------- }

function TGif.GetInterlaced: boolean;
var
    id:     PGifImageDescriptor;
begin
id := GetImageDescriptor(0);
GetInterlaced := id^.rInterlaced;
end;

procedure TGif.SetInterlaced(ilace: boolean);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
begin
for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    id^.rInterlaced := ilace;
    end;
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetLZWSize: integer;
var
    id:     PGifImageDescriptor;
begin
id := GetImageDescriptor(0);
GetLZWSize := id^.rLZWSize;
end;

procedure TGif.SetLZWSize(size: integer);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
begin
//if ((size < 2) or (size > 12)) then GIF_Error(15);
for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    id^.rLZWSize := size;
    end;
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetColorIndex(image, x, y: integer): integer;
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    p:      PChar;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
id := fImageDescriptorList.Items[image];
if ((x < 0) or (x >= id^.rWidth))  then GIF_Error(15);
if ((y < 0) or (y >= id^.rHeight)) then GIF_Error(15);

n := (y * id^.rWidth) + x;
p := id^.rPixelList + n;
i := ord(p^);

GetColorIndex := i;
end;

procedure TGif.SetColorIndex(image, x, y, color: integer);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    p:      PChar;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
id := fImageDescriptorList.Items[image];
ct := fColorTableList.Items[id^.rLocalColorTable];
if ((x < 0) or (x >= id^.rWidth))  then GIF_Error(15);
if ((y < 0) or (y >= id^.rHeight)) then GIF_Error(15);
if ((color < 0) or (color >= ct^.rSize)) then GIF_Error(15);

n := (y * id^.rWidth) + x;
p := id^.rPixelList + n;
p^ := chr(color);

id^.rBitmap.Pixels[x,y] := ct^.rColors[color];
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetColor(image, x, y: integer): TColor;
var
    i,n:    integer;
    acolor:  TColor;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    p:      PChar;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
id := fImageDescriptorList.Items[image];
ct := fColorTableList.Items[id^.rLocalColorTable];
if ((x < 0) or (x >= id^.rWidth))  then GIF_Error(15);
if ((y < 0) or (y >= id^.rHeight)) then GIF_Error(15);

n := (y * id^.rWidth) + x;
p := id^.rPixelList + n;
i := ord(p^);
acolor := ct^.rColors[i];

GetColor := acolor;
end;

procedure TGif.SetColor(image, x, y: integer; color: TColor);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    p:      PChar;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
id := fImageDescriptorList.Items[image];
ct := fColorTableList.Items[id^.rLocalColorTable];
if ((x < 0) or (x >= id^.rWidth))  then GIF_Error(15);
if ((y < 0) or (y >= id^.rHeight)) then GIF_Error(15);
if ((color < 0) or (color >= ct^.rSize)) then GIF_Error(15);

i := FindColorIndex(color, ct);
if (i < 0) then FindColorIndex((color and $00E0E0E0), ct);
if (i < 0) then FindColorIndex((color and $00C0E0E0), ct);
if (i < 0) then i := 0;

n := (y * id^.rWidth) + x;
p := id^.rPixelList + n;
p^ := chr(i);

id^.rBitmap.Pixels[x,y] := ct^.rColors[i];
end;

{ ---------------------------------------------------------------------------- }
{ transparent color for each individual image }

function TGif.GetTransparentIndex(image: integer): integer;
var
    i,n:    integer;
    gx:     PGifExtensionGraphic;
begin
i := 0;
gx := FindGraphicExtension(image);
if (gx <> nil) then i := gx^.rTransparentIndex;

GetTransparentIndex := i;
end;


procedure TGif.SetTransparentIndex(image, color: integer);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    gx:     PGifExtensionGraphic;
    p:      PChar;
begin
if ((image < 0) or (image >= fImageDescriptorList.Count)) then GIF_Error(15);
id := fImageDescriptorList.Items[image];
ct := fColorTableList.Items[id^.rLocalColorTable];
gx := FindGraphicExtension(image);
if ((color < 0) or (color >= ct^.rSize)) then GIF_Error(15);

if (gx = nil) then
    begin
    MakeGraphicExtensions;
    gx := FindGraphicExtension(image);
    end;

gx^.rTransparentValid := true;
gx^.rTransparentIndex := color;

if (id^.rBitmap <> nil) then
    begin
    id^.rBitmap.Transparent := true;
//theo    id^.rBitmap.TransparentMode := tmFixed;
    id^.rBitmap.TransparentColor := ct^.rColors[color];
    end;
end;


{ ---------------------------------------------------------------------------- }
{ transparent color for all images }

function TGif.GetTransparentColor: TColor;
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    gx:     PGifExtensionGraphic;
begin
if (fImageDescriptorList.Count <= 0) then GIF_Error(15);
id := fImageDescriptorList.Items[0];
ct := fColorTableList.Items[id^.rLocalColorTable];

i := 0;
gx := FindGraphicExtension(0);
if (gx <> nil) then i := gx^.rTransparentIndex;

GetTransparentColor := ct^.rColors[i];
end;

procedure TGif.SetTransparentColor(color: TColor);
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
    gx:     PGifExtensionGraphic;
    p:      PChar;
begin
for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    ct := fColorTableList.Items[id^.rLocalColorTable];
    gx := FindGraphicExtension(i);
    if (gx = nil) then
        begin
        MakeGraphicExtensions;
        gx := FindGraphicExtension(i);
        end;

    n := FindColorIndex(color, ct);
    if (n < 0) then n := FindColorIndex((color and $00E0E0E0), ct);
    if (n < 0) then n := FindColorIndex((color and $00C0E0E0), ct);
    if (n < 0) then n := 0;

    gx^.rTransparentValid := true;
    gx^.rTransparentIndex := n;

    if (id^.rBitmap <> nil) then
        begin
        id^.rBitmap.Transparent := true;
//theo  id^.rBitmap.TransparentMode := tmFixed;
        id^.rBitmap.TransparentColor := ct^.rColors[n];
        end;
    end;
end;


{ ---------------------------------------------------------------------------- }

function TGif.GetImageWidth(image: integer): integer;
var
    id:     PGifImageDescriptor;
begin
id := GetImageDescriptor(image);
GetImageWidth := id^.rWidth;
end;

function TGif.GetImageHeight(image: integer): integer;
var
    id:     PGifImageDescriptor;
begin
id := GetImageDescriptor(image);
GetImageHeight := id^.rHeight;
end;

function TGif.GetImageDepth(image: integer): integer;
var
    id:     PGifImageDescriptor;
    ct:     PGifColorTable;
begin
id := GetImageDescriptor(image);
ct := fColorTableList.Items[id^.rLocalColorTable];
GetImageDepth := ct^.rSize;
end;


{ ---------------------------------------------------------------------------- }
{ GENERAL INTERNAL ROUTINES -------------------------------------------------- }

{ ---------------------------------------------------------------------------- }

procedure TGif.FreeDataBlockList(var list: TList);
var
    i,n:    integer;
    db:     PGifDataBlock;
begin
if (list <> nil) then
    begin
    for i := 0 to (list.Count - 1) do
        begin
        db := list.Items[i];
        if (db <> nil) then dispose(db);
        end;

    list.Free;
    end;

list := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.FreeExtensionList(var list: TList);
var
    i,n:    integer;
    ex:     PGifExtension;
begin
if (list <> nil) then
    begin
    for i := 0 to (list.Count - 1) do
        begin
        ex := list.Items[i];
        if (ex <> nil) then
            begin
            if      (ex^.rLabel = kGifLabelComment)     then FreeDataBlockList(ex^.rComment.rDataBlockList)
            else if (ex^.rLabel = kGifLabelText)        then FreeDataBlockList(ex^.rText.rDataBlockList)
            else if (ex^.rLabel = kGifLabelApplication) then FreeDataBlockList(ex^.rApp.rDataBlockList);

            dispose(ex);
            end;
        end;

    list.Free;
    end;

list := nil;
end;


{ ---------------------------------------------------------------------------- }
{ find the graphic extension for an image }

function TGif.FindGraphicExtension(image: integer): PGifExtensionGraphic;
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    ex:     PGifExtension;
    gx:     PGifExtensionGraphic;
begin
gx := nil;
id := fImageDescriptorList.Items[image];
if (id^.rExtensionList <> nil) then
    begin
    for n := 0 to (id^.rExtensionList.Count - 1) do
        begin
        ex := id^.rExtensionList.Items[n];
        if ((ex^.rLabel = kGifLabelGraphic) and (gx = nil)) then
            begin
            gx := @(ex^.rGraphic);
            end;
        end;
    end;

FindGraphicExtension := gx;
end;

{ ---------------------------------------------------------------------------- }
{ make sure a graphic extension record exists for each image record }
{ only needed when updating animation or transparency }

procedure TGif.MakeGraphicExtensions;
var
    i,n:    integer;
    id:     PGifImageDescriptor;
    gx:     PGifExtensionGraphic;
    ge:     PGifExtension;
begin
for i := 0 to (fImageDescriptorList.Count - 1) do
    begin
    id := fImageDescriptorList.Items[i];
    if (id^.rExtensionList = nil) then id^.rExtensionList := TList.Create;
    gx := FindGraphicExtension(i);

    if (gx = nil) then
        begin
        new(ge);
        fillchar(ge^, sizeof(TGifExtension), 0);
        id^.rExtensionList.Add(ge);

        ge^.rLabel := kGifLabelGraphic;
        ge^.rGraphic.rBlockSize        := 4;
        ge^.rGraphic.rDisposal         := 0;
        ge^.rGraphic.rUserInputValid   := false;
        ge^.rGraphic.rTransparentValid := false;
        ge^.rGraphic.rDelayTime        := 0;
        ge^.rGraphic.rTransparentIndex := 0;
        end;
    end;
end;


{ ---------------------------------------------------------------------------- }

procedure TGif.ProgressInit(maxdata: integer; title: string);
var
    rr:     TRect;
begin
fProgress.rMax     := maxdata;
fProgress.rPercent := 0;
fProgress.rString  := title;
fProgress.rValue   := 0;
fillchar(rr, sizeof(TRect), 0);
fLastBitmap.OPProgress(Self, opsStarting, fProgress.rPercent, false, rr, fProgress.rString);
//if (assigned(OnProgress)) then  OnProgress(Self, psStarting, fProgress.rPercent, false, rr, fProgress.rString);
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.ProgressFinit;
var
    rr:     TRect;
begin
fProgress.rPercent := 100;
fillchar(rr, sizeof(TRect), 0);

fLastBitmap.OPProgress(Self, opsEnding, fProgress.rPercent, false, rr, fProgress.rString);
//if (assigned(OnProgress)) then  OnProgress(Self, psEnding, fProgress.rPercent, false, rr, fProgress.rString);
end;

{ ---------------------------------------------------------------------------- }
{ only update every 100 increments or so ... this is to avoid over-whelming }
{ updates to a displayed progress bar.  those displays can be slower than }
{ a GIF encode/decode operation }

procedure TGif.ProgressUpdate(value: integer);
var
    i,n:    integer;
    rr:     TRect;
begin
if (value >= (fProgress.rValue + 100)) then
    begin
    if      (value <= 0)              then fProgress.rPercent := 0
    else if (value >= fProgress.rMax) then fProgress.rPercent := 100
    else    fProgress.rPercent := (value * 100) div fProgress.rMax;
    fProgress.rValue := value;
    fLastBitmap.OPProgress(Self, opsRunning, fProgress.rPercent, false, rr, fProgress.rString);
//    if (assigned(OnProgress)) then  OnProgress(Self, psRunning, fProgress.rPercent, false, rr, fProgress.rString);
    end;
end;

{ ---------------------------------------------------------------------------- }
{ find the color within the color table; returns 0..255 }
{ return -1 if color not found }


function TGif.FindColorIndex(c: TColor; ct: PGifColorTable): integer;
var
    i,n:    integer;
begin
n := -1;
for i := 0 to (ct^.rSize - 1) do
    begin
    if ((n < 0) and (ct^.rColors[i] = c)) then n := i;
    end;

FindColorIndex := n;
end;



end.
