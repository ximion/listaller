unit bmp;
//
// Copyright (c) 1997,1998, 2001 Colosseum Builders, Inc.
// All rights reserved.
//
// Colosseum Builders, Inc. makes no warranty, expressed or implied
// with regards to this software. It is provided as is.
//
// See the README.TXT file that came with this software for restrictions
// on the use and redistribution of this file or send E-mail to
// info@colosseumbuilders.com
//

//
//  BMP Decoder Library.
//
//  Title:   BmpDecoder Class Implementation
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class decodes Windows BMP file.
//
//  Adjusted for TOPBitmap 2006 Theo Lustenberger with written consent of John M. Miano

//  Contains Encoding Code from the Freepascal project

interface

uses bitmapimage, opbitmap, classes, Sysutils;


const
  BITSPERBYTE = 8 ;
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;
  BMmagic=19778;  

type
  DWORD = Cardinal ;
  FXPT2DOT30 = Longint;

  RLEOPCODE = Packed Record
    count, command : Byte ;
    End ;
  TBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: Cardinal ;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Cardinal ;
  End;


  BITMAPCOREHEADER = packed record
    bcSize: DWORD ;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;

  RGBTRIPLE = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  TBITMAPINFOHEADER = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;


  TCIEXYZ = packed record
    ciexyzX: FXPT2DOT30;
    ciexyzY: FXPT2DOT30;
    ciexyzZ: FXPT2DOT30;
  end;


  TCIEXYZTRIPLE = packed record
    ciexyzRed: TCIEXYZ;
    ciexyzGreen: TCIEXYZ;
    ciexyzBlue: TCIEXYZ;
  end;

  TBITMAPV4HEADER = packed record
    bV4Size: DWORD;
    bV4Width: Longint;
    bV4Height: Longint;
    bV4Planes: Word;
    bV4BitCount: Word;
    bV4V4Compression: DWORD;
    bV4SizeImage: DWORD;
    bV4XPelsPerMeter: Longint;
    bV4YPelsPerMeter: Longint;
    bV4ClrUsed: DWORD;
    bV4ClrImportant: DWORD;
    bV4RedMask: DWORD;
    bV4GreenMask: DWORD;
    bV4BlueMask: DWORD;
    bV4AlphaMask: DWORD;
    bV4CSType: DWORD;
    bV4Endpoints: TCIEXYZTriple;
    bV4GammaRed: DWORD;
    bV4GammaGreen: DWORD;
    bV4GammaBlue: DWORD; 
  end;

   TColorRGB=packed record
     B,G,R:Byte;
   end;
   PColorRGB = ^TColorRGB;

   TColorRGBA=packed record
   case Boolean of
      False:(B,G,R,A:Byte);
      True:(RGB:TColorRGB);
   end;
   PColorRGBA = ^TColorRGBA;

  RGBQUAD = packed record
    rgbBlue, rgbGreen, rgbRed, spare: BYTE;
  end;

  TBmpDecoder = class(TBitmapImageDecoder)
  private
    color_table: array[0..255] of RGBQUAD;
    color_table_size: cardinal;
    fLastBitmapInfoHeader: TBITMAPINFOHEADER;
  public

    constructor Create;
    destructor Destroy; override;

    procedure ReadImage(strm: TStream; image: TOPBitmap);
    property BitmapInfoHeader: TBITMAPINFOHEADER read fLastBitmapInfoHeader;
  end;


  TBmpEncoder = class
  private
    StartPosition : int64; 
    FBpp : byte;
    FRLECompress : boolean;
    BFH : TBitMapFileHeader;
    BFI : TBitMapInfoHeader;               
    Colinfo : array of TColorRGBA;
    procedure SetColorSize (AValue : Byte);
    function GetColorSize : byte;
    procedure SetBpp (const abpp : byte);
    procedure FillColorMap(Img : TOPBitmap);
    procedure Setup16bpp;
    procedure Setup32bpp;
    procedure CompressScanLineRLE8(ALine : pbytearray; const Row, Width : Integer; Stream : TStream);
    procedure CompressScanLineRLE4(ALine : pbytearray; const Row, Width : Integer; Stream : TStream);
  protected
    function  SaveHeader(Stream:TStream; Img: TOPBitmap):boolean; virtual;

  public
    constructor Create;
    procedure WriteImage (Stream:TStream; Img: TOPBitmap);
    property BitsPerPixel : byte read FBpp write SetBpp;
    property RLECompress : boolean read FRleCompress write FRleCompress;
    Property BytesPerPixel : Byte Read GetColorSize Write SetColorSize;
  end;




  EBmpError = class(EGraphicsException)
  end;

implementation

uses Types, Math;

function ReverseByte(const X : byte) : byte ;
const A : array [$0..$F] of byte =
  ($0,$8,$4,$C, $2,$A,$6,$E, $1,$9,$5,$D, $3,$B,$7,$F) ;
begin
ReverseByte := (A[X and $0F] shl 4) or (A[X shr 4])
end ;

function ByteSwapColor(Color: LongWord): LongWord;
begin
  Pixel32(Result).Blue := Pixel32(Color).Red;
  Pixel32(Result).Green := Pixel32(Color).Green;
  Pixel32(Result).Red := Pixel32(Color).Blue;
  Pixel32(Result).Alpha:=Pixel32(Color).Alpha;
end;


procedure FindMask(mask: Cardinal; var offset, size: Cardinal);
begin
  offset := 0;
  while ((mask and (1 shl offset)) = 0) and (offset < 32) do
    inc(offset);
  size := 0;
  while (offset + size < 32) and ((mask and (1 shl (offset + size))) <> 0) do
    Inc(size);
end;


constructor TBmpDecoder.Create;
begin
  inherited Create;
end;

destructor TBmpDecoder.Destroy;
begin
  inherited Destroy;
end;


procedure TBmpDecoder.readImage(strm: TStream; image: TOPBitmap);
var
  bytesread: Cardinal;
  fileheader: TBITMAPFILEHEADER;
  count: Integer;
  headerbuffer: array[0..512] of DWord;
  width: Cardinal;
  height: Integer;
  imagesize: Cardinal;
  bitcount: Cardinal;
  compression: Cardinal;
  redmask, greenmask, bluemask, alphamask: Cardinal;
const
  Signature = BMMagic;

  function ReadOs2ColorTable(colorcount: Cardinal): Cardinal;
  var
    ii: Integer;
    buffer: array[0..255] of RGBTRIPLE;
    count: Integer;
  begin
    count := strm.read(buffer[0], colorcount * sizeof(RGBTRIPLE));
    if count <> colorcount * sizeof(RGBTRIPLE) then
      raise EBmpError.Create('Premature end of file in color table');
    for ii := 0 to colorcount - 1 do
    begin
      color_table[ii].rgbRed := buffer[ii].rgbtRed;
      color_table[ii].rgbBlue := buffer[ii].rgbtBlue;
      color_table[ii].rgbGreen := buffer[ii].rgbtGreen;
    end;
    Result := count;
  end;

  function ReadColorTable(colorcount: Cardinal): Cardinal;
  var
    count: Cardinal;
  begin
    count := strm.read(color_table[0], sizeof(RGBQUAD) * colorcount);
    if (count <> sizeof(RGBQUAD) * colorcount) then
      raise EBmpError.Create('Premature end of file in color table');
    Result := count;
  end;

  procedure GetOs2Header;
  var
    header: ^BITMAPCOREHEADER;
  begin
    header := @headerbuffer;
    width := header^.bcWidth;
    height := header^.bcHeight;
    bitcount := header^.bcBitCount;

    color_table_size := 1 shl bitcount;
    Inc(bytesread, readOs2ColorTable(color_table_size));
    compression := BI_RGB;
  end;

  procedure GetWindowsHeader;
  var
    Header: ^TBITMAPINFOHEADER;
  begin
    header := @headerbuffer;
    fLastBitmapInfoHeader := header^; 
    compression := header^.biCompression;
    width := header^.biWidth;
    height := header^.biHeight;
    bitcount := header^.biBitCount;

    color_table_size := header^.biClrUsed;
    if (color_table_size = 0) and (bitcount < 16) then
    begin
      // If the colors used field is non-zero, it gives the size of the
      // color table. Otherwise, the number of bits per pixel gives the size.
      color_table_size := 1 shl bitcount;
    end;

    // Validate Compression
    case bitcount of
      1:
        if (compression <> BI_RGB) then
          raise EBmpError.Create('Unsupported Compression');
      4:
        if (compression <> BI_RGB) and (compression <> BI_RLE4) then
          raise EBmpError.Create('Unsupported Compression');
      8:
        if (compression <> BI_RGB) and (compression <> BI_RLE8) then
          raise EBmpError.Create('Unsupported Compression');
      24:
        if (compression <> BI_RGB) then
          raise EBmpError.Create('Unsupported Compression');
      16, 32:
        if (compression <> BI_RGB) and (compression <> BI_BITFIELDS) then
          raise EBmpError.Create('Unsupported Compression');
    else
      raise EBmpError.Create('Unsupported bit count');
    end;
    if (color_table_size <> 0) then
      Inc(bytesread, readColorTable(color_table_size));
  end;

  procedure Get16BitMasks;
  var
    Header: ^TBITMAPV4HEADER;
  begin
    header := @headerbuffer;
    redmask := header^.bV4RedMask;
    if (redmask = 0) then
      redmask := $7C00;
    greenmask := header^.bV4GreenMask;
    if (greenmask = 0) then
      greenmask := $3E0;
    bluemask := header^.bV4BlueMask;
    if (bluemask = 0) then
      bluemask := $1F;
    alphamask := 0;
    if (headerbuffer[0] >= sizeof(TBITMAPV4HEADER)) then
      alphamask := header^.bV4AlphaMask;
{      writeln('Red16 ',header^.bV4RedMask,' ',$7c00);
      writeln('Green16 ',header^.bV4GreenMask,' ',$3e0);
      writeln('Blue16 ',header^.bV4BlueMask,' ',$1F);  }
  end;

  procedure Get32BitMasks;
  var
    header: ^TBITMAPV4HEADER;
  begin
    header := @headerbuffer;
    redmask := header^.bV4RedMask;
    if (redmask = 0) then
      redmask := $00FF0000;
    greenmask := header^.bV4GreenMask;
    if (greenmask = 0) then
      greenmask := $0000FF00;
    bluemask := header^.bV4BlueMask;
    if (bluemask = 0) then
      bluemask := $000000FF;
    alphamask := $FF000000;        //todo: think again
    if (headerbuffer[0] >= sizeof(TBITMAPV4HEADER)) then
      alphamask := header^.bV4AlphaMask;

 {     writeln('Red32 ',header^.bV4RedMask,' ',$00FF0000);
      writeln('Green32 ',header^.bV4GreenMask,' ',$0000FF00);
      writeln('Blue32 ',header^.bV4BlueMask,' ',$000000FF); }
  end;

  procedure SkipBytes(count: integer);
  var
    Buffer: array[1..512] of Byte;
  begin
    while count > 0 do
    begin
      if Count > Sizeof(Buffer) then
        strm.read(Buffer[1], sizeof(Buffer))
      else
        strm.read(Buffer[1], count);
      Dec(count, sizeof(buffer));
    end
  end;

  procedure AllocStorage;
  begin
    if (height >= 0) then
    begin
      imagesize := width * height;
      image.Width := Width;
      image.Height := Height;
    end
    else
    begin
      imagesize := width * -height;
      image.Width := Width;
      image.Height := -Height;
    end;
  end;

  procedure callProgressFunction(offset: Cardinal);
  const
    OPERATION = 'BMP Decode';
  var
    cancel: Boolean;
  begin
    if Assigned(progress_function) then
    begin
      cancel := false;
      progress_function(Self, progress_data, 1, 1, OPERATION, 100 * offset div Imagesize, cancel);
      if (cancel) then
        raise EGraphicsAbort.Create('');
    end;
  end;

  procedure readFractionalByteData;
  var
    bitwidth, rowwidth, physicalrowsize: Cardinal;
    buffer: array of Byte;
    count: Integer;
    ii: Integer;
    color: Cardinal;
    procedure ReadRow(rowindex: Cardinal);
    const
      Masks: array[1..4] of Cardinal = ($1, $3, 0, $F);
    var
      ii, jj: Cardinal;
      offset, position: Cardinal;
    begin
      callProgressFunction(rowindex);
      count := strm.read(buffer[0], physicalrowsize);
      if (count <> physicalrowsize) then
        raise EBmpError.Create('Premature End of Stream');

      // The pixel rows are stored in reverse order.
      jj := 0;
      for ii := 1 to width do
      begin
        position := jj div (BITSPERBYTE div bitcount);
        offset := BITSPERBYTE - ((jj mod (BITSPERBYTE div bitcount)) + 1) * bitcount;
        color := (buffer[position] shr offset) and masks[bitcount];
        case bitcount of
          1: TBitmapData1(image.Data).NativePixels[jj, rowindex] := not Boolean(color);
          4: TBitmapData4(image.Data).NativePixels[jj, rowindex] := color;
        end;
        Inc(jj);
      end;
    end;
  begin
    // Number of bits required for each pixel row.
    bitwidth := bitcount * width;
    // Number of bytes need to store each pixel row.
    rowwidth := (bitwidth + BITSPERBYTE - 1) div BITSPERBYTE;
    // Number of bytes used to store each row in the BMP file.
    // This is is rowwidth rounded up to the nearest 4 bytes.
    physicalrowsize := (rowwidth + $3) and not $3;

    SetLength(buffer, physicalrowsize);

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    if height > 0 then
      for II := 0 to Height - 1 do
        ReadRow(height - ii - 1)
    else
      for II := 0 to 1 - Height do
        ReadRow(height);
  end;

  procedure Read8Bitdata;
  var
    Buffer: array of Byte;
    physicalrowsize: Cardinal;
    ii: Integer;
    procedure ReadRow(rowindex: Cardinal);
    var
      Count: Integer;
      ii, jj: Integer;
      color: Cardinal;
    begin
      callProgressFunction(rowindex);
      count := strm.read(buffer[0], physicalrowsize);
      if (count <> physicalrowsize) then
        raise EBmpError.Create('Premature End of Stream');

  jj := 0;

      for ii := RowIndex * Width to (RowIndex + 1) * width - 1 do
      begin
        color := buffer[jj];
        TBitmapData8(image.Data).RawArray^ [ii] := color;
        jj := jj + 1;
      end;
    end;
  begin
    physicalrowsize := (width + $3) and not $3;
    SetLength(buffer, physicalrowsize);

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    if (height > 0) then
      for ii := 0 to Height - 1 do
        ReadRow(height - ii - 1)
    else
      for ii := 0 to 1 - Height do
        ReadRow(ii);
  end;

  procedure Read16BitData;
  var
    allbits: Cardinal;
    redsize, redoffset,
      greensize, greenoffset,
      bluesize, blueoffset,
      alphasize, alphaoffset: Cardinal;
    physicalrowsize: Cardinal;
    buffer: array of Word;
    ii: Integer;
    procedure ReadRow(rowindex: Cardinal);
    var
      ii, jj: Integer;
    begin
      callProgressFunction(rowindex);
      count := strm.read(buffer[0], physicalrowsize);
      if (count <> physicalrowsize) then
        raise EBmpError.Create('Premature End of Stream');

      jj := 0;
      for ii := rowindex to rowindex + width - 1 do
      begin //Typecasted to 16 but works for 15 too
        TBitmapData16(image.Data).RawArray^ [ii] := buffer[jj];
        Inc(jj);
      end;
    end;
  begin
    allbits := redmask or greenmask or bluemask or alphamask;
    if ((allbits and redmask) <> redmask) or ((allbits and greenmask) <> greenmask)
      or ((allbits and bluemask) <> bluemask) or ((allbits and alphamask) <> alphamask) then
      raise EBmpError.Create('Overlapping component mask');

    FindMask(redmask, redoffset, redsize);
    FindMask(greenmask, greenoffset, greensize);
    FindMask(bluemask, blueoffset, bluesize);
    FindMask(alphamask, alphaoffset, alphasize);


    if redmask = $7C00 then
      image.PixelFormat := pf15bit
    else if redmask = $F800 then
      image.PixelFormat := pf16bit else raise EPasBitMapError.Create('Custom 16 bit mask not supported');
    AllocStorage;

    physicalrowsize := (Sizeof(Word) * width + $3) and not $3;
    SetLength(buffer, physicalrowsize div sizeof(Word));

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    if (height > 0) then
      for ii := 0 to height - 1 do
        ReadRow((height - ii - 1) * width)
    else
      for ii := 0 to Abs(height)-1 do
      ReadRow((ii) * width);
     //   callProgressFunction(ii * 100 div -height);    //todo??
  end;

  procedure Read24BitData;
  var
    physicalrowsize: Cardinal;
    buffer: array of Byte;
    ii: Integer;
    count: Integer;

    procedure ReadRow(rowindex: Cardinal);
    var
      ii, jj: Integer;
    begin
      callProgressFunction(rowindex);
      jj := 0;
      count := strm.read(buffer[0], physicalrowsize);
      if count <> physicalrowsize then
        raise EBmpError.Create('Premature EOF in image data');
      for ii := rowindex to rowindex + width - 1 do
      begin
       Move(buffer[jj],TBitmapData24(image.Data).RawArray^[ii],3);
       Inc(jj, 3);
      end;
    end;
  begin

    physicalrowsize := (3 * width + $3) and not $3;
    SetLength(buffer, physicalrowsize);

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.

      if (height > 0) then
      for ii := 0 to height - 1 do
        ReadRow((height - ii - 1) * width)
    else
      for ii := 0 to Abs(height)-1 do
        ReadRow(ii * width) ;
      end;

  procedure Read32BitData;
  var
    allbits: Cardinal;
    redsize, redoffset,
      greensize, greenoffset,
      bluesize, blueoffset,
      alphasize, alphaoffset: Cardinal;
    buffer: array of Cardinal;
    physicalrowsize: Cardinal;
    ii: Integer;
    pix:PPixel32;

    procedure ReadRow(rowindex: Cardinal);
    var
      count: Integer;
      ii, jj: Integer;
    begin
      callProgressFunction(rowindex);
      count := strm.read(buffer[0], physicalrowsize);
      if (count <> physicalrowsize) then
        raise EBmpError.Create('Premature End of Stream');

      jj := 0;
      for ii := rowindex to rowindex + width - 1 do
      begin
        pix:= @TBitmapData32(image.Data).RawArray^[ii];
        pix^.red := ((buffer[jj] and redmask) shr (redoffset + redsize - BITSPERBYTE)) and $FF;
        pix^.green := ((buffer[jj] and greenmask) shr (greenoffset + greensize - BITSPERBYTE)) and $FF;
        pix^.blue := ((buffer[jj] and bluemask) shr (blueoffset + bluesize - BITSPERBYTE)) and $FF;
        pix^.alpha := ((buffer[jj] and alphamask) shr (alphaoffset + alphasize - BITSPERBYTE)) and $FF;

        Inc(jj);
      end;
    end;

  begin
    allbits := redmask or greenmask or bluemask or alphamask;
    if ((allbits and redmask) <> redmask) or ((allbits and greenmask) <> greenmask)
      or ((allbits and bluemask) <> bluemask) or ((allbits and alphamask) <> alphamask) then
      raise EBmpError.Create('Overlapping component mask');

    FindMask(redmask, redoffset, redsize);
    FindMask(greenmask, greenoffset, greensize);
    FindMask(bluemask, blueoffset, bluesize);
    FindMask(alphamask, alphaoffset, alphasize);

    physicalrowsize := sizeof(Cardinal) * width;
    SetLength(buffer, width);

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    if (height > 0) then
      for ii := 0 to height - 1 do
        ReadRow((height - ii - 1) * width)
    else
      for ii := 0 to Abs(height)-1 do
        ReadRow(ii * width) ;

  end;

  procedure ReadRle4;
  var
    row, col: Cardinal;
    done: boolean;
    opcode: RLEOPCODE;
    count: Integer;
    dx, dy: Byte;
    data, low, hi: Byte;
    ii: Integer;
  begin
    if (height < 0) then
      raise EBmpError.Create('Negative height not allowed in an RLE image');

    // The mechanism here is the same as for BI_RLE8 with two
    // exceptions. Here we are dealing with 4-bit nibbles rather
    // than whole bytes. This results in some extra work. In
    // addition, the coding of runs includes two color values.

    row := height - 1;
    col := 0;
    done := false;
    while not Done do
    begin
      callProgressFunction(row * width + col);
      count := strm.read(opcode, sizeof(opcode));
      if (count <> sizeof(opcode)) then
        raise EBmpError.Create('Premature EOF in RLE-compressed data');

      if (opcode.count = 0) then
      begin
        case (opcode.command) of
          0: // Advance to next pixel row
            begin
              if row > 0 then
                dec(row);
              col := 0;
            end;
          1: // Image complete
            done := true;
          2: // Move to relative location the image.
            begin
              count := strm.read(dx, sizeof(dx));
              if count <> sizeof(dx) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
              count := strm.read(dy, sizeof(dy));
              if count <> sizeof(dy) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
              Inc(col, dx);
              Dec(row, dy);
            end;
        else
          begin
            if (row >= height) or (col + opcode.command > width) then
              raise EBmpError.Create('Corrupt RLE-compressed Data');
            for ii := 0 to opcode.command - 1 do
            begin
              if ((ii and 1) = 0) then
              begin
                strm.read(data, sizeof(data));
                low := (data and $F);
                hi := (data and $F0) shr 4;
                case bitcount of
                  1: TBitmapData1(image.Data).NativePixels[col, row] := not Boolean(hi);
                  4: TBitmapData4(image.Data).NativePixels[col, row] := hi;
                end;
              end
              else
              begin
                case bitcount of
                  1: TBitmapData1(image.Data).NativePixels[col, row] := not Boolean(low);
                  4: TBitmapData4(image.Data).NativePixels[col, row] := low;
                end;
              end;
              inc(col);
            end;
            // If the number of bytes used in this instruction
            // is odd then there is a padding byte. Note that if
            // command Mod 4 is 3, that means that the data is word aligned,
            // but we only used 4-bit of the last byte.
            if (opcode.command mod 4) in [1..2] then
            begin
              count := strm.read(data, sizeof(data));
              if count <> sizeof(data) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
            end;
          end;
        end
      end
      else
      begin
        // Process a run of the same color value pairs.
        hi := opcode.command shr 4;
        low := opcode.command and $F;
        if (row >= height) or (col + opcode.count > width + 1) then //+1 theo
          raise EBmpError.Create('Corrupt RLE-compressed Data');

        for ii := 0 to opcode.count - 1 do
        begin
          if (ii and 1) = 0 then
          begin
            case bitcount of
              1: TBitmapData1(image.Data).NativePixels[col, row] := not Boolean(hi);
              4: TBitmapData4(image.Data).NativePixels[col, row] := hi;
            end;
          end
          else
          begin
            case bitcount of
              1: TBitmapData1(image.Data).NativePixels[col, row] := not Boolean(low);
              4: TBitmapData4(image.Data).NativePixels[col, row] := low;
            end;
          end;
          Inc(col);
        end;
      end
    end;
  end;

  procedure ReadRle8;
  var
    row, col: Cardinal;
    done: boolean;
    opcode: RLEOPCODE;
    count: integer;
    dx, dy: Byte;
    data: Byte;
    ii: integer;
  begin
    if (height < 0) then
      raise EBmpError.Create('Negative height not allowed in an RLE image');


    row := height - 1; // Current row
    col := 0; // Current column
    done := false;
    while not Done do
    begin
      callProgressFunction(row * width + col);
      count := strm.read(opcode, sizeof(opcode));
      if count <> sizeof(opcode) then
        raise EBmpError.Create('Premature EOF in RLE-compressed data');
      if (opcode.count = 0) then
      begin
        // A byte count of zero means that this is a special
        // instruction.
        case (opcode.command) of
          0: // 0 => Move to next row
            begin
              if row > 0 then
                dec(row);
              col := 0;
            end;
          1: // 1 => Image is finished
            done := true;
          2: // 2 => Move to a new relative position.
            begin
            // Read the relative position.
              count := strm.read(dx, sizeof(dx));
              if count <> sizeof(dx) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
              count := strm.read(dy, sizeof(dy));
              if count <> sizeof(dy) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
              inc(col, dx);
              dec(row, dy);
            end
        else
          begin
            // Store absolute data. The command code is the
            // number of absolute bytes to store.
            if (row >= height) or (col + opcode.command > width) then
              raise EBmpError.Create('Corrupt RLE-compressed Data');

            for ii := 0 to opcode.command - 1 do
            begin
              count := strm.read(data, sizeof(data));
              if count <> sizeof(data) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');

              TBitmapData8(image.Data).RawArray^[row * width + col] := data;

              Inc(col);
            end;
            // An odd number of bytes is followed by a pad byte.
            if ((opcode.command mod 2) = 1) then
            begin
              count := strm.read(data, sizeof(data));
              if count <> sizeof(data) then
                raise EBmpError.Create('Premature EOF in RLE-compressed data');
            end;
          end;
        end;
      end
      else
      begin
        // Store a run of the same color value.
        if (row >= height) or (col + opcode.count > width) then
          raise EBmpError.Create('Corrupt RLE-compressed Data');
        for ii := 0 to opcode.count - 1 do
        begin
          TBitmapData8(image.Data).RawArray^[row * width + col] := opcode.command;
          Inc(col);
        end;
      end;
    end;
  end;

begin

  bytesread := 0;
  FillChar(headerbuffer, 513, 0);
  FillChar(color_table, 1024, 0);


  count := strm.read(fileheader, sizeof(fileheader));
  if count <> sizeof(fileheader) then
    raise EBmpError.Create('Premature End of Stream');
  Inc(bytesread, count);

  if fileheader.bfType <> signature then
    raise EBmpError.Create('Not a BMP Image');

  // The header can come in one of two flavors.  They both
  // begin with a 4-byte headersize.

  count := strm.read(headerbuffer[0], Sizeof(DWORD));
  if count <> sizeof(DWORD) then
    raise EBmpError.Create('Premature End of Stream');

  count := strm.read(headerbuffer[1], headerbuffer[0] - sizeof(DWORD));
  if count <> headerbuffer[0] - sizeof(DWORD) then
    raise EBmpError.Create('Premature End of Stream');

  Inc(bytesread, headerbuffer[0]);

  color_table_size := 0;
  if (headerbuffer[0] = sizeof(BITMAPCOREHEADER)) then
    GetOs2Header
  else if (headerbuffer[0] >= sizeof(BITMAPINFOHEADER)) then
    GetWindowsHeader
  else
    raise EBmpError.Create('Invalid header size');

  if (bytesread > fileheader.bfOffBits) then
    raise EBmpError.Create('Corrupt File');


  if fileheader.bfOffBits - bytesread > 0 then //more headers
    if (bitcount = 16) or (bitcount = 32) then
    begin
      count := strm.read(headerbuffer[(bytesread div 4) - 3], fileheader.bfOffBits - bytesread);
      Inc(bytesread, count);
    end else SkipBytes(fileheader.bfOffBits - bytesread);

  image.Width := 0;

  if color_table_size>0 then
  image.CopyFromColorTable(TColorTableArray(color_table), True, color_table_size);

  case bitcount of
    1, 4:
      begin
        case bitcount of
          1: image.PixelFormat := pf1bit;
          4: image.PixelFormat := pf4bit;
        end;

        if compression = BI_RGB then
        begin
          AllocStorage;
          readFractionalByteData
        end
        else
        begin
          AllocStorage;
          readRle4;
        end;
      end;
    8:
      begin
        image.PixelFormat := pf8bit;
        AllocStorage;
        if compression = BI_RGB then
          read8BitData
        else
          readRle8;
      end;
    16:
      begin
        Get16BitMasks;
        read16BitData;
      end;
    24: begin
        image.PixelFormat := pf24bit;
        AllocStorage;
        read24BitData;
      end;
    32:
      begin
        image.PixelFormat := pf32bit;
        AllocStorage;
        Get32BitMasks;
        read32BitData;
      end;
    else raise EBmpError.CreateFmt('BPP %d not supported',[bitcount]);
  end;

  callProgressFunction(imagesize);
end;


constructor TBmpEncoder.create;
begin
  inherited create;
  FBpp:=24;
  FRleCompress:=false;
end;

{ Only for compatibility, BytesPerPixel should be removed }
{ ******************************************************* }
procedure TBmpEncoder.SetColorSize (AValue : byte);
begin
  SetBpp(AValue*8);
end;

function TBmpEncoder.GetColorSize : byte;
begin
  if FBpp<>15 then Result:=FBpp div 8
  else Result:=2;
end;
{ ******************************************************* }

procedure TBmpEncoder.SetBpp (const abpp : byte);
begin
  if not (abpp in [1,4,8,15,16,24,32]) then
    raise Exception.Create('Invalid color depth');
  FBpp:=abpp;
end;



procedure TBmpEncoder.FillColorMap(Img : TOPBitmap);
var BadPalette : boolean;
    i : integer;
begin
  BadPalette:=false;
  if Img.ColorTableSize>(1 shl FBpp) then BadPalette:=true;
  if BadPalette then 
    raise Exception.Create('Image palette is too big or absent');
// writeln('COLORS ',Img.ColorTableSize);
  BFI.biClrUsed:=Min(256,Img.ColorTableSize);  //todo ct size +1 strange and wrong sizes..
  setlength(ColInfo,BFI.biClrUsed);
  for i:=0 to BFI.biClrUsed-1 do
  begin
    ColInfo[i] := TColorRGBA(ByteSwapColor(Img.ColorTable^[i]));
    ColInfo[i].A:=0;
  end;
end;

{ True 16 bit color is 5 bits red, 6 bits green and 5 bits blue.
  Compression must be set to BI_BITFIELDS and we must specify masks for red, green and blue.
  16 bit without compression and masks is 5 bits per channel, so it's 15 bit even if in the header we
  must write 16.
  It's possible to provide custom masks but this is not compatible with windows9x, so we use 555 for 15 bit
  and 565 for 16 bit.
  Masks are longwords stored in the palette instead of palette entries (which are 4 bytes long too, with
  components stored in following order: B G R A. Since we must write a low-endian longword, B is LSB and A
  is the MSB).
  We must write first red mask, then green and then blue.

  This sounds terribly confusing, if you don't understand take a look at
  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/bitmaps_1rw2.asp
   }

procedure TBmpEncoder.Setup16bpp;
var col : TColorRGBA;
begin
  BFI.biCompression:=BI_BITFIELDS;
  setlength(ColInfo,3);
  {      A R G B
  r := $0000F800
  g := $000007E0
  b := $0000001F
  }
  col.A:=0; Col.R:=0; { These are 0 for all the three masks}
  { Red Mask }
  Col.G:=$F8; Col.B:=0;
  ColInfo[0]:=Col;
  { Green Mask }
  Col.G:=$07; Col.B:=$E0;
  ColInfo[1]:=Col;
  { Blue Mask }
  Col.G:=$00; Col.B:=$1F;
  ColInfo[2]:=Col;
end;

procedure TBmpEncoder.Setup32bpp;
var col : TColorRGBA;
begin
  BFI.biCompression:=BI_BITFIELDS;
  setlength(ColInfo,4);

  { Red Mask }
  Col.R:=$FF; Col.G:=0; Col.B:=0; Col.A:=0;
  ColInfo[0]:= Col;
  { Green Mask }
  Col.R:=0; Col.G:=$FF; Col.B:=0; Col.A:=0;
  ColInfo[1]:= Col;
  { Blue Mask }
  Col.R:=0; Col.G:=0; Col.B:=$FF; Col.A:=0;
  ColInfo[2]:=Col;
  { Alpha Mask }
  Col.R:=0; Col.G:=0; Col.B:=0; Col.A:=$FF;
  ColInfo[3]:=Col;
end;




function TBmpEncoder.SaveHeader(Stream:TStream; Img : TOPBitmap):boolean;
begin
  Result:=False;
  with BFI do
    begin
    biSize:=sizeof(TBitMapInfoHeader);
    biWidth:=Img.Width;
    biHeight:=Img.Height;
    biPlanes:=1;
    if FBpp=15 then biBitCount:=16
    else biBitCount:=FBpp;
    biXPelsPerMeter:=100;
    biYPelsPerMeter:=100;
    biClrImportant:=0;
    end;
  with BFH do
    begin
    bfType:=BMmagic;//'BM'
    bfOffBits:=sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader)+length(ColInfo)*4;
    bfReserved1:=0;
    bfReserved2:=0;
    bfSize:=bfOffBits+BFI.biSizeImage;
    end;
  Stream.seek(0,soFromBeginning);
  Stream.Write(bfh,sizeof(TBitMapFileHeader));
  Stream.Write(bfi,sizeof(TBitMapInfoHeader));
  Result:=true;
end;

{ This code is rather ugly and difficult to read, but compresses better than gimp.
  Brief explanation:
  A repetition is good if it's made of 3 elements at least: we have 2 bytes instead of 1. Let's call this a 
  "repetition" or "true repetition".
  So we start finding the first repetition from current position.
  Once found, we must decide how to handle elements between current position (i) and the repetition position (j)
  if j-i = 0 we are on the repetition, so we encode it
  if j-i = 1 there is only one pixel. We can't do anything but encode it as a repetition of 1 element.
  if j-i = 2 we have two pixels. These can be a couple (a repetition of 2 elements) or 2 singles
             (2 repetitions of 1 element)
  if j-i > 2 we have two choices. In fact, we must consider that absolute mode is 2 bytes + length of chunk.
             A repetition is always 2 bytes, so for 1 element we leak 1 byte, while for 2 elements we don't leak
             any byte.
             So if we have at most 1 single this means that everything else is made up of couples: it's best to
             use repetitions so that we leak 0 to 1 byte.
             If we have 2 singles or more it's better to use absolute mode, since we leak 2 bytes always,
             without regard to the size of chunk. }
             
             

procedure TBmpEncoder.CompressScanLineRLE8(ALine : pbytearray; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles : integer;
    prev,tmp : byte;
begin
  i:=0;
  while (i<Width) do
  begin
    { let's see how bytes are disposed, so that we can choose the best way to compress }
    couples:=0; singles:=1;
    prev:=Aline^[i];
    j:=i+1;
    while ((j<Width) and ((j-i)<255)) do
    begin
      if Aline^[j]=prev then { this is a couple at least }
      begin
        dec(singles); { so the previous one wasn't a single }
        if (((j+1)<Width) and (Aline^[j+1]=prev)) then { at least three equal items, it's a repetition }
        begin
          dec(j); { repetition starts at j-1, since j is the middle pixel and j+1 is the third pixel }
          break;
        end
        else inc(couples) { ok it's a couple }
      end
      else inc(singles); { this is a single if next isn't a couple }
      prev:=Aline^[j];
      inc(j);
    end;

    { ok, now that we know more about byte disposition we write data }
    case (j-i) of
      0 : begin { there is a repetition with count>=3 }
            prev:=Aline^[i];
            j:=i+1;
            while ((j<Width) and ((j-i)<255)) do
            begin
              if Aline^[j]<>prev then break;
              inc(j);
            end;
            tmp:=j-i;
            Stream.Write(tmp,1);
            Stream.Write(prev,1);
          end;
      1 : begin { single value: we write a repetition of 1 }
            tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline^[i],1);
          end;
      2 : begin
            if couples=1 then { a couple: we write a repetition of 2 }
            begin
              tmp:=2;
              Stream.Write(tmp,1);
              Stream.Write(Aline^[i],1);
            end
            else { two singles: we write two repetitions of 1 each }
            begin
              tmp:=1;
              Stream.Write(tmp,1);
              Stream.Write(Aline^[i],1);
              Stream.Write(tmp,1);
              Stream.Write(Aline^[i+1],1);
            end;
          end;
      else { here we have two choices }
      begin
        if singles>1 then { it's cheaper to use absolute mode }
        begin
          tmp:=0; Stream.Write(tmp,1);   { escape }
          tmp:=j-i; Stream.Write(tmp,1); { number of pixels in absolute mode }
          Stream.Write(Aline^[i],j-i);    { write these pixels... }
          if ((tmp mod 2)<>0) then       { we must end on a 2-byte boundary }
          begin
            tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
          end;
        end
        else { they're nearly all couples, don't use absolute mode }
        begin
          k:=i;
          while (k<j) do
          begin
            if ((k+1<j) and (Aline^[k]=Aline^[k+1])) then
            begin
              tmp:=2;
              inc(k);
            end
            else tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline^[k],1);
            inc(k);
          end;
        end;
      end;
    end;
    i:=j;
  end;
  tmp:=0; Stream.Write(tmp,1); { escape }
  if Row=0 then { last line, end of file }
    tmp:=1;
  Stream.Write(tmp,1);
end;




{ Ok, this is even uglier than the RLE8 version above, and this time gimp compresses better :\
  Differences with RLE8: repetition count is pixel-relative, not byte-relative, but repetition data is made
  of 2 pixels. So you have a repetition when you have pixels repeated in an alternate way, even if you can do
  something like:
  01E0 => E
  0316 => 161.
  A repetition is good if it's made of five elements at least (2 bytes instead of 3).
  In rle4 we consider "single" either a single nibble or 2 (a byte), while a couple is a repetition of 3 or 4
  elements. }

procedure TBmpEncoder.CompressScanLineRLE4(ALine : pbytearray; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles, lastsingle : integer;
    prev1, prev2, prev : word;
    tmp : byte;
    nibline : pbytearray; { temporary array of nibbles }
    even : boolean;
begin
  getmem(nibline,width);
  try
    k:=(Width div 2) + (Width mod 2);
    i:=0;
    while (i<k) do
    begin
      nibline^[i*2]:=aline^[i] shr 4;
      nibline^[i*2+1]:=aline^[i] and $F;
      inc(i);
    end;
    i:=0;
    while (i<Width) do
    begin
      { let's see how nibbles are disposed, so that we can choose the best way to compress }
      couples:=0; singles:=1; lastsingle:=-10;
      prev1:=nibline^[i];
      prev2:=nibline^[i+1];
      j:=i+2;
      while ((j<Width) and ((j-i)<255)) do
      begin
        if nibline^[j]=prev1 then { this is a half-couple at least (repetition of 3) }
        begin
          dec(singles); { so the previous one wasn't a single }
          if (((j+1)<Width) and (nibline^[j+1]=prev2)) then { at least a couple (repetition of 4) }
          begin
            if (((j+2)<Width) and (nibline^[j+2]=prev1)) then { at least a repetition of 5, good }
            begin
              dec(j,2); { repetition starts at j-2: prev1 prev2 prev1* prev2 prev1, we are here * }
              break;
            end
            else
            begin { ok it's a couple }
              inc(couples);
              if (j-i)=254 then { in this rare case, j-i becomes 256. So, force a half-couple and exit }
              begin
                inc(j);
                break;
              end;
              prev1:=256; { this is a couple, don't consider these positions in further scanning }
              prev2:=256;
              inc(j,2);
              continue;
            end
          end
          else
            begin { ok it's a half-couple }
            inc(couples);
            prev:=256; //this is a half-couple, don't consider this position in further scanning.
          end;
        end
        else
        begin
          if lastsingle<>(j-1) then
          begin
            inc(singles); { this is a single if next isn't a couple }
            lastsingle:=j;
          end;
          prev:=nibline^[j];
        end;
        prev1:=prev2;
        prev2:=prev;
        even:=not even;
        inc(j);
      end;
      if j>Width then j:=Width; { if j was Width-1 loop was skipped and j is Width+1, so we fix it }

      { ok, now that we know more about byte disposition we write data }
      case (j-i) of
        0 : begin { there is a repetition with count>=5 }
              even:=true;
              prev1:=nibline^[i];
              prev2:=nibline^[i+1];
              j:=i+2;
              while ((j<Width) and ((j-i)<255)) do
              begin
                if even then if nibline^[j]<>prev1 then break;
                if not even then if nibline^[j]<>prev2 then break;
                even:=not even;
                inc(j);
              end;
              tmp:=j-i;
              Stream.Write(tmp,1);
              prev:=(prev1 shl 4) + (prev2 and $F);
              tmp:=prev;
              Stream.Write(tmp,1);
            end;
        1 : begin { single value: we write a repetition of 1 }
              tmp:=1;
              Stream.Write(tmp,1);
              tmp:=nibline^[i] shl 4;
              Stream.Write(tmp,1);
            end;
        2 : begin { 2 singles in the same byte: we write a repetition of 2 }
              tmp:=2;
              Stream.Write(tmp,1);
              tmp:=(nibline^[i] shl 4) + (nibline^[i+1] and $F);
              Stream.Write(tmp,1);
            end;
        3 : begin
              if couples=1 then { a couple: we write a repetition of 3 }
              begin
                tmp:=3;
                Stream.Write(tmp,1);
                tmp:=(nibline^[i] shl 4) + (nibline^[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 and 1 respectively }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline^[i] shl 4) + (nibline^[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=1;
                Stream.Write(tmp,1);
                tmp:=nibline^[i+2] shl 4;
                Stream.Write(tmp,1);
              end;
            end;
        4 : begin
              if singles=0 then { a couple: we write a repetition of 4 }
              begin
                tmp:=4;
                Stream.Write(tmp,1);
                tmp:=(nibline^[i] shl 4) + (nibline^[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 each }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline^[i] shl 4) + (nibline^[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline^[i+2] shl 4) + (nibline^[i+3] and $F);
                Stream.Write(tmp,1);
              end;
            end;
        else { here we have two choices }
        begin
          if singles>1 then { it's cheaper to use absolute mode }
          begin
            tmp:=0; Stream.Write(tmp,1);    { escape }
            tmp:=j-i; Stream.Write(tmp,1);  { number of pixels in absolute mode }
            k:=i;
            while (k<j) do                  { write these pixels... }
            begin
              tmp:=nibline^[k] shl 4;
              inc(k);
              if k<j then
              begin
                tmp:=tmp+(nibline^[k] and $F);
                inc(k);
              end;
              Stream.Write(tmp,1);
            end;
            k:=j-i;
            k:=k+(k mod 2);
            if (k mod 4)<>0 then            { we must end on a 2-byte boundary }
            begin
              tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
            end;
          end
          else { they're nearly all couples, don't use absolute mode }
          begin
            k:=i;
            while (k<j) do
            begin
              if ((k+2<j) and (nibline^[k]=nibline^[k+2])) then
              begin
                if ((k+3<j) and (nibline^[k+1]=nibline^[k+3])) then tmp:=4
                else tmp:=3;
              end
              else
              begin
                if (k+1>=j) then tmp:=1
                else if ((k+3<j) and (nibline^[k+1]=nibline^[k+3])) then tmp:=1
                else tmp:=2;
              end;
              Stream.Write(tmp,1);
              prev:=tmp;
              tmp:=nibline^[k] shl 4;
              if tmp<>1 then tmp:=tmp+(nibline^[k+1] and $F);
              Stream.Write(tmp,1);
              inc(k,prev);
            end;
          end;
        end;
      end;
      i:=j;
    end;
    tmp:=0; Stream.Write(tmp,1); { escape }
    if Row=0 then { last line, end of file }
      tmp:=1;
    Stream.Write(tmp,1);
  finally
    FreeMem(nibline);
  end;
end;

procedure TBmpEncoder.WriteImage (Stream:TStream; Img:TOPBitmap);
var
  Row,Col,RowSize:Integer;
  PadCount : byte;
  aLine: PByteArray;
  i : Integer;
  tmppos : int64;
  continue : boolean;
  percent : byte;
  percentinterval : longword;
  percentacc : longword;
    Rect : TRect;
  PBt:PByte;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  percent:=0;
  percentinterval:=(Img.Height*4) div 100;
  if percentinterval=0 then percentinterval:=$FFFFFFFF;
  percentacc:=0;
//  Progress(Img,opsStarting,0,false,Rect,'',continue);
  Img.OPProgress(Img,opsStarting,0,false,Rect,'');
  if not continue then exit;
  if (FRLECompress and (not (FBpp in [4,8]))) then
    raise Exception.Create('Can''t use RLE compression with '+IntToStr(FBpp)+' bits per pixel');
  if FRLECompress and (FBpp=4) then BFI.biCompression:=BI_RLE4
  else if FRLECompress and (FBpp=8) then BFI.biCompression:=BI_RLE8
  else BFI.biCompression:=BI_RGB;
  BFI.biClrUsed:=0;
  try
    if FBpp<=8 then FillColorMap(Img); { sets colormap and ClrUsed}
    if FBpp=16 then Setup16bpp; { sets colormap with masks and Compression }
    if FBpp=32 then Setup32bpp;

    RowSize:=Img.Data.LineLength;
    PadCount:=(4-(RowSize mod 4)) mod 4; { every row must end on 4 byte boundary }
    inc(RowSize,PadCount);

    BFI.biSizeImage:=img.GetDataSize;

    SaveHeader(Stream,Img); { write the headers }
    for i:=0 to length(ColInfo)-1 do { write the palette (or the masks in 16bpp case) }
      Stream.Write(ColInfo[i],sizeof(TColorRGBA));

    GetMem(aLine,RowSize);
    try
      for Row:=Img.Height-1 downto 0 do
      begin
        i:=0; Col:=0;
        case FBpp of
          1 : while(Col<img.Width) do
              begin
               PAPixel8(aLine)^[i]:= ReverseByte(TBitmapData1(img.Data).RawArray^[Row * img.Data.LineLength + i]) xor $ff;
               inc(col,8);
               inc(i);
              end;
          4 : while(Col<img.Width) do
              begin
                PBt:=@TBitmapData4(img.Data).RawArray^[Row * img.Data.LineLength + i];
                PAPixel8(aLine)^[i]:=((PBt^ and $0F) shl 4) or ((PBt^ and $F0) shr 4);
                inc(col,2);
                inc(i);
              end;
          8 : for Col:=0 to img.Width-1 do
                begin
                PBt:=@TBitmapData8(img.Data).RawArray^[Row * img.Data.LineLength + Col];
//                PBt:=@TBitmapData8(img.Data).RawArray^[Row * img.Width + Col];
                PAPixel8(aLine)^[Col]:=PBt^;
                end;
         15 : for Col:=0 to img.Width-1 do
               PAPixel16(aLine)^[Col]:=TBitmapData15(img.Data).RawArray^[Row * img.Width + Col];
         16 : for Col:=0 to img.Width-1 do
               PAPixel16(aLine)^[Col]:=TBitmapData16(img.Data).RawArray^[Row * img.Width + Col];
         24 : for Col:=0 to img.Width-1 do
                PAPixel24(aLine)^[Col]:= TBitmapData24(img.Data).NativePixels[Col,Row];
         32 : for Col:=0 to img.Width-1 do
                PAPixel32(aLine)^[Col]:= TBitmapData32(img.Data).NativePixels[Col,Row];
        end;
        { pad the scanline with zeros }
        for i:=RowSize-PadCount to RowSize-1 do
          PbyteArray(aline)^[i]:=0;

        if BFI.biCompression=BI_RLE8 then CompressScanLineRLE8(aLine,Row,img.Width,Stream)
        else if BFI.biCompression=BI_RLE4 then CompressScanLineRLE4(aLine,Row,img.Width,Stream)
        else Stream.Write(aLine[0],RowSize);

        inc(percentacc,4);
        if percentacc>=percentinterval then
        begin
          percent:=percent+(percentacc div percentinterval);
          percentacc:=percentacc mod percentinterval;
          Img.OPProgress(Img,opsRunning,percent,false,Rect,''{,continue});
          if not continue then exit;
        end;
      end;
      { If image is compressed we must fix the headers since we now know the size of the image }
      if BFI.biCompression in [BI_RLE4,BI_RLE8] then
      begin
        tmppos:=Stream.Position-StartPosition-BFH.bfOffBits;
        BFI.biSizeImage:=tmppos;          { set size of the image }
        tmppos:=Stream.Position;        { remember where we are }
        Stream.Position:=StartPosition; { rewind to the beginning }
        SaveHeader(Stream,Img);         { rewrite headers (this will update BFH.Size too) }
        Stream.Position:=tmppos;        { restore our position }
      end;
      Img.OPProgress(Img,opsEnding,100,false,Rect,''{,continue});
    finally
      FreeMem(aLine);
    end;
  finally
    setlength(ColInfo,0);
  end;
end;


end.
