unit gifdecoder;
//
// Copyright (c) 2005 Colosseum Builders, Inc.
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
// Description:
//
//   TGifDecoder is a class for decoding GIF images into a TBitmapImage
//   object.
//
//   This decoder supports the GIF format as defined in
//   "GRAPHICS INTERCHANGE FORMAT", CompuServe, July 31, 1990.
//   Markers of the form #N reference section N of this document.
//
//  Author: John M. Miano - miano@colosseumbuilders.com
//  Date:  June 10, 2005
//

//Changes made by theo 2006 are marked with //theo
interface

uses
  Classes, gifinputstream, opbitmap, bitmapimage, gif, systemspecific;


type
  GifColorTable = array[0..255] of GifColorTableEntry;

  DictionaryTree = record
    data: UBYTE1;
    Parent: Cardinal;
  end;

  TGifDecoder = class(TBitmapImageDecoder)
  private
    global_color_table: GifColorTable;
    global_color_count: Cardinal;
    screen_width: Cardinal;
    screen_height: Cardinal;
    more_images: Boolean;
    verbose_flag: Boolean;

    multi_image_mode: Boolean;

      // Values to indicate the presence of a transparent color.
    transparent_flag: Boolean;
    transparent_color: Cardinal;

      // Position of last image on the logical screen.
    image_position_top, image_position_left: Cardinal;


      // We have to save a reference to the input stream
      // because GIF allows multiple images per file.
    input_stream: TGifInputStream;
    fGraphicCtrlBlock: GifGraphicControlBlock; //theo
    fGifImageDescriptor: GifImageDescriptor; //theo

    procedure ReadToNextImage;
    procedure callProgressFunction(pass, passcount: Cardinal; progress: Cardinal);
  protected
    procedure SetMultiImageMode(state: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure readImage(inputstream: TGifInputStream; image: TOPBitmap);
    procedure readNextImage(image: TOPBitmap);
    procedure readImageFile(filename: string; image: TOPBitmap); //override;
    procedure readImageStream(Stream:TStream; image :  TOPBitmap); //theo
    property MoreImages: Boolean read more_images;
    property Verbose: Boolean read verbose_flag write verbose_flag;
    property MultiImageMode: Boolean read multi_image_mode write SetMultiImageMode;
    function ActiveInputStream: Boolean;
    procedure CloseInputSTream;
    property ImagePositionTop: Cardinal read image_position_top;
    property ImagePositionLeft: Cardinal read image_position_left;
    property GraphicCtrlBlock: GifGraphicControlBlock read fGraphicCtrlBlock; //theo
    property GifImgDescriptor: GifImageDescriptor read fGifImageDescriptor; //theo
    procedure Reset; //theo moved to public
  end;

  EGifError = class(EGraphicsException);

implementation

uses
  gifInputfilestream;

constructor TGifDecoder.Create;
begin
  inherited Create;
  Reset;
end;

destructor TGifDecoder.Destroy;
begin

  inherited Destroy; // Final Step
end;

procedure TGifDecoder.Reset;
begin
  more_images := false;
  transparent_flag := false;
  input_stream := nil;
end;

//
//  Function to read the first image from the input stream.
//
//  Parameters:
//    inputstream : the stream to read the image from.
///   image : the image to read to.
//

procedure TGifDecoder.readImage(inputstream: TGifInputStream; image: TOPBitmap);

  // This nested function reads the fixed area at the start of the input stream.
  procedure ReadStreamHeader;
  var
    header: GifHeader;
    screen: GifScreenDescriptor;
    count: Cardinal;
    II: Integer;
  begin
    count := inputstream.read(header, sizeof(header));
    if count <> sizeof(header) then
      raise EGifError.Create('GIF: Cannot read GIF header');

    if verbose_flag then
      Print(header);

    if (header.file_signature[0] <> 'G')
      or (header.file_signature[1] <> 'I')
      or (header.file_signature[2] <> 'F')
      or (header.file_version[0] <> '8')
      or ((header.file_version[1] <> '7') and (header.file_version[1] <> '9'))
      or (header.file_version[2] <> 'a') then
      raise EGifError.Create('Not a GIF stream');

    count := inputstream.read(screen, sizeof(screen));
    if count <> sizeof(screen) then
      raise EGifError.Create('Cannot read GIF screen descriptor.');

    screen_width := screen.logical_screen_width;
    screen_height := screen.logical_screen_height;
    if verbose_flag then
      Print(screen);

    if GlobalColorTableFlag(screen) then
    begin
      global_color_count := GlobalColorTableSize(screen);
      for ii := 0 to global_color_count - 1 do
      begin
        Count := inputstream.read(global_color_table[ii], sizeof(global_color_table[ii]));
        if count <> sizeof(global_color_table[ii]) then
          raise EGifError.Create('Error reading global color table in GIF file.');
      end;
    end
    else
    begin
      global_color_count := 0;
    end;
  end; // ReadImageHeader


begin // Read Image

  if Assigned(input_stream) then
    raise EGifError.Create('GIF: Already have open input stream.');

  Reset;
  ReadStreamHeader;

  // We read the stream header through the parameter. We only save
  // the stream once we know it is good for something.
  input_stream := inputstream;
  ReadToNextimage;
  if not more_images then
    raise EGifError.Create('GIF: No image in input stream.');

  ReadNextImage(image);
end;

//
//  This function reads the first image from a GIF file.
//
//  Parameters:
//    filename : the input file name
//    image : the image to write to.
//

procedure TGifDecoder.readImageFile(filename: string; image: TOPBitmap);
var
  input: TGifInputFileStream;
begin
  if Assigned(input_stream) then
    raise EGifError.Create('GIF: Already have open input stream');

  input := TGifInputFileStream.Create(0);
  try
    input.open(filename);
    readImage(input, image);
  finally
    if not MultiImageMode and Assigned(input_stream) then
    begin
      if input_stream <> input then
        raise EGifError.Create('GIF: Internal Error - Corrupt input stream.');
      CloseInputStream;
    end;
  end;
end;


procedure TGifDecoder.readImageStream(Stream:TStream; image :  TOPBitmap);
var
  input: TGifInputTStream;
begin
  if Assigned(input_stream) then
    raise EGifError.Create('GIF: Already have open input stream');

  input := TGifInputTStream.Create(0);
  try
    Stream.position:=0;
    input.Stream:=Stream;
    readImage(input, image);
  finally
    if not MultiImageMode and Assigned(input_stream) then
    begin
      if input_stream <> input then
        raise EGifError.Create('GIF: Internal Error - Corrupt input stream.');
      CloseInputStream;
    end;
  end;
end;

//
//  This procedure reads the next image in a GIF stream.
//
//  This procedure expects readToNextImage to have been called immediately
//  before this function.
//

procedure TGifDecoder.ReadNextImage(image: TOPBitmap);
var
    // Descriptor for the image being decoded.
  id: GifImageDescriptor;
    // Color table for the image being decode. Either the local color table
    // or a copy of the global color table.
  isinterlaced: boolean;

  ct: GifColorTable;
  colorcount: cardinal; // Size of the color table.
  count: Cardinal;
  ii: Integer;

  pass: Cardinal; // Interlace lace

  firstcharacter: UBYTE1; // The first character in the last code.

  rowptr, colptr: Cardinal; // Input output pointers.

    // mincodesize (read from the input stream) is the maximum number
    // of bit needed to represent literals in the input stream.
  mincodesize: Cardinal;
  codesize: Cardinal; // Current code size
  clearcode: Cardinal; // The code to reset the decompressor.
  endcode: Cardinal; // The code to mark the end of the compressed stream.
  nextcode: Cardinal; // The next available dynamic code.

    // The LZW dictionary.
  Dictionary: array[0..(1 shl GifMaxBitsPerCode) - 1] of DictionaryTree;
  ICT:TColorTableArray;

  // This nexted function writes a data value to the output image.
  procedure OutputToImage(data: UBYTE1);
    // All of these constants are used to support interlaced files.
  const
    maxpasses = 4;
      // We have one extra entry here for debugging. If
    passstart: array[0..maxpasses] of Cardinal = (0, 4, 2, 1, High(Cardinal));
    passcount: array[0..maxpasses - 1] of Cardinal = (8, 8, 4, 2);
    passrows: array[0..maxpasses - 1] of Cardinal = (8, 4, 2, 1);
  var
    offset: cardinal;
    limit: cardinal;
    ii: cardinal;
   // pix: PPixel32;
   continue:Boolean;
  begin
  Continue:=True;
    if rowptr > image.height then //theo > inst >=
    begin
     //ShowMessage(inttostr(rowptr)+' '+inttostr(image.height));
     //exit;
        Continue:=False;     //theo
       // writeln('ROWP: ',rowptr,' ',image.height);
      //raise EGifError.Create('GIF: Corrupt input stream. Compressed data past image.');
    end;
    if Continue then
    if isinterlaced then
    begin
      limit := image.width * image.height; // Mark the end of the image.

      offset := rowptr * image.width + colptr; // Locate the current pixel.
      ii := 0;
      while (ii < passrows[pass]) and (offset < limit) do
      begin // Here we fillin the pixel and all the pixels between the
              // interlaced rows.
       //theo
        TBitmapData8(image.Data).RawArray^[offset]:=data;
        inc(ii);
        inc(offset, image.width); // Advance to the next row.
      end;
      inc(colptr);
      if colptr = image.width then
      begin
        colptr := 0;
        inc(rowptr, passcount[pass]);
        callProgressFunction(pass + 1, maxpasses, (100 * rowptr) div image.height);
        if (rowptr >= image.height) then
        begin
          inc(pass);

          // Handle something stupid like a one-pixel high interlaced image.
          while (passstart[pass] >= image.height) and (pass < maxpasses) do
            inc(pass);
          rowptr := passstart[pass];
        end;
      end;
    end
    else
    begin // Noninterlaced file.
      offset := rowptr * image.width + colptr;
      TBitmapData8(image.Data).RawArray^[offset]:=data;  //if transparent_flag and (data = transparent_color) then
      inc(colptr);
      if colptr = image.width then
      begin
        inc(rowptr);
        callProgressFunction(1, 1, (100 * rowptr) div image.height);
        colptr := 0;
      end;
    end;
  end;

  // This function outputs the data for a single code in the
  // the compressed stream.
  procedure Outputcode(code: Cardinal);
  var
    SP: Cardinal;
    Stack: array[1..(1 shl GifMaxBitsPerCode)] of UBYTE1;
    lastcode: Cardinal;
  begin

    // Push the values on the stack in reverse order.
    sp := 0;
    repeat
      begin
        inc(sp);
        Stack[sp] := dictionary[code].data;
        lastcode := code;
        code := dictionary[code].parent;
      end
    until lastcode < clearcode;

    firstcharacter := stack[sp];

    // Pop the values form the stack in the correct order.
    while sp > 0 do
    begin
      OutputToImage(stack[sp]);
      dec(sp);
    end;
  end;

  // This nested function initializes the decompressor. It gets
  // called at the start of decompression and whenever the input
  // stream contains a CLEAR CODE.
  procedure Initialize;
  var
    ii: Integer;
    limit: integer;
  begin
    // Initialize the LZW Dictionary
    Limit := (1 shl mincodesize) - 1;
    for ii := 0 to Limit do
    begin
      Dictionary[ii].data := ii;
      Dictionary[ii].parent := 0;
    end;
    for II := Limit + 1 to High(Dictionary) do
    begin
      // Load with junk to aid debugging.
      Dictionary[ii].data := $FF;
      Dictionary[ii].parent := $FFFF;
    end;

    // Generate values based upon the code size
    codesize := mincodesize + 1; // Initial code size
    clearcode := 1 shl mincodesize; // Reset decompressor code
    endcode := clearcode + 1; // End of compressed stream code
    nextcode := endcode + 1; // Next free code
  end;

  // This nested function decompresses the compressed GIF image data.
  procedure Decompress;
  var
    Code, lastcode: Cardinal;
  begin
    // Get the first code in the stream. A clear code can come anywhere and
    // some compressors like to put them at the start of the stream.
    code := input_stream.nextCode(codesize);
    while code = clearcode do
      code := input_stream.nextCode(codesize);
    OutputCode(code);

    while True do
    begin
      lastcode := code;
      // See if we need to increase the code size.
      if (nextcode >= (1 shl codesize)) and (codesize < GifMaxBitsPerCode) then
        inc(codesize);

      code := input_stream.nextCode(codesize);
      if code = endcode then
      begin
        Exit; // Endcode => all done.
      end
      else if Code = clearcode then
      begin
        // Clearcode => Reset the compressor and start over.
        Initialize;
        code := input_stream.nextCode(codesize);
        while code = clearcode do
          code := input_stream.nextCode(codesize);
        if code = endcode then
          Exit;
        OutputCode(code);
      end
      else if Code < nextcode then
      begin
        // A code that has already been defined.
        OutputCode(code);
        dictionary[nextcode].parent := lastcode;
        dictionary[nextcode].data := firstcharacter;
        inc(nextcode);
      end
      else
      begin
        // Special Case of undefined code.
        dictionary[nextcode].parent := lastcode;
        dictionary[nextcode].data := firstcharacter;
        inc(nextcode);
        OutputCode(code);
      end;
    end;
  end;

  procedure UseGlobalColorTable;
  var
    ii: Cardinal;
  begin
    for II := Low(global_color_table) to global_color_count - 1 do
      ct[ii] := global_color_table[ii];
  end;

begin // ReadNextImage

  if not Assigned(input_stream) then
    raise EGifError.Create('GIF: No input stream');

  if not more_images then
    raise EGifError.Create('GIF: No more images in input stream.');

  if input_stream.read(id, sizeof(id)) <> sizeof(id) then
    raise EGifError.Create('Error reading GIF image descriptor');
  if verbose_flag then
    Print(id);
  fGifImageDescriptor := id; //theo

  // Save the image position so the user can retrieve it later to position
  // the image on the logical screen.
  image_position_top := id.top_position;
  image_position_left := id.left_position;

  // Interlaced handling.
  isinterlaced := InterLaceFlag(id);
  pass := 0;

  // Determine if this image uses the local color table.
  if LocalColorTableFlag(id) and (ColorTableSize(id) <> 0) then
  begin
    // Use a local color table.

    colorcount := 1 shl (ColorTableSize(Id) + 1);
    input_stream.read(ct[0], sizeof(ct[0]) * colorcount);

    for ii := 0 to colorcount - 1 do
    begin
      Count := input_stream.read(ct[ii], sizeof(ct[ii]));
      if count <> sizeof(ct[ii]) then
        raise EGifError.Create('Error reading local color table in GIF file.');
    end;
  end
  else
  begin
    // No local color table.

    // If there is no global color table as well then we're in trouble.
    if global_color_count = 0 then
      raise EGifError.Create('GIF image contains no color table');
    UseGlobalColorTable;
  end;
  // Allocate the image buffer.
  //theo
//  image.setSize (id.image_width, id.image_height) ;

  image.Width:=0;
  image.PixelFormat:=pf8bit;
  image.Width := id.image_width;
  image.Height := id.image_height;
  for ii:= 0 to 255 do ICT[ii]:=ct[ii].Red shl 16 + ct[ii].Green shl 8 + ct[ii].Blue;
  Image.CopyFromColorTable(ICT, true );


  // Initialize the output position in the image.
  rowptr := 0;
  colptr := 0;

  // The minimum code size comes before the compressed data.
  mincodesize := input_stream.getByte;

  // Read the comrpessed data.
  Initialize;
  input_stream.EnterBitMode;
  Decompress;
  input_stream.ExitBitMode;

  // Get in a position where the user can read the next image in the stream.
  if MultiImageMode then
    ReadToNextImage;

end; // ReadNextImage ;

procedure TGifDecoder.ReadToNextImage;
var
  blocktype: Cardinal;
  controllabel: Cardinal;
  size: Cardinal;
  graphiccontrolbblock: GifGraphicControlBlock;
  applicationblock: GifApplicationBlock;
  textblock: GifPlainTextBlock;
  buffer: array[0..255] of char;
begin

  while input_stream.MoreData do
  begin
    blocktype := input_stream.getByte;
    case blocktype of
      GifTrailer:
        begin
          more_images := false;
          Exit;
        end;
      GifImageSeparator:
        begin
          more_images := true;
          Exit;
        end;
      GifExtension:
        begin
          controllabel := input_stream.GetByte;
          case controllabel of
            GifPlainTextExtension:
              begin
                size := input_stream.read(textblock, sizeof(textblock));
                if size <> sizeof(textblock) then
                  raise EGifError.Create('GIF: Cannot read Plain Text Extension Block');
                if textblock.block_size <> (sizeof(textblock) - sizeof(textblock.block_size)) then
                  raise EGifError.Create('Corrupt GIF file - wrong size in GIF extension');
                if verbose_flag then
                  Print(textblock);
                size := input_stream.getByte;
                while (size <> 0) and input_stream.MoreData do
                begin
                  input_stream.read(buffer[0], size);
                  size := input_stream.getByte;
                end;
                if size <> 0 then
                  raise EGifError.Create('GIF: Cannot read Plaint Text data');
              end;
            GifGraphicControlExtension:
              begin
                input_stream.read(graphiccontrolbblock, sizeof(graphiccontrolbblock));
                if graphiccontrolbblock.block_size <> (sizeof(graphiccontrolbblock) - sizeof(graphiccontrolbblock.block_size)) then
                  raise EGifError.Create('Corrupt GIF file - wrong size in Graphic Control Block');
                if verbose_flag then
                begin
                  Print(graphiccontrolbblock);
                end;
                fGraphicCtrlBlock := graphiccontrolbblock; //theo
                blocktype := input_stream.GetByte;
                if blocktype <> 0 then
                  raise EGifError.Create('Corrupt GIF file - missing terminator');

                transparent_flag := TransparentColorFlag(graphiccontrolbblock);
                transparent_color := graphiccontrolbblock.transparent_color;
              end;
            GifCommentExtension:
              begin
                Size := input_stream.getByte;
                input_stream.read(buffer, size + 1); //theo +1
                if verbose_flag then
                begin
                  WriteLn('{ Comment Extension');
                  WriteLn(buffer);
                  WriteLn('}');
                end
              end;
            GifApplicationExtension:
              begin
                input_stream.read(applicationblock, sizeof(applicationblock));
                if applicationblock.block_size <> 11 then
                  raise EGifError.Create('Corrupt GIF file - invalid Application Extension Length');
                if verbose_flag then
                begin
                  WriteLn('{ Application Extension');
                  WriteLn('    Application Name: ', applicationblock.application_name);
                  WriteLn('    Application ID: ', applicationblock.authentication_code[0], ' ',
                    applicationblock.authentication_code[1], ' ',
                    applicationblock.authentication_code[2]);
                  WriteLn('}');
                end;
                size := input_stream.getByte;
                while (size <> 0) and input_stream.MoreData do
                begin
                  input_stream.read(buffer[0], size);
                  size := input_stream.getByte;
                end;
                if size <> 0 then
                  raise EGifError.Create('GIF: Cannot read Application Extension data');
              end;
          else
            raise EGifError.Create('GIF: Invalid GIF Extension');
          end;
        end;
    else
      raise EGifError.Create('Invalid GIF block');
    end;
  end;

  // Reached the end of stream without the GifTrailer marker.
  raise EGifError.Create('Missing GIF Trailer marker');
end; // ReadToNextImage


procedure TGifDecoder.SetMultiImageMode(state: Boolean);
begin
  if Assigned(Input_stream) then
    raise EGifError.Create('GIF: Cannot change multi-image mode with active input stream.');

  multi_image_mode := state;
end;

function TGifDecoder.ActiveInputStream: Boolean;
begin
  Result := Assigned(input_stream);
end;

procedure TGifDecoder.CloseInputSTream;
begin
  if not Assigned(input_stream) then
    Exit;

  try
    input_stream.destroy;
  finally
    input_stream := nil;
  end;
end;

procedure TGifDecoder.callProgressFunction(pass, passcount: Cardinal; progress: Cardinal);
var
  abort: Boolean;
  percent: Cardinal;
begin
  if (not Assigned(progress_function)) then
    Exit;

  abort := false;
  if (progress > 100) then
    percent := 100
  else
    percent := progress;

  progress_function(Self,
    progress_data,
    pass,
    passcount,
    'GIF Decode',
    percent,
    abort);
  if (abort) then
    raise EGraphicsAbort.Create('');
end;


end.
