unit giftype;


interface


uses
    Classes, Types, Sysutils, opbitmap{, gifpasbitmap};

// define a set of error messages



type EInvalidGraphic = class(Exception);
  EInvalidGraphicOperation = class(Exception);

const
    kGifErrorMessages:      array[0..24] of string = (
        'no error',                                                 // 0
        'Invalid GIF Signature Code',                               // 1
        'No Local or Global Color Table for Image',                 // 2
        'Unknown Graphics Extension Type',                          // 3
        'Unknown Graphics Operation Code',                          // 4
        'Invalid Extension Block Size',                             // 5
        '[special message]',                                        // 6
        'Invalid Extension Block Terminator',                       // 7
        'Invalid Integer Size',                                     // 8
        'No GIF Terminator Found',                                  // 9
        'Extension Block Out-Of-Order With Image Data',             // 10
        'Invalid Image Descriptor Index',                           // 11
        'Invalid LZW Code Size',                                    // 12
        'Invalid LZW Data Format',                                  // 13
        'LZW Code Overflow',                                        // 14
        'Value Out Of Range',                                       // 15
        'NIL Pointer assigned',                                     // 16
        'Invalid Color Table Size',                                 // 17
        'No Image Description',                                     // 18
        'Invalid Bitmap Image',                                     // 19
        'Invalid Color Table Index',                                // 20
        'Invalid Interlace Pass',                                   // 21
        'Invalid Bitmap',                                           // 22
        'Too Many Colors In Bitmap',                                // 23
        'next message'                                              //
    );

// GIF record separators

const
    kGifImageSeparator:     byte = $2c;
    kGifExtensionSeparator: byte = $21;
    kGifTerminator:         byte = $3b;
    kGifLabelGraphic:       byte = $f9;
    kGifLabelComment:       byte = $fe;
    kGifLabelText:          byte = $01;
    kGifLabelApplication:   byte = $ff;

// LZW encode table sizes

const
    kGifCodeTableSize = 4096;


// the parts of a GIF file
// yes, yes, I know ... I don't have to put in "type"
// before each record definition.  I just think it makes it
// easier to read, especially when the definitions may be broken
// across the printed page.  if you don't like it, take them out.


type
    PGifDataBlock           = ^TGifDataBlock;
    TGifDataBlock           = record            // generic data clump
        rSize:              integer;            // NOTE: array starts at "1"
        rData:              packed array[1..255] of byte;
        end;

type
    PGifSignature           = ^TgifSignature;
    TGifSignature           = record            // GIF87A or GIF89A
        rSignature:         packed array[1..6] of char;
        end;

type
    PGifExtensionGraphic    = ^TgifExtensionGraphic;
    TGifExtensionGraphic    = record            // graphic control extension
        rBlockSize:         integer;            // must always be 4
        rDisposal:          integer;            // disposal method when drawing
        rUserInputValid:    boolean;            // wait for user input?
        rTransparentValid:  boolean;            // transparent color given?
        rDelayTime:         integer;            // delay between display images
        rTransparentIndex:  integer;            // into color table
        end;

type
    PGifExtensionComment    = ^TgifExtensionComment;
    TGifExtensionComment    = record            // comment extension
        rDataBlockList:     TList;               // data blocks
        end;

type
    PGifExtensionText       = ^TGifExtensionText;
    TGifExtensionText       = record            // plain text extension
        rBlockSize:         integer;            // must always be 12
        rGridLeft:          integer;            // text grid position
        rGridTop:           integer;
        rGridWidth:         integer;            // text grid size
        rGridHeight:        integer;
        rCellWidth:         integer;            // size of a character cell
        rCellHeight:        integer;
        rForegroundIndex:   integer;            // text foreground color
        rBackgroundIndex:   integer;            // text background color
        rDataBlockList:     TList;              // data blocks
        end;

type
    PGifExtensionApplication    = ^TgifExtensionApplication;
    TGifExtensionApplication    = record        // application extension
        rBlockSize:         integer;            // must always be 11
        rIdentifier:        packed array[1..8] of char;
        rAuthentication:    packed array[1..3] of char;
        rDataBlockList:     TList;              // data blocks
        end;

type
    PGifExtension           = ^TGifExtension;
    TGifExtension           = record            // for any extension type
        case rLabel: byte of                    // cannot use CONST names
            $f9:        (rGraphic:  TGifExtensionGraphic);
            $fe:        (rComment:  TGifExtensionComment);
            $01:        (rText:     TGifExtensionText);
            $ff:        (rApp:      TGifExtensionApplication);
            $00:        (rDummy:    longint);
        end;

type
    PGifScreenDescriptor    = ^TGifScreenDescriptor;
    TGifScreenDescriptor    = record
        rWidth:             integer;            // size of logical screen
        rHeight:            integer;            // size of logical screen
        rGlobalColorValid:  boolean;            // global color table found in file?
        rColorResolution:   integer;            // bits per color
        rSorted:            boolean;            // global colors are sorted?
        rGlobalColorSize:   integer;            // size of global color table
        rBackgroundIndex:   integer;            // background color index
        rAspectRatio:       integer;            // pixel aspect ratio
        rGlobalColorTable:  integer;            // default color table for all images
        end;

type
    PGifColorTable          = ^TGifColorTable;  // pointer to a color table
    TGifColorTable          = record
        rSize:              integer;            // number of valid entries
        rColors:            array[0..255] of TColor;    // the colors
        end;

type
    PGifImageDescriptor     = ^TGifImageDescriptor;
    TGifImageDescriptor     = record
        rIndex:             integer;            // which image is this?
        rLeft:              integer;            // position of image
        rTop:               integer;            // position of image
        rWidth:             integer;            // size of image
        rHeight:            integer;            // size of image
        rLocalColorValid:   boolean;            // color table used?
        rInterlaced:        boolean;            // interlaced lines?
        rSorted:            boolean;            // color table sorted?
        rLocalColorSize:    integer;            // number entries in local color table
        rLocalColorTable:   integer;            // index into master list
        rLZWSize:           integer;            // LZW minimum code size
        rExtensionList:     TList;              // extensions read before this image
        rPixelList:         PChar;              // decoded pixel indices
        rPixelCount:        longint;            // number of pixels
        rBitmap:            TOPBitmap;            // the actual image
        end;


type
    PGifZip                 = ^TGifZip;
    TGifZip                 = record
        rID:                PGifImageDescriptor;    // image parameters to decode
        rCT:                PGifColorTable;         // color table for this image
        rPrefix:            array[0..kGifCodeTableSize-1] of integer;   // string prefixes
        rSuffix:            array[0..kGifCodeTableSize-1] of integer;   // string suffixes
        rCodeStack:         array[0..kGifCodeTableSize-1] of byte;      // decode/encoded pixels
        rSP:                integer;                // pointer into CodeStack
        rClearCode:         integer;                // reset decode params
        rEndCode:           integer;                // last code in input stream
        rHighCode:          integer;                // highest LZW code possible
        rCurSize:           integer;                // current code size
        rBitString:         integer;                // steady stream of bits to be decoded
        rBits:              integer;                // number of valid bits in BitString
        rCurSlot:           integer;                // next stack index to store a code
        rTopSlot:           integer;                // highest slot used so far
        rMaxVal:            boolean;                // max code value found?
        rCurX:              integer;                // position of next pixel
        rCurY:              integer;                // position of next pixel
        rCurPass:           integer;                // pixel line pass 1..4
        rFirstSlot:         integer;                // for encoding an image
        rNextSlot:          integer;                // for encoding
        rCount:             integer;                // number of bytes read/written
        rLast:              integer;                // last byte read in
        rUnget:             boolean;                // read a new byte, or use zLast?
    end;

type
    PGifDrawData            = ^TGifDrawData;
    TGifDrawData            = record
        rCanvas:            TCanvas;                // where to draw the image
        rRect:              TRect;                  // where on the canvas
//theo        rTimer:             TTimer;                 // for animation
        rImage:             integer;                // which image to draw
        rDelay:             integer;                // animation interval
        rCount:             integer;                // how many times to animate
        rProgressive:       boolean;                // display while decoding?
    end;

type
    PGifProgress            = ^TGifProgress;
    TGifProgress            = record
        rMax:               integer;                // max data to consume
        rPercent:           integer;                // current amount finished
        rValue:             integer;                // actual value
        rString:            string;                 // what it is doing
    end;



{ ---------------------------------------------------------------------------- }

procedure GIF_Error(n: integer);
procedure GIF_ErrorMessage(m: string);



{ ---------------------------------------------------------------------------- }

var
    GIF_ErrorCode:      integer;                    // last error
    GIF_ErrorString:    string;                     // last error


implementation


{ ---------------------------------------------------------------------------- }
{ RAISE AN ERROR ------------------------------------------------------------- }

procedure GIF_Error(n: integer);
begin
GIF_ErrorCode   := n;
GIF_ErrorString := kGifErrorMessages[n];
raise EInvalidGraphicOperation.CreateFmt('TGif: %s', [GIF_ErrorString]);
end;


procedure GIF_ErrorMessage(m: string);
begin
GIF_ErrorCode := 6;
GIF_ErrorString := m;
raise EInvalidGraphicOperation.CreateFmt('TGif: %s', [GIF_ErrorString]);
end;

end.
