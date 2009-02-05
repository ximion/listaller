unit UnitIHDR;

interface

uses
  Classes,
  UnitBloc;

type

  TBlocIHDR = class(TBloc)
  private
  public
    constructor Create(Stream : TStream; Width, Height : longword; BitDepth, ColorType,
                       Compression, Filter, Interlace : byte);
  end;

implementation

constructor TBlocIHDR.Create(Stream : TStream; Width, Height : longword; BitDepth, ColorType,
                       Compression, Filter, Interlace : byte);
var
  fContenu : array of byte;
begin
  fBlocType[0]:=ord('I');
  fBlocType[1]:=ord('H');
  fBlocType[2]:=ord('D');
  fBlocType[3]:=ord('R');
  inherited Create(Stream);
  SetLength(fContenu,13);
  fContenu[0]:=(Width and $FF000000) shr 24;
  fContenu[1]:=(Width and $00FF0000) shr 16;
  fContenu[2]:=(Width and $0000FF00) shr 8;
  fContenu[3]:=(Width and $000000FF);
  fContenu[4]:=(Height and $FF000000) shr 24;
  fContenu[5]:=(Height and $00FF0000) shr 16;
  fContenu[6]:=(Height and $0000FF00) shr 8;
  fContenu[7]:=(Height and $000000FF);
  fContenu[8]:=BitDepth;
  fContenu[9]:=ColorType;
  fContenu[10]:=Compression;
  fContenu[11]:=Filter;
  fContenu[12]:=Interlace;
  AddSequence(fContenu);
end;

end.
