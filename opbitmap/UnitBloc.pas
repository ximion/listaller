unit UnitBloc;

interface

uses

  Classes;

type

  TRGB = record
    Red, Green, Blue : byte;
  end;

  tBlocType = array[0..3] of byte;

  TBloc = class
  protected
    fBlocType : tBlocType;
    fCRC : longword;
    fTaille : integer;
    fTaillePosition : int64; // position in fStream where fTaille must be written
    fStream : TStream;
  public
    property BlocType : tBlocType read fBlocType;
    property Taille : integer read fTaille;
    property CRC : longword read fCRC;
    constructor Create(Stream : TStream); //virtual;
    destructor Destroy; override;
    procedure AddSequence(var Sequence : array of byte);
  end;

implementation

uses
  UnitCRC;

function Exchange (Value : longword) : longword;
begin
  result:=(Value and $FF) shl 24 +
          (Value and $FF00) shl 8 +
          (Value and $FF0000) shr 8 +
          (Value and $FF000000) shr 24;
end;

constructor TBloc.Create(Stream : TStream);
// fBlocType MUST be defined in child before calling TBloc.Create
begin
  inherited Create;
//  fContenuReady:=false;
  fStream:=Stream;
  fTaille:=0;
  fTaillePosition:=fStream.Position;
  Stream.Write(Taille,4);
  Stream.Write(BlocType[0],4);
  fCRC:=HeaderCRC(BlocType);
end;

destructor TBloc.Destroy;
var
  CurrentPosition : int64;
  XTaille, XCRC : longword;
begin
  inherited Destroy;
  if Taille<>0 then
  begin // back to the beginning of the block to write the size
    CurrentPosition:=fStream.Position;
    fStream.Position:=fTaillePosition;
    XTaille:=Exchange(Taille);
    fStream.Write(XTaille,4);
    fStream.Position:=CurrentPosition;
  end;
  XCRC:=Exchange(fCRC);
  fStream.Write(XCRC,4);
end;

procedure TBloc.AddSequence(var Sequence : array of byte);
var
  LaTaille : integer;
begin
  LaTaille:=high(Sequence)+1;
  inc(fTaille,LaTaille);
  if LaTaille>0 then
    fStream.Write(Sequence[0],LaTaille);
  IncrementCRC(fCRC, Sequence);
end;

end.
