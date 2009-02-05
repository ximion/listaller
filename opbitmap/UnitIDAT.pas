unit UnitIDAT;

interface

uses
  Classes,
  UnitBloc,
  pnzlib2, pnzdeflate, pnzutil;

type

  TBlocIDAT = class(TBloc)
  private
    fUnused : array of byte; // uncompressed reminding datas
    fMyStream : z_stream;
    procedure CompressOne(var OnePart : array of byte; Dernier : boolean); // compress on part <=32768
  public
    constructor Create(Stream : TStream);
    destructor Destroy; override;
//    constructor Create(var Sequence : array of byte);
    procedure AddUncompressed(var Sequence : array of byte);
  end;

implementation

constructor TBlocIDAT.Create(Stream : TStream);
var
  err : integer;
begin
  fBlocType[0]:=ord('I');
  fBlocType[1]:=ord('D');
  fBlocType[2]:=ord('A');
  fBlocType[3]:=ord('T');
  inherited Create(Stream);

  // initialization of compression LGZIP
  fMyStream.total_in:=0;
  fMyStream.total_out:=0;
  fMyStream.zalloc:=nil;
  fMyStream.zfree:=nil;
  fMyStream.opaque:=nil;
  err := deflateInit(fMyStream, 9);
  if (err<>Z_OK) then
      exit;
end;

destructor TBlocIDAT.Destroy;
begin
  // store the last datas (size <32768)
  CompressOne(fUnused, True);
  inherited Destroy;
end;

procedure TBlocIDAT.CompressOne(var OnePart : array of byte; Dernier : boolean);
// compress on part of size <=32768 bytes
var
  Compressed : array of byte;
  Start_out : integer;
  err : integer;
begin
  // compression LGZIP

  fMyStream.next_in:=pBytef(@OnePart[0]);
  fMyStream.avail_in:=high(OnePart)+1;

  SetLength(Compressed,32813);
  fMyStream.next_out:=pBytef(@Compressed[0]);
  fMyStream.avail_out:=32813;
  Start_out:=fMyStream.total_out;

  if not Dernier then
  begin
// Z_NO_FLUSH donne des résultats légèrement meilleurs (0.2%) sur une zone homogène
// mais pourrait poser problème sur de grandes zones.
    err := deflate (fMyStream, Z_PARTIAL_FLUSH);
    if (err<>Z_OK) then
        exit;
  end
  else begin
    err := deflate (fMyStream, Z_FINISH);
    if (err<>Z_STREAM_END) then
        exit;
  end;
  SetLength(Compressed, fMyStream.total_out - Start_out);
  AddSequence(Compressed);
end;

procedure TBlocIDAT.AddUncompressed(var Sequence : array of byte);
var
  I, TailleSource, TailleUnused : integer;
  SmallPart : array of byte;
begin
  TailleUnused:=high(fUnused)+1;
  TailleSource:=TailleUnused+(high(Sequence)+1);
  if TailleSource<32768 then
  begin // still less than 32768, we add the new part to the unused one
    SetLength(fUnused,TailleSource);
    move(Sequence[0],fUnused[TailleUnused],high(Sequence)+1);
  end
  else begin
    // first part is made of fUnused and beginning of Sequence
    SetLength(SmallPart,32768);
    if TailleUnused>0 then
      move(fUnused[0],SmallPart[0],TailleUnused);
    move(Sequence[0],SmallPart[TailleUnused],32768-TailleUnused);
    CompressOne(SmallPart,false);
    // next parts are made of bulk sequence
    for I:=1 to TailleSource div 32768 -1 do
    begin
      move(Sequence[I*32768-TailleUnused],SmallPart[0],32768);
      CompressOne(SmallPart,false);
    end;
    // store the reminding part
    I:=TailleSource div 32768 ;
    SetLength(fUnused,TailleSource-I*32768);
    move(Sequence[I*32768-TailleUnused],fUnused[0],TailleSource-I*32768);
  end;
end;

end.
