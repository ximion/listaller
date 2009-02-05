unit UnittRNS;

interface

uses
  Classes,
  UnitBloc;

type

  TBloctRNS = class(TBloc)
  private
  public
    constructor Create(Stream : TStream; Palette : array of byte); overload; // color model 3
    constructor Create(Stream : TStream; GreyLevel : byte); overload; // color model 0
    constructor Create(Stream : TStream; Couleur : TRGB); overload; // color model 2
  end;

implementation

constructor TBloctRNS.Create(Stream : TStream; Palette : array of byte); // color model 3
var
  I : integer;
  fContenu : array of byte;
begin
  fBlocType[0]:=ord('t');
  fBlocType[1]:=ord('R');
  fBlocType[2]:=ord('N');
  fBlocType[3]:=ord('S');
  inherited Create(Stream);
  SetLength(fContenu,high(Palette)+1);
  for I:=low(Palette) to high(Palette) do
    fContenu[I]:=Palette[I];
  AddSequence(fContenu);
end;

constructor TBloctRNS.Create(Stream : TStream; GreyLevel : byte); // color model 0
var
  fContenu : array of byte;
begin
  fBlocType[0]:=ord('t');
  fBlocType[1]:=ord('R');
  fBlocType[2]:=ord('N');
  fBlocType[3]:=ord('S');
  inherited Create(Stream);
  SetLength(fContenu,2);
  fContenu[0]:=0;
  fContenu[1]:=GreyLevel;
  AddSequence(fContenu);
end;

constructor TBloctRNS.Create(Stream : TStream; Couleur : TRGB); // color model 2
var
  fContenu : array of byte;
begin
  fBlocType[0]:=ord('t');
  fBlocType[1]:=ord('R');
  fBlocType[2]:=ord('N');
  fBlocType[3]:=ord('S');
  inherited Create(Stream);
  SetLength(fContenu,6);
  fContenu[0]:=0;
  fContenu[1]:=Couleur.Red;
  fContenu[2]:=0;
  fContenu[3]:=Couleur.Green;
  fContenu[4]:=0;
  fContenu[5]:=Couleur.Blue;
  AddSequence(fContenu);
end;

end.
