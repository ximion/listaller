unit UnitPLTE;

interface

uses
  Classes,
  UnitBloc;

type

  TBlocPLTE = class(TBloc)
  private
  public
    constructor Create(Stream : TStream; Palette : array of TRGB);
  end;

implementation

constructor TBlocPLTE.Create(Stream : TStream; Palette : array of TRGB);
var
  I : integer;
  fContenu : array of byte;
begin
  fBlocType[0]:=ord('P');
  fBlocType[1]:=ord('L');
  fBlocType[2]:=ord('T');
  fBlocType[3]:=ord('E');
  inherited Create(Stream);
  SetLength(fContenu,(high(Palette)+1)*3);
  for I:=low(Palette) to high(Palette) do
  begin
    fContenu[3*I]:=Palette[I].Red;
    fContenu[3*I+1]:=Palette[I].Green;
    fContenu[3*I+2]:=Palette[I].Blue;
  end;
  AddSequence(fContenu);
end;

end.
