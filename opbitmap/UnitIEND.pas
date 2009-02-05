unit UnitIEND;

interface

uses
  Classes,
  UnitBloc;

type


  TBlocIEND= class(TBloc)
  private
  public
    constructor Create(Stream : TStream);
  end;

implementation

constructor TBlocIEND.Create(Stream : TStream);
begin
  fBlocType[0]:=ord('I');
  fBlocType[1]:=ord('E');
  fBlocType[2]:=ord('N');
  fBlocType[3]:=ord('D');
  inherited Create(Stream);
end;

end.
