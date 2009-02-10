unit RegisterLazOP;

{$mode objfpc}{$H+}

interface

uses
  LazOpBMP, {$ifndef OpbCompat} LazOpPCX, LazOpCUT, LazOpSGI, LazOpPCD, LazOpPSD, LazOpPSP, LazOpTGA, LazOpTIFF, {$endif}
  LazOpGif, LazOpJPEG, LazOpPNG;

implementation

end.

