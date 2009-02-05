unit KDEBind; 
 
{$mode objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, dynlibs,
  StdCtrls, qt4;
  
  function GetKIcon(iname: String): TBitmap;
 
  type
  TGetKDEIcon = function (inm:Pchar):QBitmapH; cdecl;
 
var
  LibHandle: TLibHandle;
  GetKDEIcon:TGetKDEIcon;
 
  const   LIB_PATH = './libkbind.so';
 
implementation
 
{ TForm1 }
 
function GetKIcon(iname: String): TBitmap;
begin
  Result:=TBitmap(GetKDEIcon(Pchar(iname)));
end;
 
initialization
//  {$I kdebind.lrs}
 
  LibHandle:= LoadLibrary(LIB_PATH);
  GetKDEIcon:= TGetKDEIcon(GetProcAddress(LibHandle, 'Get_KDE_Icon'));
 
finalization
 
  if LibHandle <> NilHandle then
  begin
    FreeLibrary(LibHandle)
  end;
 
end.
