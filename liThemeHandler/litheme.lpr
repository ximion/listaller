program litheme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, thinstall, LResources;

{$IFDEF WINDOWS}{$R litheme.rc}{$ENDIF}

begin
  Application.Title:='Theme Handler';
  {$I litheme.lrs}  Application.CreateForm(TisFrm, isFrm);
  Application.Run;
end.

