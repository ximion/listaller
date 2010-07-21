program project1;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, glib2;

{$R project1.res}

begin
  {$ifndef LCLGTK2}
   g_type_init();
  {$endif}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

