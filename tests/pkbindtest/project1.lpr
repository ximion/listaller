program project1;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, pkdesktop
  { you can add units after this };

{$R project1.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

