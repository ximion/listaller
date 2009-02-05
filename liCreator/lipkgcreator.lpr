program LiPkgCreator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, editor, fwiz, AbZipper, AbBase, AbArcTyp,
  AbZipKit, prjwizard;

begin
  Application.Initialize;
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.CreateForm(TfrmFileWizard, frmFileWizard);
  Application.CreateForm(TfrmProjectWizard, frmProjectWizard);
  Application.Run;
end.

