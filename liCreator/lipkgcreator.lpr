program LiPkgCreator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  editor, fwiz, prjwizard, LResources;

{$IFDEF WINDOWS}{$R lipkgcreator.rc}{$ENDIF}

begin
  {$I lipkgcreator.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.CreateForm(TfrmFileWizard, frmFileWizard);
  Application.CreateForm(TfrmProjectWizard, frmProjectWizard);
  Application.Run;
end.

