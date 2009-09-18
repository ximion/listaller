program LiPkgCreator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  editor, fwiz, prjwizard, LResources;

{$IFDEF WINDOWS}{$R licreator.rc}{$ENDIF}

begin
  {$I licreator.lrs}
  Application.Initialize;
  Application.CreateForm(TFrmEditor, FrmEditor);
  Application.CreateForm(TfrmFileWizard, frmFileWizard);
  Application.CreateForm(TfrmProjectWizard, frmProjectWizard);
  Application.Run;
end.

