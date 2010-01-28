unit loadfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, GIFAnimator, LiBasic;

type

  { TLoadDisp }

  TLoadDisp = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    gif: TGifThread;
  public
    { public declarations }
  end; 

var
  LoadDisp: TLoadDisp;

implementation

{ TLoadDisp }

procedure TLoadDisp.FormCreate(Sender: TObject);
begin

end;

procedure TLoadDisp.FormDestroy(Sender: TObject);
begin

end;

initialization
  {$I loadfrm.lrs}

end.

