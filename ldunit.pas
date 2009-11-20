unit ldunit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TLoadForm }

  TLoadForm = class(TForm)
    Label1: TLabel;
    PBar1: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  LoadForm: TLoadForm;

implementation

initialization
  {$I ldunit.lrs}

end.

