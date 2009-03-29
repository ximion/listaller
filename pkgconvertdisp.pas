{
 * pkgconvertdisp.pas
 * Copyright (C) Listaller Project 2008
 *
 * pkgconvertdisp.pas is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pkgconvertdisp.pas is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit provides a form that shows the output of 'alien'
unit pkgconvertdisp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  process, ExtCtrls, utilities, LCLType, manager;

type

  { TConvDisp }

  TConvDisp = class(TForm)
    GetOutPutTimer: TIdleTimer;
    Label1: TLabel;
    Memo1: TMemo;
    Process1: TProcess;
    procedure FormCreate(Sender: TObject);
    procedure GetOutPutTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ConvDisp: TConvDisp;

implementation

{ TConvDisp }

procedure TConvDisp.GetOutPutTimerTimer(Sender: TObject);
  var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess(Process: TProcess;
    OutputMemo: TMemo);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
  begin

    if Process.Running then
    begin

      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
       begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer,1, BytesRead);
        Application.ProcessMessages;
        BytesAvailable := Process.OutPut.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      if BytesRead>0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;

  end;
begin
  repeat
    NoMoreOutput := true;
    Application.ProcessMessages;

    DoStuffForProcess(Process1, Memo1);
  until noMoreOutput;
if Process1.ExitStatus>0 then begin
    GetOutputTimer.Enabled:=false;
    ShowMessage('Error converting package!');
    Memo1.Lines.SaveTofile(ConfigDir+'messages.log');
    exit;
  end;
  if not Process1.Running then begin
  GetOutPutTimer.Enabled:=false;
  if Application.MessageBox('Converting done. Do you want to close this window now?'#13'Press "No" if you want to check the output.','Close?',MB_YESNO)=IDYES then begin
  close;
  Application.ProcessMessages;
  mnFrm.LoadEntries;
  end;end;
end;

procedure TConvDisp.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I pkgconvertdisp.lrs}

end.

