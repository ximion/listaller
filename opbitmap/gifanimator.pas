{ *************************************************************************** }
{ Copyright (c) 2009 Theo Lustenberger, Matthias Klumpp                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ This unit is part of the Listaller project and                              }
{ released under LGPLv3-license                                               }
{ *************************************************************************** }
unit gifanimator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, opbitmapformats, gif, gifdecoder, lazbridge, ExtCtrls, Graphics,
  opbitmap;

type
  { TGifThread }

  TGifThread = Class(TThread)
    Private
    fCanvas: Graphics.TCanvas;
    fFileName: string;
    fImage: TCanvasOpBitmap;
    fWidth: integer;
    fHeight: integer;
    fLeft: integer;
    fTop: integer;
    fOnUpdate: TNotifyEvent;
    fDisposalMethod: integer;
    Procedure UpdateDims(Desc: GifImageDescriptor);
    Public
    Procedure Execute; override;
    Procedure Update;
    property Image: TCanvasOpBitmap read fImage;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    property FileName: string read fFileName write fFileName;
    property TrCanvas: Graphics.TCanvas read fCanvas write fCanvas;
    property GifDisposalMethod: integer read fDisposalMethod;
    property Left: integer read fLeft;
    property Top: integer read fTop;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    Procedure GifUpdate(Sender: TObject);
    procedure Initialize(cv: Graphics.TCanvas);
  End;

implementation

{ TGifThread }

Procedure TGifThread.UpdateDims(Desc: GifImageDescriptor);
Begin
  fLeft := Desc.left_position;
  fTop := Desc.top_position;
  fWidth := Desc.image_width;
  fHeight := Desc.image_height;
End;

Procedure TGifThread.Execute;

Var gif: TGifDecoder;
  repeatmode: boolean;
Begin
  Try
    Try
      repeatmode := false;
      fimage := TCanvasOpBitmap.Create;
      gif := TGifDecoder.Create;
      gif.Verbose := false;
      gif.MultiImageMode := true;
      Repeat
        gif.Reset; ;
        gif.ReadImageFile(FileName, image);
        UpdateDims(gif.GifImgDescriptor);
        fImage.Transparent := TransparentColorFlag(gif.GraphicCtrlBlock);
        fImage.TransparentColor := fImage.ColorTable^[gif.GraphicCtrlBlock.transparent_color];

        If Not Terminated Then Synchronize(@Update);
        If gif.moreImages And gif.MultiImageMode Then repeatmode := true;
        While gif.moreImages And gif.MultiImageMode And (Not Terminated) Do
          Begin
            sleep(gif.GraphicCtrlBlock.delay_time * 10);
            If Not Terminated Then gif.readNextImage(image);
            fDisposalMethod := DisposalMethod(gif.GraphicCtrlBlock);
            UpdateDims(gif.GifImgDescriptor);
            If Not Terminated Then Synchronize(@Update);
          End;
      Until (Not repeatmode) Or Terminated;
    Except
      Terminate;
    End;
  Finally
    image.Destroy;
    gif.Destroy;
  End;
End;

Procedure TGifThread.Update;
Begin
  If Assigned(fOnUpdate) Then OnUpdate(Self);
End;

Procedure TGifThread.GifUpdate(Sender: TObject);
Var Bmp: TBitmap;
Begin
  With TGifThread(Sender) Do
    Begin
      If GifDisposalMethod <> 1 Then TrCanvas.FillRect(0, 0, TrCanvas.Width - 1, TrCanvas.Height - 1);
      Bmp := TBitmap.create;
      AssignOpBitmapToBitmap(image, Bmp);
      Bmp.Transparent := Image.Transparent;
      TrCanvas.Draw(Left, Top, bmp);
      Bmp.free;
    End;
End;

procedure TGifThread.Initialize(cv: Graphics.TCanvas);
begin
OnUpdate:= @GifUpdate;
FreeOnTerminate := True;
TrCanvas := cv;
TrCanvas.Brush.color := clBtnFace;
Resume;
end;

end.

