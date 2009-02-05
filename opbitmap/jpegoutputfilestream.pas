unit jpegoutputfilestream;
//
//  Title:  JpegOutputFileStream Class
//
//  Copyright 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    July 5, A.D. 2001
//
//  Version: 1
//
//  Description:
//
//    The TJpegOutputFileStream handles JPEG output using a file.
//

interface

Uses
  jpegoutputstream, classes ;

Type

  { TJpegOutputFileStream }

  TJpegOutputFileStream = Class (TJpegOutputStream)
    Private
      output_stream : TStream ;
      is_open : Boolean ;
      output_buffer : Array [0..$40000] Of Char ;
      fFileMode:Boolean;
      procedure SetOutputStream(const AValue: TStream);
    Protected
      Procedure flushBuffer ; Override ;
    Public
      Constructor Create ;
      Destructor Destroy ; Override ;
      Procedure Open (filename : String) ;
      Procedure Close ;
      Procedure CloseStream ;
      property Stream:TStream read output_stream write SetOutputStream;  //chd theo
    End ;

implementation

Uses
  sysutils ;

Constructor TJpegOutputFileStream.Create ;
  Begin
  Inherited Create ;
  is_open := false ;
  output_stream := Nil ;
  fFileMode:=True;
  End ;

Destructor TJpegOutputFileStream.Destroy ;
  Begin
  if fFileMode then Close else CloseStream;
  Inherited Destroy ;
  End ;

Procedure TJpegOutputFileStream.Open (filename : String) ;
  Begin
  fFileMode:=True;
  output_stream:= TFileStream.Create (filename, fmCreate Or fmShareDenyWrite) ;
  buffer_limit := @output_buffer [High (output_buffer)] ;
  current_byte := @output_buffer [Low (output_buffer)] ;
  End ;


Procedure TJpegOutputFileStream.Close ;
  Begin
  If Assigned (output_stream) Then
    Begin
    flushBuffer ;
    output_stream.Destroy ;
    output_stream := Nil ;
    End ;
  End ;

procedure TJpegOutputFileStream.CloseStream;
begin
  flushBuffer ;
end;

procedure TJpegOutputFileStream.SetOutputStream(const AValue: TStream);
begin
 fFileMode:=False;
 output_stream:=AValue;
 buffer_limit := @output_buffer [High (output_buffer)] ;
 current_byte := @output_buffer [Low (output_buffer)] ;
end;

//
//  Description:
//
//    This function writes the contents of the output buffer to
//    the output file.
//
Procedure TJpegOutputFileStream.flushBuffer ;
  Var
    count : Cardinal ;
  Begin
{$IFDEF FPC}
  count := Cardinal (current_byte) - Cardinal (@output_buffer) ;
{$ELSE}
  count := current_byte - output_buffer ;
{$ENDIF}
  output_stream.write (output_buffer [0], count) ;
  buffer_limit := @output_buffer [High (output_buffer)] ;
  current_byte := @output_buffer [Low (output_buffer)] ;
  End ;
End.
