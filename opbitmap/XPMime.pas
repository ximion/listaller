{ *************************************************************************** }
{ Copyright (c) 2003 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

//By far not perfect to read all Magic of freedesktop.org.xml but does the job for now.

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

unit XPMime;

interface

uses Classes, xmlread, dom, Sysutils, Contnrs {, Dialogs};

const
  MIME_JPG = 'image/jpeg';
  MIME_PNG = 'image/png';
  MIME_GIF = 'image/gif';
  MIME_BMP = 'image/bmp';
  MIME_TIF = 'image/tiff';
  MIME_PCX = 'image/x-pcx';
  MIME_PCD = 'image/x-photo-cd';
  MIME_PBM = 'image/x-portable-bitmap';
  MIME_PGM = 'image/x-portable-graymap';
  MIME_PPM = 'image/x-portable-pixmap';
  MIME_PSD = 'image/x-psd';
  MIME_PSP = 'image/x-psp';
  MIME_CUT = 'image/x-cut';
  MIME_RGB = 'image/x-rgb';
  MIME_SGI = 'image/sgi';
  MIME_OPB = 'image/opb';
  MIME_TGA = 'image/x-tga';

type TMagicType = (mtString, mtBig16, mtBig32, mtLittle16, mtLittle32, mtHost16, mtHost32, mtByte);


type TMagicObject = class
    MagicType: TMagicType;
    Offset: string;
    Value: string;
    Mask: string;
    CompareLength: integer;
    MagicList: TList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TMimeObject = class
  private

  public
    MimeType: string;
    StandardName: string;
    UserLangName: WideString;
    PatternList: TStrings;
    MagicList: TList;
  end;

  { TXPMime }

  TXPMime = class
  private
    fLang: string;
  protected
    fXML: TXMLDocument;
    fMimeList: TList;
    procedure ReadMimeInfos;
    procedure ReadMimeInfo(Node: TDomNode);
    procedure DebugList;
  public
    constructor Create(MimeFilePath: WideString; Lang: string);
    destructor Destroy; override;
    function GetMimeInfoFromPattern(Pattern: string): TMimeObject;
    function GetMagicMimeInfo(Stream: TStream; FileName: string = ''): TMimeObject;
    function MakeLoadFilter: string;
    function MakeSaveFilter: string;
    property Lang:String read fLang;
  published
  end;

var MimeMagic: TXPMime;

implementation
uses StrUtils, uMimeBytes, mimexml;

function ConvertMagicType(MType: string): TMagicType;
begin
  Result := mtString;
  if MType = 'string' then Result := mtString else
    if MType = 'byte' then Result := mtByte else
      if MType = 'big16' then Result := mtBig16 else
        if MType = 'big32' then Result := mtBig32 else
          if MType = 'little16' then Result := mtLittle16 else
            if MType = 'little32' then Result := mtLittle32 else
              if MType = 'host16' then Result := mtHost16 else
                if MType = 'host32' then Result := mtHost32;
end;

constructor TMagicObject.Create;
begin
  MagicList := TList.create;
end;

destructor TMagicObject.Destroy;
var i:integer;
begin
  if MagicList.Count>0 then for i:=0 to MagicList.count-1 do
  begin
   TMagicObject(MagicList[i]).free;
  end;
  MagicList.free;
  inherited;
end;

procedure StringToMemStream(AString: AnsiString; Strm: TMemoryStream);
var Len: integer;
begin
  if Strm <> nil then
  begin
    Len := Length(AString);
    Strm.Size := Len;
    Strm.Position := 0;
    Strm.Write(PChar(AString)^, Len);
    Strm.Position := 0;
  end;
end;


constructor TXPMime.Create(MimeFilePath: WideString; Lang: string);
var Strm: TMemoryStream;
begin
  fLang := Lang;
  Strm:=TMemoryStream.create;
  StringToMemStream(MimeXMLDat, Strm);
  ReadXMLFile(fXML,Strm);
  Strm.free;
  fMimeList := TList.create;
  ReadMimeInfos;
  MimeMagic := Self;
end;

destructor TXPMime.Destroy;
var i,ui:integer;
begin
  for i:=0 to fMimeList.count-1 do
  begin
   For ui:=0 to TMimeObject(fMimeList[i]).MagicList.count-1 do
    TMagicObject(TMimeObject(fMimeList[i]).MagicList[ui]).free;
    TMimeObject(fMimeList[i]).MagicList.Free;
    TMimeObject(fMimeList[i]).PatternList.free;;
    TMimeObject(fMimeList[i]).free;
  end;
  fMimeList.free;
  fXML.free;
  inherited;
end;

procedure TXPMime.ReadMimeInfo(Node: TDomNode);
var MagicNode: TDomNode;
  TypeValue, TempStr: string;
  i, nodenumber, CurrentNodeCount: integer;
  Obj: TMimeObject;
  found: boolean;

  mlevel: integer;
  //inner
  procedure GetMagic(Node: TDomNode; ParentList: TList);
  var mi, MagicCount: integer;
    Mo: TMagicObject;
    CurrentAttr: TDOMAttr;
    SubNode: TDomNode;
  begin
    MagicCount := Node.ChildNodes.Count;
    for mi := 0 to MagicCount - 1 do
    begin
      Subnode := TDomNode(Node.ChildNodes[mi]);
      Mo := TMagicObject.create;
      CurrentAttr := TDomAttr(Subnode.Attributes.GetNamedItem('type'));
      if CurrentAttr <> nil then Mo.MagicType := ConvertMagicType(CurrentAttr.Value);
      CurrentAttr := TDomAttr(Subnode.Attributes.GetNamedItem('value'));
      if CurrentAttr <> nil then Mo.Value := CurrentAttr.Value;
      CurrentAttr := TDomAttr(Subnode.Attributes.GetNamedItem('offset'));
      if CurrentAttr <> nil then Mo.Offset := CurrentAttr.Value;
      CurrentAttr := TDomAttr(Subnode.Attributes.GetNamedItem('mask'));
      if CurrentAttr <> nil then Mo.Mask := CurrentAttr.Value;
      ParentList.add(Mo);
      if Subnode.hasChildNodes then
      begin
        mlevel := mlevel + 1;
        GetMagic(Subnode, Mo.MagicList);
        mlevel := mlevel - 1;
      end;
    end;
  end;

begin
  Obj := TMimeObject.Create;
  Obj.PatternList := TStringList.Create;
  Obj.MagicList := TList.Create;

  Obj.MimeType := Node.Attributes.GetNamedItem('type').NodeValue;
  CurrentNodeCount := Node.ChildNodes.count;
  nodenumber := 0;

  if CurrentNodeCount > 0 then
  begin
    Obj.StandardName := TDomNode(Node.ChildNodes[nodenumber]).TextContent;
    Obj.UserLangName := Obj.StandardName;
  end;
  nodenumber := 1;

  found := false;
  while (CurrentNodeCount > nodenumber) and (TDomNode(Node.ChildNodes[nodenumber]).NodeName = 'comment') do
  begin
    if not found then
    begin
      TempStr := TDomNode(Node.ChildNodes[nodenumber]).Attributes.GetNamedItem('xml:lang').NodeValue;
      if TempStr = fLang then
      begin
       // Obj.UserLangName := TDomNode(Node.ChildNodes[nodenumber]).Nodevalue;
        Obj.UserLangName := TDomNode(Node.ChildNodes[nodenumber]).TextContent;
        //Obj.UserLangName := UTF8Decode(Obj.UserLangName);
        found := true;
      end;
    end;
    inc(nodenumber);
  end;

  if (CurrentNodeCount > nodenumber) and (TDomNode(Node.ChildNodes[nodenumber]).NodeName = 'magic') then
  begin
    MagicNode := TDomNode(Node.ChildNodes[nodenumber]);
    mlevel := 0;
    GetMagic(MagicNode, Obj.MagicList);

    inc(nodenumber);
  end;
  while (CurrentNodeCount > nodenumber) and (TDomNode(Node.ChildNodes[nodenumber]).NodeName = 'glob') do
  begin
    TempStr := TDomNode(Node.ChildNodes[nodenumber]).Attributes.GetNamedItem('pattern').NodeValue;
    Obj.PatternList.Add(TempStr);
    inc(nodenumber);
  end;

  fMimeList.Add(Obj);
end;

procedure TXPMime.ReadMimeInfos;
var MimeNode: TDomNode;
  i: integer;
begin
  fMimeList.Clear;
  MimeNode := fXML.FindNode('mime-info');
  for i := 0 to MimeNode.ChildNodes.count - 1 do  //theo debug
    ReadMimeInfo(TDomNode(MimeNode.ChildNodes[i]));
//  DebugList;
end;

function TXPMime.GetMimeInfoFromPattern(Pattern: string): TMimeObject;
var i, ui: integer;
var found: boolean;
begin
  Result := nil;
  found := false;
  for i := 0 to fMimeList.count - 1 do
  begin
    for ui := 0 to TMimeObject(fMimeList[i]).PatternList.Count - 1 do
      if Pattern = TMimeObject(fMimeList[i]).PatternList[ui] then
      begin
        Result := TMimeObject(fMimeList[i]);
        found := true;
        break;
      end;
    if found then break;
  end;
end;

procedure TXPMime.DebugList;
var i, ui: integer;
begin
{  Form1.Memo1.Lines.BeginUpdate;

  for i := 0 to fMimeList.Count - 1 do
  begin
    Form1.Memo1.Lines.add(TMimeObject(fMimeList[i]).MimeType + ' : ' +
      TMimeObject(fMimeList[i]).StandardName + ' : ' +
      TMimeObject(fMimeList[i]).UserLangName);
    for ui := 0 to TMimeObject(fMimeList[i]).PatternList.count - 1 do
      Form1.Memo1.Lines.add(TMimeObject(fMimeList[i]).PatternList[ui]);

  end;
  Form1.Memo1.Lines.EndUpdate;}
end;



function TXPMime.GetMagicMimeInfo(Stream: TStream; FileName: string): TMimeObject;
type TByteRec = record
    LBLW, HBLW, LBHW, HBHW: byte;
  end;
var
  fs: TFileStream;
  Buffer: TByteArr;
  tempCard: Cardinal;
  i, ui, cnt: integer;

  mo: TMagicObject;
  found: Boolean;

//inner
  function CheckMagic(Mo: TMagicObject): Boolean;
  var s, e: integer;
    FileID, MagicString, TempStr: string;
    MagicArr, MaskArray, MaskedBuffer, MaskedMagic: TByteArr;
    MagicLen: integer;
  begin
    result := false;

    GetOffset(mo.Offset, s, e);
    if mo.MagicType = mtString then
    begin
      MagicLen := DecodeText(HTMLDecodeMagic(mo.Value), magicArr);
    end;

    if (mo.MagicType = mtBig32) or (mo.MagicType = mtHost16) then
    begin

      MagicLen := HexStringToByteArr(mo.Value, MagicArr);
    end;

    if mo.MagicType in [mtBig32, mtString, mtHost16] then
    begin
      if mo.Mask <> '' then
      begin
        MagicLen := HexStringToByteArr(mo.Mask, MaskArray);
        MaskedBuffer := MaskArr(Buffer, MaskArray, MagicLen);
        MaskedMagic := MaskArr(MagicArr, MaskArray, MagicLen);
        result := CompareOffsetByteArr(MaskedBuffer, MaskedMagic, MagicLen, s);
      end else
        if e = 0 then
          result := CompareOffsetByteArr(Buffer, MagicArr, MagicLen, s)
        else
          result := CompareByteArrRange(Buffer, MagicArr, MagicLen, s, e);
    end;



  end;

//inner
  function AnalyzeItem(Item: TMagicObject): boolean;
  var i, count: integer;
    itemvalue: Boolean;
  begin
    result := false;
    count := Item.MagicList.count;
    itemvalue := CheckMagic(Item);
// itemvalue:=false;
    if count = 0 then Result := Itemvalue else
    begin
      if itemvalue then
        for i := 0 to count - 1 do
        begin
        //recursion
          if AnalyzeItem(TMagicObject(Item.MagicList[i])) then
          begin
            Result := true;
            break;
          end;
        end
      else result := false;
    end;
  end;


begin
  Result := nil;

  if FileName <> '' then
  begin
    fs := TFileStream.create(FileName, fmOpenRead, fmShareDenyNone);
    try
      fs.position := 0;
      fs.Read(Buffer, MimeBufferSize);
    finally
      fs.free;
    end;
  end else
  begin
    if Stream <> nil then
    begin
      Stream.position := 0;
      Stream.Read(Buffer, MimeBufferSize);
    end else raise Exception.Create('Unassigned Stream');
  end;

  for i := 0 to fMimeList.Count - 1 do
  begin

    for ui := 0 to TMimeObject(fMimeList[i]).MagicList.Count - 1 do
    begin
      mo := TMagicObject(TMimeObject(fMimeList[i]).MagicList[ui]);
      if AnalyzeItem(mo) then
      begin
        result := TMimeObject(fMimeList[i]);
        break;
      end;
    end;


    if Result <> nil then break;
  end;

end;


function TXPMime.MakeLoadFilter: string;
var i, ui: integer;
begin
  if Lang='de' then
  Result := 'Unterstuetze Formate|' else   //Todo: many languages from resource
  Result := 'Supported Formats|';
  for i := 0 to fMimeList.Count - 1 do
  begin
    for ui := 0 to TMimeObject(fMimeList[i]).PatternList.count - 1 do
      Result := Result + TMimeObject(fMimeList[i]).PatternList[ui] + ';';
  end;

  for i := 0 to fMimeList.Count - 1 do
  begin
    Result := Result + '|' + TMimeObject(fMimeList[i]).UserLangName + '|';
    for ui := 0 to TMimeObject(fMimeList[i]).PatternList.count - 1 do
      Result := Result + TMimeObject(fMimeList[i]).PatternList[ui] + ';';
    Result := Copy(Result, 1, Length(Result) - 1);
  end;
  if Lang='de' then
  Result := Result + '|Alle Formate|*' else
  Result := Result + '|All Formats|*';
end;

function TXPMime.MakeSaveFilter: string;
var i, ui: integer;
  MimeTp: string;
  first: Boolean;
begin
  Result := '';
  first := true;
  for i := 0 to fMimeList.Count - 1 do
  begin
    MimeTp := TMimeObject(fMimeList[i]).MimeType;
    if (MimeTp = MIME_JPG) or (MimeTp = MIME_BMP) or (MimeTp = MIME_PNG) or (MimeTp = MIME_GIF) or
      (MimeTp = MIME_OPB) {$ifndef OpbCompat}or (MimeTp = MIME_TGA) or (MimeTp = MIME_TIF) {$endif} then
    begin
      if first then
        Result := TMimeObject(fMimeList[i]).UserLangName + '|' else
        Result := Result + '|' + TMimeObject(fMimeList[i]).UserLangName + '|';
      first := false;
      for ui := 0 to TMimeObject(fMimeList[i]).PatternList.count - 1 do
        Result := Result + TMimeObject(fMimeList[i]).PatternList[ui] + ';';
      Result := Copy(Result, 1, Length(Result) - 1);
    end;
  end;
end;

initialization

{$IFDEF LINUX}
  MimeMagic := TXPMime.create('', Copy(GetEnvironmentVariable('LANG'), 1, 2));
{$ELSE}
  MimeMagic := TXPMime.create('', 'en'); //todo: winlang
{$ENDIF}

finalization

  MimeMagic.free;

end.
