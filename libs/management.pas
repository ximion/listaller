unit management;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Ds, IniFiles, GetText, TRStrings, LiCommon,
  DB, FileUtil;

type
//** Different message types
MessageType = (mtInfo,mtWarning,mtError);

//** Container for information about apps
TAppInfo = record
 Name: PChar;
 ShortDesc: PChar;
 Version: PChar;
 Author: PChar;
 Icon: PChar;
 UId: PChar;
end;

//** Event to catch messages
TYMessageEvent = function(msg: String;ty: MessageType): Boolean; cdecl;
//** Event to catch thrown application records
TAppEvent = function(name: PChar;obj: TAppInfo): Boolean;cdecl;

//** Available groups
GroupType = (gtALL,gtEDUCATION,gtOFFICE,gtDEVELOPMENT,gtGRAPHIC,gtNETWORK,gtGAMES,gtSYSTEM,gtMULTIMEDIA,gtADDITIONAL,gtOTHER);

{** Process .desktop-file and add info to list @param fname Name of the .desktop file
      @param tp Category name}
//function ProcessDesktopFile(fname: String; tp: String): Boolean;

//** Load software list entries
function LoadEntries(group: GroupType): Boolean;
//** Register message callback
procedure RegisterMessage(me: TYMessageEvent);
//** Register app callback (thrown if new app was found)
procedure RegisterAppMessage(me: TAppEvent);

var Root: Boolean=false;

implementation

var FMsg: TYMessageEvent;
    FApp: TAppEvent;

procedure msg(s: String;t: MessageType);
begin
 if Assigned(FMsg) then FMsg(s,t);
end;

procedure newapp(s: String;oj: TAppInfo);
begin
 if Assigned(FApp) then FApp(PChar(s),oj);
end;

procedure RegisterMessage(me: TYMessageEvent);
begin
 FMsg:=me;
end;

procedure RegisterAppMessage(me: TAppEvent);
begin
 FApp:=me;
end;

function IsInList(nm: String;list: TStringList): Boolean;
begin
Result:=list.IndexOf(nm)>-1;
end;

function LoadEntries(group: GroupType): Boolean;
var ini: TIniFile;tmp,xtmp: TStringList;i,j,k: Integer;p,n: String;tp: String;
    entry: TAppInfo;
    dsApp: TSQLite3Dataset;
    blst: TStringList;

//Internal function to process desktop files
procedure ProcessDesktopFile(fname: String; tp: String);
var d: TIniFile;entry: TAppInfo;dt: TMOFile;lp: String;
    translate: Boolean; //Used, because Assigned(dt) throws an AV
//Translate string if possible/necessary
function ldt(s: String): String;
var h: String;
begin
 h:=s;
 try
 if translate then
 begin
  h:=dt.Translate(s);
  if h='' then h:=s;
 end;
 except
  Result:=h;
 end;
 Result:=s;
end;
begin
       d:=TIniFile.Create(fname);
       msg(rsLoading+'  '+ExtractFileName(fname),mtInfo);
       translate:=false;

       if (not Root)and(d.ReadString('Desktop Entry','Exec','')[1]<>'/')
       then
       else
       if (LowerCase(d.ReadString('Desktop Entry','NoDisplay','false'))<>'true')
       and (pos('yast',LowerCase(fname))<=0)
       and(LowerCase(d.ReadString('Desktop Entry','Hidden','false'))<>'true')
       and(not IsInList(d.ReadString('Desktop Entry','Name',''),blst))
       and((pos(tp,LowerCase(d.ReadString('Desktop Entry','Categories','')))>0)or(tp='all'))
      // and(pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('core',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('.hidden',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('base',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','X-KDE-ParentApp','#')='#')
       and(pos('screensaver',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('setting',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('utility',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','OnlyShowIn','')='')
       and(d.ReadString('Desktop Entry','X-AllowRemove','true')='true')then
       begin

       //Check for Autopackage.org installation
       if pos('apkg-remove',LowerCase(d.ReadString('Desktop Entry','Actions','')))>0 then
       entry.UId:=PChar('!'+d.ReadString('Desktop Action Apkg-Remove','Exec',''))
       else
       entry.UId:=PChar(fname);

       if d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','')<>'' then
       begin
       try
       lp:='/usr/share/locale-langpack/'+GetLangID+'/LC_MESSAGES/'+
                         d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','app-install-data')+'.mo';
       if not FileExists(lp) then
        lp:='/usr/share/locale/de/'+GetLangID+'/LC_MESSAGES/'
            +d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','app-install-data')+'.mo';
       if FileExists(lp) then
       begin
        dt:=TMOFile.Create(lp);
        translate:=true;
       end;
       finally
       end;

       end;

       with entry do
       begin
       if d.ValueExists('Desktop Entry','Name['+GetLangID+']') then
        Name:=PChar(d.ReadString('Desktop Entry','Name['+GetLangID+']','<error>'))
       else
        Name:=PChar(ldt(d.ReadString('Desktop Entry','Name','<error>')));

         Name:=PChar(StringReplace(Name,'&','&&',[rfReplaceAll]));

        // instLst.Add(Lowercase(d.ReadString('Desktop Entry','Name','<error>')));

        if d.ValueExists('Desktop Entry','Comment['+GetLangID+']') then
         ShortDesc:=PChar(d.ReadString('Desktop Entry','Comment['+GetLangID+']',''))
        else
         ShortDesc:=PChar(ldt(d.ReadString('Desktop Entry','Comment','')));

        Author:=PChar(rsAuthor+': '+d.ReadString('Desktop Entry','X-Publisher','<error>'));
        if Author=rsAuthor+': '+'<error>' then
        Author:='';
        Version:='';
        if d.ReadString('Desktop Entry','X-AppVersion','')<>'' then
        Version:=PChar(rsVersion+': '+d.ReadString('Desktop Entry','X-AppVersion',''));

        //Load the icons
        if (LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.tiff') then
        begin
        try
        if (d.ReadString('Desktop Entry','Icon','')<>'')
        and(d.ReadString('Desktop Entry','Icon','')[1]<>'/') then
        begin
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            Icon:=PChar('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            Icon:=PChar('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) else
        if FileExists('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            Icon:=PChar('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            Icon:=PChar('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon',''));
        //
        if FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.xpm')then
            Icon:=PChar('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm') then
             Icon:=PChar('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm');
        if (FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png'))
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.png')then
            Icon:=PChar('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png');

        { This code is EXPERIMENTAL!}
        //Load KDE4 Icons
          //GetEnvironmentVariable('KDEDIRS')

        if FileExists('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png')
        else
        if FileExists('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png');
        end else
        begin
         if (FileExists(d.ReadString('Desktop Entry','Icon','')))
         and(LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.svg') then
            Icon:=PChar(d.ReadString('Desktop Entry','Icon',''));
        end;
        //If icon loading failed
        except writeLn('ERROR: Unable to load icon!');msg(StringReplace(rsCannotLoadIcon,'%a',Name,[rfReplaceAll]),mtWarning);
        end;
       end;
      end;

         newapp(fname,entry);
      //  if Assigned(dt) then dt.Free;
         if translate then dt.Free;

        end;
       d.Free;
end;

begin
j:=0;

msg(rsLoading,mtInfo);
blst:=TStringList.Create; //Create Blacklist

writeLn('Opening database...');
dsApp:= TSQLite3Dataset.Create(nil);
with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;
dsApp.Active:=true;

if blst.Count<4 then
begin
blst.Clear;
blst.LoadFromFile('/etc/lipa/blacklist');
blst.Delete(0);
end;

//Set original names
case group of
gtALL: tp:='all';
gtEDUCATION: tp:='education';
gtOFFICE: tp:='office';
gtDEVELOPMENT: tp:='development';
gtGRAPHIC: tp:='graphic';
gtNETWORK: tp:='network';
gtGAMES: tp:='games';
gtSYSTEM: tp:='system';
gtMULTIMEDIA: tp:='multimedia';
gtADDITIONAL: tp:='additional';
gtOTHER: tp:='other';
end;

if not DirectoryExists(RegDir) then
begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered:=true;
dsApp.First;
while not dsApp.EOF do
begin
 if (LowerCase(dsApp.FieldByName('AGroup').AsString)=tp)
 or (tp='all') then
 begin

 entry.Name:=PChar(dsApp.FieldByName('Name').AsString);

 blst.Add(entry.Name);
 entry.UId:=PChar(dsApp.FieldByName('ID').AsString);

 entry.Version:=PChar(rsVersion+': '+dsApp.FieldByName('Version').AsString);
 entry.Author:=PChar(rsAuthor+': '+dsApp.FieldByName('Publisher').AsString);
 if dsApp.FieldByName('Publisher').AsString='' then entry.Author:='';
 p:=RegDir+LowerCase(entry.Name+'-'+entry.UId)+'/';

// InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

 entry.ShortDesc:=PChar(dsApp.FieldByName('Description').AsString);
 if entry.ShortDesc='#' then entry.ShortDesc:='No description given';

 if FileExists(p+'icon.png') then
 entry.Icon:=PChar(p+'icon.png');
 newapp('ipk',entry);
 end;
 dsApp.Next;
end;
dsApp.Close;

{if (CBox.ItemIndex=0) or (CBox.ItemIndex=10) then
begin
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

j:=0;
for i:=0 to xtmp.Count-1 do begin
try
ReadXMLFile(Doc, xtmp[i]);
xnode:=Doc.FindNode('product');
 SetLength(AList,ListLength+1);
 Inc(ListLength);
 AList[ListLength-1]:=TListEntry.Create(MnFrm);
 AList[ListLength-1].Parent:=SWBox;
 AList[ListLength-1].AppLabel.Caption:=xnode.Attributes.GetNamedItem('desc').NodeValue;
 instLst.Add(LowerCase(xnode.Attributes.GetNamedItem('desc').NodeValue));
 blst.Add(AList[ListLength-1].AppLabel.Caption);
xnode:=Doc.DocumentElement.FindNode('component');
 AList[ListLength-1].Vlabel.Caption:=strVersion+': '+xnode.Attributes.GetNamedItem('version').NodeValue;
IdList.Add(xtmp[i]);
//Unsupported
AList[ListLength-1].MnLabel.Visible:=false;
AList[ListLength-1].DescLabel.Visible:=false;
AList[Listlength-1].id:=IDList.Count-1;
AList[ListLength-1].SetPositions;
Application.ProcessMessages;
except
j:=101;
end;
end;

tmp.free;
xtmp.Free;

end; //End Autopackage  }

n:=ConfigDir;
ini:=TIniFile.Create(n+'config.cnf');

//Search for other applications that are installed on this system...
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

if Root then //Only if user is root
begin
tmp.Assign(FindAllFiles('/usr/share/applications/','*.desktop',true));
xtmp.Assign(FindAllFiles('/usr/local/share/applications/','*.desktop',true));
end else
tmp.Assign(FindAllFiles(GetEnvironmentVariable('HOME')+'/.local/share/applications','*.desktop',false));

for i:=0 to xtmp.Count-1 do tmp.Add(xtmp[i]);

xtmp.Free;

if tp='games' then tp:='game';
if tp='multimedia' then tp:='audiovideo';
for i:=0 to tmp.Count-1 do
       begin
       ProcessDesktopFile(tmp[i],tp);
       end;
       tmp.Free;
ini.Free;

//Check LOKI-success:
if j>100 then
msg(rsLOKIError,mtWarning);

msg(rsReady,mtInfo); //Loading list finished!

dsApp.Free;
blst.Free; //Free blacklist

end;

end.

