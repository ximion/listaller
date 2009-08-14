unit appman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

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

GroupType = (gtALL,gtEDUCATION,gtOFFICE,gtDEVELOPMENT,gtGRAPHIC,gtNETWORK,gtGAMES,gtSYSTEM,gtMULTIMEDIA,gtADDITIONAL,gtOTHER);

//** Event to catch messages
TYMessageEvent = function(msg: String;ty: MessageType): Boolean; cdecl;
//** Event to catch thrown application records
TAppEvent = function(name: PChar;obj: TAppInfo): Boolean;cdecl;

 const amanlib='libappmanager.so';

 function load_applications(ty: GroupType): Boolean;cdecl;external amanlib name 'load_applications';
 function register_message_call(call: TYMessageEvent): Boolean;cdecl;external amanlib name 'register_message_call';
 function register_application_call(call: TAppEvent): Boolean;cdecl;external amanlib name 'register_application_call';
 function set_su_mode(md: Boolean): Boolean;cdecl;external amanlib name 'set_su_mode';

implementation

end.

