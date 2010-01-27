
typedef enum
{
	rqError,
	rqWarning,
	rqQuestion,
	rqInfo,

} RqType;

typedef enum
{
	rqsYes,
	rqsNo,
	rqsOK,

} RqResult;

typedef enum
{
	mtInfo,
	mtWarning,

} MType;

typedef RqResult (*LiMessageCallback)(gchar *msg,
									 MType imp,
									 gpointer user_data);
									 
 //** Callback that submits a notification
 TMessageCall = procedure(msg: PChar;imp: TMType;user_data: Pointer);cdecl;
 //** Called if a progress was changed
 TProgressCall = procedure(pos: Integer;user_data: Pointer);cdecl;
 //** Called if progress was changed; only for internal use
 TProgressEvent = procedure(pos: Integer;user_data: Pointer) of object;
									 
									 
typedef enum
{
	gtALL,
        gtEDUCATION,
        gtOFFICE,
        gtDEVELOPMENT,
        gtGRAPHIC,
        gtNETWORK,
        gtGAMES,
        gtSYSTEM,
        gtMULTIMEDIA,
        gtADDITIONAL,
        gtOTHER,
        gtUNKNOWN,

} GroupType;

typedef struct _AppInfo        AppInfo;

struct _AppInfo
{
  char *Name;
  char *ShortDesc;
  char *Version;
  char *Author;
  char *Icon;
  char *UId;
  GroupType Group;
};

//** Event to catch thrown application records
 TAppEvent = function(name: PChar;obj: PAppInfo): Boolean;cdecl;
 
typedef enum
{
	ptLinstall,
	ptDLink,
	ptContainer,
	ptUnknown,

} PkgType;
