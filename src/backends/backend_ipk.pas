(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 *  Matthias Klumpp
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Uninstaller backend for Listaller software
unit backend_ipk;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LiBackend, LiTypes, LiUtils, IniFiles, StrLocale, Process,
  LiFileUtil, SoftwareDB, ListallerDB, IPKCDef10, PackageKit;

type
  TIPKBackend = class(TLiBackend)
  private
    dskFileName: String;
    fastrm: Boolean;
    rmdeps: Boolean;

    appInfo: LiAppInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(ai: LiAppInfo): Boolean; override;
    function CanBeUsed: Boolean; override;
    function Run: Boolean; override;

    property FastRemove: Boolean read fastrm write fastrm;
    property RemoveDeps: Boolean read rmdeps write rmdeps;
  end;

//** Helper procedure to create USource file if missing
procedure CreateUpdateSourceList(path: String);

implementation

{ TIPKBackend }

constructor TIPKBackend.Create;
begin
  inherited;
end;

destructor TIPKBackend.Destroy;
begin
  inherited;
end;

function TIPKBackend.Initialize(ai: LiAppInfo): Boolean;
begin
  appInfo := ai;

  Result := true;
end;

function TIPKBackend.CanBeUsed: Boolean;
var
  ldb: TListallerDB; // We use no db abstraction here and query ListallerDB directly
begin
  ldb := TListallerDB.Create;
  ldb.Load(SUMode);
  Result := ldb.AppExists(appInfo.RemoveId);
  if Result then
  begin
    Result := DirectoryExistsUTF8(PkgRegDir + LowerCase(appInfo.PkName));
    if not Result then
      //EmitRequest(rsAppRegistBroken, rqError);
      perror('App registration is broken for #' + appInfo.RemoveId);
  end;
  ldb.Free;
end;

function TIPKBackend.Run: Boolean;
var
  tmp, tmp2, slist: TStringList;
  p, f: String;
  i, j: Integer;
  k: Boolean;
  upd: String;
  proc: TProcess;
  dlink: Boolean;
  t: TProcess;
  pkit: TPackageKit;
  db: TSoftwareDB;
  mnprog: Integer;
  bs: Double;
  ipkc: TIPKControl;
begin
  Result := false;
  p := PkgRegDir + LowerCase(appInfo.PkName) + '/';
  p := CleanFilePath(p);

  mnprog := 0;

  EmitProgress(0);

  //Check if an update source was set
  ipkc := TIPKControl.Create(p + 'application');
  upd := ipkc.USource;
  ipkc.Free;

  EmitInfoMsg(rsStartingUninstall);

  db := TSoftwareDB.Create;
  DB.Load;

  DB.OpenFilter(fAppIPK);
  while not DB.EndReached do
  begin
    if (DB.CurrentDataField.App.Name = appInfo.Name) and
      (DB.CurrentDataField.App.removeId = appInfo.removeId) then
    begin

      if DB.CurrentDataField.App.PkType = ptDLink then
        dlink := true
      else
        dlink := false;

      bs := 6;
      EmitProgress(4);

      if not dlink then
      begin
        tmp := TStringList.Create;
        tmp.LoadFromfile(p + 'files.list');
        bs := (bs + tmp.Count) / 100;
      end;

      if not fastrm then
      begin
        if FileExists(p + 'prerm') then
        begin
          EmitInfoMsg('PreRM-Script found.');
          t := TProcess.Create(nil);
          t.Options := [poUsePipes, poWaitonexit];
          t.CommandLine := FindBinary('chmod') + ' 775 ''' + p + 'prerm''';
          t.Execute;
          EmitInfoMsg('Executing prerm...');
          t.CommandLine := '''' + p + 'prerm''';
          t.Execute;
          t.Free;
          EmitInfoMsg('Done.');
        end;

        ///////////////////////////////////////
        if RmDeps then
        begin
          EmitInfoMsg(rsRMUnsdDeps);
          tmp2 := TStringList.Create;
          tmp2.Text := DB.CurrentDataField.App.Dependencies;

          if tmp2.Count > -1 then
          begin
            bs := (bs + tmp2.Count) / 100;
            for i := 0 to tmp2.Count - 1 do
            begin
              f := tmp2[i];
              //Skip catalog based packages - impossible to detect unneeded dependencies
              if pos('cat:', f) > 0 then
                break;

              //Asterisk (*) indicates that package was newly installed by installer
              if (LowerCase(f) <> 'libc6') and (f[1] <> '*') then
              begin
                //Check if another package requires this package
                t := TProcess.Create(nil);
                if pos(')', f) > 0 then
                  EmitInfoMsg(f + ' # ' + copy(f, pos(' (', f) + 2,
                    length(f) - pos(' (', f) - 2))
                else
                  EmitInfoMsg(f);

                pkit := TPackageKit.Create;

                if pos(')', f) > 0 then
                  pkit.GetRequires(copy(f, pos(' (', f) + 2, length(f) -
                    pos(' (', f) - 2))
                else
                  pkit.GetRequires(f);

                if pkit.RList.Count <= 1 then
                begin
                  if pos(')', f) > 0 then
                    pkit.RemovePkg(copy(f, pos(' (', f) + 2, length(f) -
                      pos(' (', f) - 2))
                  else
                    pkit.RemovePkg(f);
                  //GetOutPutTimer.Enabled:=true;

                  EmitInfoMsg('Removing ' + f + '...');
                end;

                pkit.Free;
              end;
              Inc(mnprog);
              EmitProgress(round(bs * mnprog));
            end; //End of tmp2-find loop

          end
          else
            EmitInfoMsg('No installed deps found!');

          tmp2.Free;
        end; //End of remove-deps request
      end; //End of "fast"-request
      //////////////////////////////////////////////////////

      if not dlink then
      begin
        slist := TStringList.Create;


        //Undo Mime-registration (if necessary)
        for i := 0 to tmp.Count - 1 do
        begin
          if pos('<mime>', tmp[i]) > 0 then
          begin
            EmitInfoMsg('Uninstalling MIME-Type "' + ExtractFileName(tmp[i]) + '" ...');
            t := TProcess.Create(nil);
            if (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i]))) = '.png') or
              (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i]))) = '.xpm') then
            begin
              t.CommandLine :=
                FindBinary('xdg-icon-resource') + ' uninstall ' +
                SysUtils.ChangeFileExt(ExtractFileName(DeleteModifiers(tmp[i])), '');
              t.Execute;
            end
            else
            begin
              t.CommandLine :=
                FindBinary('xdg-mime') + ' uninstall ' + DeleteModifiers(
                f + '/' + ExtractFileName(tmp[i]));
              t.Execute;
            end;
            t.Free;
          end;
        end;

        EmitInfoMsg('Removing files...');
        //Uninstall application
        for i := 0 to tmp.Count - 1 do
        begin

          f := SyblToPath(tmp[i]);

          f := DeleteModifiers(f);

          k := false;
          for j := 0 to slist.Count - 1 do
            if f = slist[j] then
              k := true;

          if not k then
            DeleteFile(f);

          Inc(mnprog);
          EmitProgress(round(bs * mnprog));
        end;

        Inc(mnprog);
        EmitProgress(round(bs * mnprog));

        EmitInfoMsg('Removing empty dirs...');
        tmp.LoadFromFile(p + 'dirs.list');
        proc := TProcess.Create(nil);
        proc.Options := [poWaitOnExit, poStdErrToOutPut, poUsePipes];
        for i := 0 to tmp.Count - 1 do
        begin
          proc.CommandLine := FindBinary('rm') + ' -rf ' + tmp[i];
          proc.Execute;
        end;
        proc.Free;

        if upd <> '#' then
        begin
          CreateUpdateSourceList(PkgRegDir);
          if FileExists(PkgRegDir + 'updates.list') then
          begin
            tmp.LoadFromFile(PkgRegDir + 'updates.list');
            EmitInfoMsg('Removing update-source...');
            for i := 1 to tmp.Count - 1 do
              if pos(upd, tmp[i]) > 0 then
              begin
                tmp.Delete(i);
                break;
              end;
            tmp.SaveToFile(PkgRegDir + 'updates.list');
            tmp.Free;
          end;
        end;

      end;

      EmitInfoMsg('Unregistering...');

      DB.DeleteCurrentField;

      proc := TProcess.Create(nil);
      proc.Options := [poWaitOnExit];
      proc.CommandLine := FindBinary('rm') + ' -rf ' + '''' +
        ExcludeTrailingBackslash(p) + '''';
      proc.Execute;
      proc.Free;

      Inc(mnprog);
      EmitProgress(round(bs * mnprog));

      EmitInfoMsg('Application removed.');
      EmitInfoMsg('- Finished -');
      Result := true;

      break;
    end;
    DB.NextField;
  end;

  DB.CloseFilter;
  DB.Free;
end;

procedure CreateUpdateSourceList(path: String);
var
  fi: TStringList;
begin
  if not FileExists(path + 'updates.list') then
  begin
    ForceDirectories(path);
    fi := TStringList.Create;
    fi.Add('List of update repositories v.1.0');
    fi.SaveToFile(path + 'updates.list');
    fi.Free;
  end;
end;

end.

