{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** Functions to build IPK packages from source files
unit ipkbuild;

{$mode objfpc}{$H+}

interface

uses
  MD5, Classes, GPGSign, LiTypes, LiUtils, Process, RegExpr, SysUtils,
  IPKCDef10, LiFileUtil, IPKPackage11;

type

  //** Information about the new package
  TPackInfo = record
    desc: TStringList;
    pkName, Maintainer, Author: String;
    Version: String;
    build, depDEB, depRPM: TStringList;
    path: String;
    out: String;
  end;

{** Creates an IPK-Update-Source (IPKUS)
    @param FName Name of the IPS source file
    @param path Path to an folder where the source should be created}
procedure CreateUpdateSource(FName, path: String);
{** Builds the application from source using the "Build" element
    @param Connection to an TPackageInfo}
procedure BuildApplication(pk: TPackInfo);
{** Builds an IPK package from an IPS source
    @param fi IPS source
    @param o Output filename
    @param genbutton Generate distro info button
    @param signpkg Create signed package}
procedure BuildPackage(fi: String; o: String; genbutton: Boolean = false;
  signpkg: Boolean = false);
{** Creates the "Linux distribution compatible" button.
    @param dlist Names of the distributions that are compatible
    @param of Name of the output PNG file}
procedure CreateLiCompButton(dlist: TStringList; op: String);

implementation

const
  //** Working directory of Listaller's build tool
  WDir = '/tmp/listaller/pkgbuild/';
  //** Size of Linux output pipe
  READ_BYTES = 2048;

////////////////////////////////////////////////////////////////////////////////
///////////////////Functions that are used by the others////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure BuildApplication(pk: TPackInfo);
var
  i: Integer;
  M: TMemoryStream;
  n: longint;
  BytesRead: longint;
  p: TProcess;
  s: String;
  lastDir: String;
begin
  CreateDir(pk.out + '/pkbuild');

  p := TProcess.Create(nil);
  p.Options := [poUsePipes];

  writeLn;
  writeLn('=== Building application ===');

  for i := 0 to pk.build.Count - 1 do
    if pos('include:"', pk.build[i]) > 0 then
    begin
      pk.build[i] := StringReplace(pk.build[i], 'include:"', '', [rfReplaceAll]);
      pk.build[i] := copy(pk.build[i], 1, pos('"', pk.build[i]) - 1);
    end;
  lastDir := '';
  getdir(0, lastDir);
  chdir(pk.path);
  for i := 0 to pk.build.Count - 1 do
  begin
    if length(pk.build[i]) > 0 then
    begin
      if pk.build[i][1] = '.' then
      begin
        p.CurrentDirectory := ExtractFilePath(
          copy(pk.path + pk.build[i], 0, pos(' ', pk.path + pk.build[i])));
        p.CommandLine := StringReplace(pk.path + pk.build[i], '%IDIR',
          pk.out + '/pkbuild', [rfReplaceAll]);
      end
      else
      begin
        p.CurrentDirectory :=
          ExtractFilePath(copy(pk.build[i], 0, pos(' ', pk.build[i])));
        p.CommandLine := StringReplace(pk.build[i], '%IDIR', pk.out +
          '/pkbuild', [rfReplaceAll]);
      end;
    end
    else
      Continue;

    writeLn('[Exec]: ' + p.CommandLine);//pk.build[i]);
    p.CommandLine := FindBinary('bash') + ' ' + p.CommandLine;
    p.Execute;
    n := 0;

    M := TMemoryStream.Create;
    m.Clear;
    BytesRead := 0;
    while P.Running do
    begin
      M.SetSize(BytesRead + READ_BYTES);
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then
      begin
        SetString(s, PChar(M.Memory + BytesRead), n);
        Write(s);
        Inc(BytesRead, n);
      end
      else
      begin
        // no data, wait 100 ms
        Sleep(100);
      end;
    end;
    // read last part
    repeat
      M.SetSize(BytesRead + READ_BYTES);
      // try reading it
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then
      begin
        SetString(s, PChar(M.Memory + BytesRead), n);
        Write(s);
        Inc(BytesRead, n);
      end;
    until n <= 0;
    M.Free;

    if p.ExitStatus > 0 then
    begin
      writeLn('error:');
      writeLn(' Build failed!');
      writeLn(' Please fix all errors to continue');
      halt(p.ExitStatus);
    end;
  end;

  chdir(lastDir);
  p.Free;
  pk.build.Free;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Create Update-Source from IPS-File///////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure CreateUpdateSource(FName, path: String);
var
  fls: TStringList;
  i: Integer;
  ubit: TLiUpdateBit;
  h, dir, fn: String;
  script: TStringList;
begin
  if not FileExists(ChangeFileExt(fname, '') + '_fdata.ulist') then
  begin
    writeLn('error:');
    writeLn(' No file information found. You need to build the package first!');
    halt(1);
    exit;
  end;

  fls := TStringList.Create;
  fls.LoadFromFile(ChangeFileExt(fname, '') + '_fdata.ulist');

  writeLn('Building update source...');
  writeLn('Please wait!');
  ubit := TLiUpdateBit.Create;

  i := 0;
  while i <= fls.Count - 1 do
  begin

    if fls[i][1] = '>' then
      dir := copy(fls[i], 2, length(fls[i]))
    else
    begin
      if (fls[i][1] = '/') or (fls[i][1] = '.') then
      begin

        if fls[i][1] = '.' then
          fn := CleanFilePath(ExtractFilePath(fname) + '/' + fls[i])
        else
          fn := fls[i];

        fn := DeleteModifiers(fn);

        if (not FileExists(DeleteModifiers(fn))) and
          (not FileExists(ExtractFilePath(FName) + fn)) then
        begin
          writeLn('error: ');
          writeln(' The file "' + fn + '" does not exists!');
          halt(101);
        end;

        if not DirectoryExists(path + '/' + SyblToX(dir)) then
          ForceDirectories(path + '/' + SyblToX(dir));
        h := path + '/' + SyblToX(dir);

        if (i = fls.Count - 1) or (MD5.MDPrint(MD5.MD5File(fn, 1024)) <> fls[i + 1]) then
        begin
          writeln('Writing ' + ExtractFileName(fn) + ' ...');
          ubit.Compress(fn, CleanFilePath(h + '/' + ExtractFileName(fn) + '.xz'));

          fls[i + 1] := MD5.MDPrint(MD5.MD5File(fn, 1024));

        end;
        writeLn('File ' + ExtractFileName(fn) + ' checked out.');

      end;
    end;

    Inc(i);
  end;

  ubit.Free;

  writeLn('Save configuration...');

  fls.SaveToFile(ChangeFileExt(fname, '') + '_fdata.ulist');

  for i := 0 to fls.Count - 1 do
    if (length(fls[i]) > 0) and ((fls[i][1] = '/') or (fls[i][1] = '.')) then
      fls[i] := ExtractFileName(fls[i]);

  fls.SaveToFile(path + 'content.id');
  fls.LoadFromFile(fname);
  script := TStringList.Create;
  for i := 0 to fls.Count - 1 do
  begin
    if pos('!-Files ~', fls[i]) > 0 then
      break
    else
      script.Add(fls[i]);
  end;
  fls.Free;
  script.SaveToFile(path + 'source.pin');
  script.Free;


  writeLn('Done.');
  writeLn('');
  writeLn(' Copy the folder ' + path + ' to an location on a webserver,');
  writeLn(' to create an update source in this directory.');
  writeLn(' To update the repository, execute the "lipa -u" command with the same parameters again.');
end;

function IsNumeric(Value: String; const AllowFloat: Boolean = false): Boolean;
var
  ValueInt: Integer;
  ValueFloat: extended;
  ErrCode: Integer;
begin
  // Check for integer: Val only accepts integers when passed integer param
  Value := SysUtils.Trim(Value);
  Val(Value, ValueInt, ErrCode);
  Result := ErrCode = 0;      // Val sets error code 0 if OK
  if not Result and AllowFloat then
  begin
    // Check for float: Val accepts floats when passed float param
    Val(Value, ValueFloat, ErrCode);
    Result := ErrCode = 0;    // Val sets error code 0 if OK
  end;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Function to build IPK-Packages///////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure BuildPackage(fi: String; o: String; genbutton: Boolean = false;
  signpkg: Boolean = false);
var
  i, j, k: Integer;
  b: Boolean;
  fc: TStringList;
  lh, h, s: String;
  packtype: PkgType;
  pki: TPackInfo;
  sl, files, fsec: TStringList;
  tmpdepends: TStringList; //Contains all autodetected dependencies
  prID: String;
  dlist: TStringList;
  aname: String;
  control: TIPKControl;
  script: TIPKScript;
  ipkpkg: TLiPackager;
  res: Boolean;
  uinfo: TStringList; //Generate update source baseinfo

  procedure RaiseCopyError(fname: String);
  begin
    writeLn('error: ');
    writeln(' Copying of ' + fname + ' failed.');
    halt(5);
  end;

  procedure RaiseError(str: String);
  begin
    writeLn('error:');
    writeLn(' ' + str);
  end;

  procedure bp_AddFile(fname: String; const basepath: String = '');
  var
    orig: String;
    x: String;
    forig: String;
  begin
    if fname = '' then
      exit;
    orig := h;
    if basepath <> '' then
      h := h + '/' + StringReplace(ExtractFilePath(fname), basepath, '', [rfReplaceAll])
    else
      h := h + '/';

    h := CleanFilePath(h);
    ForceDirectories(WDir + h);

    if not FileNameIsAbsolute(fname) then
      forig := ExtractFilePath(fi) + DeleteModifiers(fname)
    else
      forig := DeleteModifiers(fname);

    res := FileCopy(forig, WDir + AppendPathDelim(h) +
      ExtractFileName(DeleteModifiers(fname)));

    if b then
      if FileIsExecutable(forig) then
      begin
        GetELFDepends(forig, tmpdepends);
      end;

    if not res then
      RaiseCopyError(DeleteModifiers(fname));

    if basepath <> '' then
    begin
      x := StringReplace(ExtractFilePath(fname), basepath, '', [rfReplaceAll]);
      x := StringReplace(x, '/', '', [rfReplaceAll]);
      x := StringReplace(x, ' ', '', [rfReplaceAll]);
      if x <> '' then
      begin
        fc.Add('>' + CleanFilePath(s + '/' + StringReplace(
          ExtractFilePath(fname), basepath, '', [rfReplaceAll])));
      end;
    end;
    fc.Add(CleanFilePath(h + '/' + ExtractFileName(DeleteModifiers(fname))));

    if not ipkpkg.AddDataFile(WDir + h + '/' +
      ExtractFileName(DeleteModifiers(fname))) then
    begin
      writeLn('error: ');
      writeln(' Could not add file "' + DeleteModifiers(fname) + '" to TAR archive.');
      halt(8);
    end;

    //Add information to file control section
    fc.Add(MD5.MDPrint(MD5.MD5File(DeleteModifiers(fname), 1024)));
    //ExcludeTrailingBackslash(Files[i+1]));

    //Add info to update source info
    if not HasSharedMod(fname) then
    begin
      uinfo.Add(fname);
      uinfo.Add(MD5.MDPrint(MD5.MD5File(DeleteModifiers(fname), 1024)));
    end;

    h := orig;
    Write('.');
  end;

begin
  if FileExists(o) then
  begin
    writeLn('error: ');
    writeLn(' This file already exists.');
    writeLn(' Please choose another target!');
    halt(6);
    exit;
  end;
  if FileExists(WDir) then
  begin
    writeLn('Cleaning up...');
    LiFileUtil.DeleteDirectory(WDir, false);
  end;

  if not FileExists(fi) then
  begin
    writeLn('Input file does not exists!');
    halt(5);
  end;

  writeLn('Reading IPS source file...');

  sl := TStringList.Create; //ReadIn file
  sl.LoadFromFile(fi);
  files := TStringList.Create;
  fsec := TStringList.Create;

  if (LowerCase(sl[0]) <> 'ipk-standard-version: 1.1') and
    (LowerCase(sl[0]) <> 'ipk-standard-version: 1.0') then
  begin
    writeLn(' This is no valid IPS source file');
    halt(1);
  end;

  for i := 0 to sl.Count - 1 do
    if pos('!-Files ~', sl[i]) > 0 then
      break;
  for j := i to sl.Count - 1 do
    fsec.Add(sl[j]);

  sl.Free;

  script := TIPKScript.Create;
  script.LoadFromFile(fi);

  if (LowerCase(ExtractFileExt(o)) <> '.ipk') then
  begin
    if script.IPKName = '' then
      o := ExtractFilePath(fi) + '/' + script.PkName + '.ipk'
    else
      o := ExtractFilePath(fi) + '/' + script.IPKName + '.ipk';
  end;

  control := script.FinalizeToControl;
  script.Free;

  control.BasePath := AppendPathDelim(ExtractFilePath(fi));

  if pos(' ', control.PkName) > 0 then
  begin
    RaiseError('Package ID must not contain spaces!');
    control.Free;
    exit;
  end;

  if trim(control.PkName) = '' then
  begin
    RaiseError('Package must have a unique ID!');
    control.Free;
    exit;
  end;

  o := CleanFilePath(o);

  uinfo := TStringList.Create;

  sl := TStringList.Create;
  control.ReadBuildCMDs(sl);
  if sl.Count > 0 then
  begin
    for i := 0 to sl.Count - 1 do
      if (length(sl[i]) > 0) and (sl[i][1] = ' ') then
        sl[i] := copy(sl[i], 2, length(sl[i]));

    pki.build := sl;
    pki.path := ExtractFilePath(fi);
    pki.out := ExtractFilePath(o);
    BuildApplication(pki);
  end;
  if sl is TStringList then
    sl.Free; //Can be freed by BuildApplication()

  writeLn;
  writeLn('=== Building IPK Package ===');
  writeLn;
  if not DirectoryExists(tmpdir) then
    SysUtils.CreateDir(tmpdir);
  if not DirectoryExists(WDir) then
    SysUtils.CreateDir(WDir)
  else
  begin
    DeleteDirectory(WDir, true);
    SysUtils.CreateDir(WDir);
  end;

  //Creating dirs
  CreateDir(WDir + 'files/');
  SysUtils.CreateDir(WDir + 'pkgdata/');

  fc := TStringList.Create;
  ipkpkg := TLiPackager.Create(o);
  ipkpkg.BaseDir := wdir;

  tmpdepends := TStringList.Create;

  b := false;
  control.ReadDependencies('', sl);
  for i := 0 to sl.Count - 1 do
    if pos('%AUTODEPENDS', sl[i]) > 0 then
    begin
      sl.Delete(i);
      b := true;
      break;
    end;

  Write('Preparing files.');

  //Replace all build-time placeholders
  for i := 0 to fsec.Count - 1 do
    fsec[i] := StringReplace(fsec[i], '%IDIR', CleanFilePath(
      ExtractFilePath(o) + '/pkbuild/'), [rfReplaceAll]);

  for j := 0 to fsec.Count - 1 do
  begin
    files.Clear;
    fc.Clear;
    prID := '-1';
    if pos('!-Files ~', fsec[j]) > 0 then
    begin
      prID := (copy(fsec[j], pos('~', fsec[j]) + 1, length(fsec[j])));
      uinfo.Add('!-Files ~' + prID);
      for i := j + 1 to fsec.Count - 1 do
        if pos('!-Files ~', fsec[i]) > 0 then
          break
        else
          files.Add(fsec[i]);
    end;

    i := 0;
    while i < files.Count do
    begin
      if length(files[i]) > 0 then
        if files[i][1] = '>' then
        begin
          //We have a new target folder. Set path
          s := copy(files[i], 2, length(files[i]));
          //Copy targed folder section
          fc.Add(files[i]);
          //Add info to update info list too
          uinfo.Add(files[i]);
        end
        else
        begin
          if (files[i][1] <> '#') then
          begin

            if not IsNumeric(prID) then
            begin
              if prID[1] = '{' then
                prID := copy(prID, 2, length(prID) - 1);  //If own dir is forced

              h := '/files/' + prID + '/' + SyblToX(s); //For multiarch packages
            end
            else
              h := '/files/' + SyblToX(s); //Normal mode

            if not DirectoryExists(WDir + h) then
              LiFileUtil.ForceDirectory(WDir + h);

            if (DirectoryExistsUTF8(DeleteModifiers(Files[i]))) or
              (pos('*', ExtractFileName(Files[i])) > 0) then
            begin
              sl := TStringList.Create;
              FindFiles(sl, ExtractFilePath(Files[i]), ExtractFileName(Files[i]));
              for k := 0 to sl.Count - 1 do
                bp_AddFile(sl[k], ExtractFilePath(files[i]));

              if sl is TStringList then
                sl.Free;
            end
            else
            begin
              if (not FileExists(DeleteModifiers(Files[i]))) and
                (not FileExists(ExtractFilePath(fi) + DeleteModifiers(Files[i]))) then
              begin
                writeLn('error: ');
                writeln(' The file ' + DeleteModifiers(Files[i]) + ' doesn''t exists!');
                halt(6);
              end;
              bp_AddFile(Files[i]);
            end;

          end;
        end;
      Inc(i);
    end;


    if prID <> '-1' then
    begin
      fc.SaveToFile(WDir + 'pkgdata/' + 'fileinfo-' + prID + '.id');
      ipkpkg.AddControlFile(WDir + 'pkgdata/fileinfo-' + prID + '.id');
    end;

  end; //End of file including
  writeLn;
  writeLn;
  fsec.Free;

  packtype := control.SType;

  if LowerCase(control.Architecture) = 'any' then
    control.Architecture := GetSystemArchitecture;

  //Add icon
  if control.Icon <> '' then
  begin
    h := control.Icon;
    if not FileNameIsAbsolute(h) then
      h := control.BasePath + h;

    if not FileExists(h) then
    begin
      writeLn('error: ');
      writeLn(' Icon-path is invalid!');
      halt(9);
    end
    else
    begin
      FileCopy(h, WDir + 'pkgdata/' + 'packicon.png');
      //!!! Should be changed to support more picture-filetypes
      control.Icon := '/pkgdata/' + 'packicon.png';
      ipkpkg.AddControlFile(WDir + 'pkgdata/' + 'packicon.png');
      writeLn(' I: Icon included.');
    end;
  end; //END <>nil

  //Linstall specific actions
  if packtype = ptLinstall then
  begin
    writeLn('Mode: Build standard IPK');

    if genbutton then
    begin
      dlist := TStringList.Create;
      control.UseMoTranslation := false;
      aname := control.AppName;
      lh := control.DSupport;
      control.UseMoTranslation := true;
      i := 1;
      while length(lh) > 0 do
      begin
        i := pos(',', lh);
        if i <= 0 then
          i := pos(';', lh);
        if i = 0 then
        begin
          dlist.Add(lh);
          lh := '';
        end
        else
        begin
          dlist.Add(copy(lh, 1, i - 1));
          Delete(lh, 1, i);
        end;
      end;
      writeLn(' I: A compatibility button will be generated.');
    end;

    writeLn('Creating control-script...');
    SysUtils.CreateDir(WDir + 'pkgdata/');

    sl := TStringList.Create;
    if b then
    begin
      writeLn('Updating dependecy information...');
      for i := 0 to sl.Count do
        tmpdepends.Add(sl[i]);
      RemoveDuplicates(tmpdepends);

      for i := 0 to tmpdepends.Count - 1 do
        if tmpdepends[i][length(tmpdepends[i])] = '.' then
          tmpdepends[i] := tmpdepends[i] + '*';

      control.WriteDependencies('', tmpdepends);
    end;
    sl.Free;
    tmpdepends.Free;

    sl := TStringList.Create;

    control.ReadAppLicense(sl);
    if sl.Count > 0 then
    begin
      control.WriteAppLicense(sl);
    end
    else
      writeLn(' W: No license file found!');

    sl.Clear;
    control.ReadAppDescription(sl);
    if sl.Count > 0 then
    begin
      control.WriteAppDescription(sl);
    end
    else
      writeLn(' W: No long description found!');

    sl.Free;


    if FileExists(ExtractFilePath(fi) + '/preinst') then
    begin
      FileCopy(ExtractFilePath(fi) + '/preinst', WDir + 'preinst');
      ipkpkg.AddControlFile(WDir + 'preinst');

      writeLn(' I: Preinst script found.');
    end;
    if FileExists(ExtractFilePath(fi) + '/postinst') then
    begin
      FileCopy(ExtractFilePath(fi) + '/postinst', WDir + 'postinst');
      ipkpkg.AddControlFile(WDir + 'postinst');
      writeLn(' I: Postinst script found.');
    end;
    if FileExists(ExtractFilePath(fi) + '/prerm') then
    begin
      FileCopy(ExtractFilePath(fi) + '/prerm', WDir + 'prerm');
      ipkpkg.AddControlFile(WDir + 'prerm');
      writeLn(' I: Prerm script found.');
    end;

    //Include mo files for translation
    sl := TStringList.Create;
    control.GetMoFileList(sl);
    control.SetMoFilesToDir('locale');
    if sl.Count > 0 then
      ForceDirectories(WDir + 'locale');
    for i := 0 to sl.Count - 1 do
    begin
      if not FileCopy(sl[i], WDir + 'locale/' + ExtractFileName(sl[i])) then
        RaiseCopyError(sl[i]);
      writeLn(' I: Include locale: ' + ExtractFileName(sl[i]));
      ipkpkg.AddControlFile(WDir + 'locale/' + ExtractFileName(sl[i]));
    end;
    sl.Free;

    writeLn('Saving update info to "' + ChangeFileExt(ExtractFileName(fi), '') +
      '_fdata.ulist"');
    uinfo.SaveToFile(ChangeFileExt(fi, '') + '_fdata.ulist');

    //Done mofiles
  end
  else
  begin //If pkgtype is another one
    if packtype = ptDLink then
    begin
      writeLn('Mode: Build DLink IPK package');

      sl := TStringList.Create;
      control.ReadAppDescription(sl);

      if sl.Count <= 0 then
      begin
        writeLn('error: ');
        writeLn(' Description is invalid!');
        halt(10);
      end;

      control.WriteAppDescription(sl);

    end;
    //??? Container builder needs more work!
    //    (Add additional file processor)
    if packtype = ptContainer then
    begin
      writeLn('Mode: Build container IPK package');

      if not FileCopy(control.Binary, WDir + 'files/' +
        ExtractFileName(control.Binary)) then
        RaiseCopyError(control.Binary);

      ipkpkg.AddControlFile(WDir + 'files/' + ExtractFileName(control.Binary));
      control.Binary := '/files/' + ExtractFileName(control.Binary);
    end;
  end;

  uinfo.Free;

  randomize;

  control.SaveToFile(WDir + 'arcinfo.pin');

  ipkpkg.AddControlFile(WDir + 'arcinfo.pin');

  ipkpkg.Finalize; //Freeze the IPK package state
  files.Free;

  if signpkg then
  begin
    writeLn('Now signing package.');
    if GPGFound then
    begin
      writeLn('Calling GnuPG sign...');
      if not ipkpkg.SignPackage then
        writeLn(' E: Package signing FAILED!');
    end
    else
      writeLn(' E: GPG was not found! Cannot sign package.');
  end
  else
    writeLn(' I: Package is not signed.');

  i := random(9999) + 1000;

  writeLn('Compressing package...');
  if FileExists(o) then
  begin
    writeLn('warning:');
    writeLn(' The file "' + ExtractFileName(o) + '" already exists in output dir.');
    writeLn(' Renamed to "' + ExtractFileName(o) + '~' + IntToStr(i) + '.ipk"');
    o := ChangeFileExt(o, '') + '~' + IntToStr(i) + '.ipk';
  end;
  writeLn(' #-> ' + o);
  ipkpkg.IPKFile := o;
  if not ipkpkg.ProduceIPKPackage then
  begin
    writeln('error:');
    writeLn(' Build of IPK package failed!');
    halt(1);
    exit;
  end;
  sleep(20);

  if FileExists(WDir) then
  begin
    writeLn('Cleaning up...');
    LiFileUtil.DeleteDirectory(WDir, false);
  end;

  writeLn('Done!');
  if (genbutton) and (Assigned(dlist)) then
  begin
    if aname <> '' then
      CreateLiCompButton(dlist, ExtractFilePath(o) + '/' + aname + '-licomp.png')
    else
      CreateLiCompButton(dlist, ExtractFilePath(o) + '/' + ChangeFileExt(
        ExtractFileName(o), '') + '-licomp.png');

    dlist.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Creates the Linux-compatibility button///////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure CreateLiCompButton(dlist: TStringList; op: String);
var
  //bt, buf, res: TPNGImage;
  i, j: Integer;
begin
  //FIXME: This function was based on OPBitmaps. It needs to be converted to StdGraphics or
  //       something else which meets our requirements.
  writeLn;
  writeLn('=== Creating graphics ===');
  writeLn('TODO: Make the function use StdGraphics!');
  {writeLn('Generating "Linux compatible" button ' + ExtractFileName(op) + ' ...');
  bt := TPNGImage.Create;
  buf := TPNGImage.Create;
  res := TPNGImage.Create;
  buf.LoadFromFile('/usr/share/listaller/graphics/libutton/left.png');
  j := dlist.Count div 2;
  if (dlist.Count mod 2) > 0 then
    Inc(j);
  Dec(j);

  for i := 0 to j - 1 do
  begin
    if i = 0 then
      bt.LoadFromFile('/usr/share/listaller/graphics/libutton/firstblock.png')
    else
      bt.LoadFromFile('/usr/share/listaller/graphics/libutton/block.png');

    res.Width := buf.Width + bt.Width;
    res.Height := bt.Height;
    res.Canvas.Draw(0, 0, buf);
    res.Canvas.Draw(buf.Width, 0, bt);
    buf.Width := res.Width;
    buf.Height := res.Height;
    buf.Canvas.Draw(0, 0, res);
  end;

  bt.LoadFromFile('/usr/share/listaller/graphics/libutton/lastblock.png');
  res.Width := buf.Width + bt.Width;
  res.Height := bt.Height;
  res.Canvas.Draw(0, 0, buf);
  res.Canvas.Draw(buf.Width, 0, bt);

  j := 0;
  for i := 0 to dlist.Count - 1 do
  begin
    if FileExists('/usr/share/listaller/graphics/libutton/distro/' +
      LowerCase(dlist[i]) + '.png') then
    begin
      bt.LoadFromFile('/usr/share/listaller/graphics/libutton/distro/' +
        LowerCase(dlist[i]) + '.png');
      if i mod 2 = 0 then
      begin
        j := i;
        res.Canvas.Draw(100 + 30 * i, 14, bt);
      end
      else
        res.Canvas.Draw(100 + 30 * j, 74, bt);
    end
    else
    begin
      writeLn('No icon found for distribution "' + dlist[i] + '"');
      writeLn('You can add the icon yourself.');
    end;
  end;

  res.SaveToFile(op);
  bt.Free;
  buf.Free;
  res.Free;}
end;

end.

