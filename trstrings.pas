{ trstrings.pas
  Copyright (C) Listaller Project 2008-2009

  trstrings.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  trstrings.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains the default strings for translation
unit trstrings;

{$mode objfpc}{$H+}

interface

resourcestring
{Installer application (listallgo)
----------------------------------}
strInstAborted='Installation aborted.';
strAppNInstall='The application %a was not installed.';
strWasInstalled='The application %a was installed successfully!';
strInstallNow='Install now';
strRunParam='Please run listallgo with path to install-package as first parameter!';
strWelcome='Welcome!';
strnToStart='Press "Next" to start the installation!';
strprogDesc='Program description:';
strLicense='Software license';
strpleaseRead='Please read the following information carefully:';
strRunning='Running installation...';
strplWait='Please wait.';
strComplete='Installation completed!';
strPrFinish='Press "Finish" to close.';
strFinish='Finish';
strAbort='Abort';
strBack='Back';
strNext='Next';
strDispLog='Display installation log';
strIagree='I agree with the above terms and conditions';
strInagree='I don''t agree';
strLDnSupported='Your Linux distribution is not supported by Listaller yet!';
strnSupported='Your Linux distribution is not supported by this package!';
strInClose='The installer will close now';
notifyDevs='Please notify the developers on http://launchpad.net/listaller';
strExtractError='Error while extracting files!';
strPkgDM='The Listaller-package could be damaged or you haven''t enough rights for access some files';
strAbLoad='Loading aborted.';
strAlreadyinst='This application is already installed';
strInstallAgain='Do you want to install it again?';
strWelcomeTo='Welcome to the installation of %a';
strInstOf='Installation of %a';
strTestmode='Testmode';
strTestFinished='Test of the application finished.';
strpkgInval='The package was invalid!';
strCouldntSolve='Dependencies couldn''t be solved!';
strViewLog='Please view the logfile at %p';
strPKGError='Installation package is corrupt';
strAppClose='The application will close now ';
strStep1='Phase 1/4: Resolving dependencies...';
strWDLdep='This application wants to download & install a dependency from %l';
strwAllow='Do you want to allow this?';
strLiCloseANI='Listaller will close now. Application couldn''t be installed.';
strStep2='Phase 2/4: Installing files...';
strStep3='Phase 3/4: Chmod new files...';
strStep4='Phase 4/4: Registering application...';
strAddUpdSrc='This package tries to add the following update source: ';
strQAddUpdSrc='Should this source be added to the update-list?';
strFinished='Finished';
strinstAnyway='Do you want to install it anyway? (This could be problematical)';
strInvarchitecture='This package was not build for this system-architecture';
strWillDLFiles='(This program will download the needed files from the internet)';
strInvalidDVersion='Package was not build for this release.';
strcnGetDep='Problem while downloading the dep-file.';
strFTPfailed='Problem while downloading the packages. Maybe the login on the FTP-Server failed.';
strSuccess='Success!';
strMain='Main';
strDetails='Details';
strInstallation='Installation';
strNoLDSources='There are no sources available for your Linux-distribution'#13'Try to install common packages?';
strUseCompPQ='Use compatible packages?';
strNoComp='No compatible packages found!';
strActionNotPossiblePkg='The selected action is not possible with this package.'#13'Please contact the package creator for more information!';
strIDInvalid='This package has no valid ID'#13'The installer will close.';
strReInstall='Reinstall';
strPkgDownload='The following packages will be downloaded:';
strGetDependencyFrom='Get dependency from';
strPlWait2='Please wait...';
strDepDLProblem='Problem while downloading the dep-file.';
strHashError='Hash doesn''t match!'#13'The package has been modified.'#13'Please obtain a new copy';
strInstallationMode='Installation mode:';
strIModeInstruction='Select which parts of the application should be installed.';
strMode='Mode';
{ExecMode Form
-----------------}
strWantToDoQ='What do you want to do?';
strPackageKitWarning='Your PackageKit version is %cp. Listaller needs PackageKit %np or higher to work correctly.'#13'Please update PackageKit!';
strSpkWarning='Make sure that you can trust this package publisher!';
strInstallEveryone='Install application for everyone';
strTestApp='Test application';
strInstallHome='Install into my Home directory';
strSelInstMode='Select installation mode';
{Software manager
-----------------}
strSoftwareManager='Software Manager';
strUpdSources='Update sources';
strClose='Close';
strDelSrc='Delete source';
strListofSrc='Here''s a list of all update-sources:';
strUninstall='Uninstall';
strInstNew='Install new application';
strShowSettings='Listaller settings';
strSWCatalogue='Software catalogue';
strShow='Show:';
strAll='All';
strEducation='Education';
strOffice='Office';
strDevelopment='Development';
strGraphic='Graphic';
strNetwork='Network';
strGames='Games';
strSystem='System';
strMultimedia='Multimedia';
strAddidional='Additional';
strOther='Other';
strVersion='Version';
strAuthor='Autor';
strUsername='Username';
strPassword='Password';
strProxySettings='Proxy-Settings';
strEnableProxy='Enable Proxy-Server';
strLOKIError='Can''t load LOKI-Setup information.';
strCannotLoadIcon='Unable to load the icon of %a. Please notify the developers of Listaller or this application!';
strAutoLoadDep='Load dependencies from included webserver-urls automatically';
strReady='Ready.';
strConvertPkg='You want to install an %x-Package, but your Linux-Distribution''s package system is %y.'#13'This package can be converted using "alien", but this will take some time and eventually the application won''t work'#13'Do you want to convert the package now?';
strConvertPkgQ='Convert package?';
strConvTitle='Converting %p package...';
strFiltering='Filtering...';
strFilter='Filter:';
strLoading='Loading...';
strDispRootApps='Display system applications';
strDispOnlyMyApps='Display my applications';
strSelMgrMode='Select software-manager mode:';
strListallerAlien='Listaller uses "alien" to convert foreign packages, bit the tool is not installed'#13'Do you want to install "alien" now to continue?';
strInstPkgQ='Install package?';
strPkgInstFail='Package %p could not be installed.';
strShowPkMon='Start PackageKit monitor before running transactions';
strAboutListaller='About Listaller';
strRmSrcQ='Are you really sure that you want to delete this source?';
strRmSrcQC='Delete source';
strPkitProbPkMon='Problem while connecting PackageKit. Run "pkmon" to get further information.';
strNoAppsFound='No applications found!';
//Catalogue
strCategory='Category:';
strWInstallDl='Select software you want to download and to install:';
strNoInfo='No information available!';
strDLSetUp='Downloading set-up...';
strErrContactMan='Cannot download this package. Please contact the catalogue managers on %h';
strInstalling='Running application installation...';
strDownloadCTbase='Downloading catalogue base information...';
strOpenPage='Loading catalogue page...';
strctDLAbort='Do you really want to abort this download?';
//Uninstall
strRealUninstQ='Do you really want to uninstall %a?';
strUnistSuccess='Application uninstalled successfully!';
strRMerror='Error while uninstalling!';
strCannotHandleRM='This application is not a MoJo-Installation and no other package-type Listaller can handle.';
strRMUnsdDeps='Uninstalling unused deps...';
strUninstalling='Uninstalling...';
strRMPkg='Do you really want to remove "%p", containing %a?'#13'The following package(s) will be removed also: %pl'#13'If you aren''t sure that you won''t need those packages, press "No"!';
strRmPkgQ='Really remove?';
strWaiting='Waiting...';
strRMAppC='Uninstalling %a';
{Updater
---------}
strNoUpdates='There are no updates available!';
strLogUpdInfo='Update info:';
strFilesChanged='%f files will be changed.';
strUpdTo='The application will be updated to version %v';
strCheckForUpd='Check for updates';
strInstUpd='Install updates';
strShowUpdater='Show';
strQuitUpdater='Quit';
strUpdInstalling='Installing updates...';
strUpdConfError='Error while unpacking and configuring files.';
implementation
end.

