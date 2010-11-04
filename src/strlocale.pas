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
//** This unit contains the default strings for translation
unit strlocale;

{$mode objfpc}{$H+}

interface

resourcestring
  rsBrokenDepsFixQ = 'Some dependencies are missing. Should they be installed now?';
  rsCannotResolv = 'Cannot resolve dependencies';
  rsCheckAppDepsQ = 'Do you want to check the applications''s dependencies?';
  rsCheckDepsQ = 'Check dependencies?';
  rsCheckApps = 'Check applications';
  rsCheckRootAppsQ = 'Do you want to check global-installed applications too?';
  rsCommands = 'Commands:';
  rsCouldntFindUpdater = 'Can''t find update-tool!';
  rsInternalError = 'An internal error occured';
  rsLaunchLiMgr = 'Launch Listaller Manager';
  rsLipaInfo2 = 'Resolve Listaller path variable';
  rsLipaInfo3 = 'Installs an IPK package';
  rsListallerMgrNotFound = 'Can''t find Listaller Manager!';
  rsUnableFind = 'Unable to find %s';
  rsNotFoundliTray = 'Could not find Listaller TrayIcon Application';
  rsOptions = 'Options:';
  rsLipaInfo4 = 'Runs installation in testmode';
  rsLipaInfo5 = 'Print all available log messages';
  rsLipaInfo6 = 'Check if dependencies of all installed applications are available.';
  rsCMDInfoPkgBuild = 'Package build commands:';
  rsLiBuildInfoA = 'Build IPK-package';
  rsLiBuildInfoB = 'Create/Update update-repository';
  rsLiBuildInfoC = 'Create DEB and RPM file from IPS';
  rsDone = 'Done.';
  rsDoYouAcceptLicenseCMD = 'Do you accept this license [y/n]?';
  rsEnterNumber = 'You have to enter a number!';
  rsInstAborted = 'Installation aborted.';
  rsInstAbortedByUser = 'Installation aborted by user.';
  rsAppNInstall = 'The application "%a" was not installed.';
  rsLipaAutoFixQ = 'Should lipa fix these problems automatically [y/n]?';
  rsLipaInfo1 = 'Listaller command-line tool to handle IPK-packages';
  rsModeNumber = 'Mode number:';
  rsN = 'n';
  rsNo = 'no';
  rsOkay = 'Okay';
  rsPreparingInstall = 'Preparing installation (please wait)';
  rsRootPassAdvancedPriv = 'Enter your password to run the application with '
    + 'advanced privileges.';
  rsRootPassQAppEveryone = 'Enter your password to install the application for '
    + 'everyone.';
  rsConvDone = 'Conversion done. Do you want to close this window now?'#10'Press "No" if you want to check the output.';
  rsSelectListNumber = 'Please select a number shown in the list!';
  rsSelectIModeA = 'Select the installation mode of this application:';
  rsShowDetailedInfoCMD = 'Do you want to see detailed information [y/n]?';
  rsAborted = 'Aborted.';
  rsViewLogQ = 'Do you want to view the logfile?';
  rsWasInstalled = 'The application %a was installed successfully!';
  rsCNFindAppExecutable = 'Could not find main executable of this application!';
  rsExecAppTesting = 'Executing application main executable...';
  rsInstallNow = 'Install now';
  rsRunParam = 'Please run "listallgo" with path to setup-package as first parameter!';
  rsWelcome = 'Welcome!';
  rsnToStart = 'Press "Next" to start the installation!';
  rsProgDesc = 'Program description:';
  rsLicense = 'Software license';
  rspleaseRead = 'Please read the following information carefully:';
  rsRunning = 'Running installation...';
  rsplWait = 'Please wait.';
  rsComplete = 'Installation completed!';
  rsInstFailed = 'Installation failed.';
  rsPrFinish = 'Press "Finish" to close.';
  rsFinish = 'Finish';
  rsAbort = 'Abort';
  rsBack = 'Back';
  rsNext = 'Next';
  rsFile = 'File';
  rsDispLog = 'Display installation log';
  rsIagree = 'I agree with the above terms and conditions';
  rsInagree = 'I don''t agree';
  rsLDnSupported = 'Your Linux distribution is not supported by Listaller yet!';
  rsnSupported = 'This package does not support your Linux distribution.';
  rsDepPkgsNotFound = 'No packages containing the following libraries were found: %l';
  rsResolvingDynDeps = 'Resolving dynamic dependencies...';
  rsDBConnClosed = 'Database connection closed.';
  rsDBOpened = 'Software database opened.';
  rsInClose = 'The installer will close now';
  rsCnOverride = 'Unable to override the file %f.';
  rsCnCopy = 'Unable to copy the file %s.';
  rsNotifyDevs = 'Please notify the developers on http://launchpad.net/listaller';
  rsExtractError = 'Error while extracting files!';
  rsPkgDM = 'The package may be damaged or you haven''t enough permissions to execute this action!';
  rsUnknownErrorOC = 'Unknown error occured!';
  rsAbLoad = 'Loading aborted.';
  rsPackageIsUnsigned = 'Package is unsigned.';
  rsPackageHasTrustedSign = 'Package has trusted signature.';
  rsPackageSigIsUntrusted = 'Package signature is UNTRUSTED!';
  rsOneInstAtTime = 'You cannot install two applications at the same time.' +
    #10'Please finish the first installation, then install this one again.' +
    #10'(If you do not run two installations, try to remove the temporary installer directory.)';
  rsAlreadyInst = 'This application is already installed';
  rsInstallAgain = 'Do you want to install it again?';
  rsWelcomeTo = 'Welcome to the installation of %a';
  rsInstOf = 'Installation of %a';
  rsTestmode = 'Testmode';
  rsTestFinished = 'Test-installation of package finished.';
  rsCleaningUp = 'Cleaning up...';
  rsNotExecAsRootPolKit = 'Please do not execute this action with superuser rights!'
    + #10'(PolicyKit will ask for authorization if privileges are required.)'
    + #10'Do you want to force Listaller to execute all with superuser permission?';
  rspkgInval = 'The submitted package is invalid!';
  rsCouldntSolve = 'Dependencies couldn''t be solved!';
  rsViewLog = 'Please view the logfile at %p';
  rsPKGError = 'Installation package is corrupt';
  rsAppClose = 'The application will close now.';
  rsStep1 = 'Phase 1/4: Resolving dependencies...';
  rsState = 'State';
  rsWDLdep = 'This application wants to fetch a dependency from %l';
  rswAllow = 'Do you want confirm this download of external files?';
  rsCouldNotDetectPkgType = 'Could not detect package type!';
  rsLiCloseANI = 'Listaller will close now. The package couldn''t be installed.';
  rsStep2 = 'Phase 2/4: Installing files...';
  rsStep3 = 'Phase 3/4: Chmod new files...';
  rsStep4 = 'Phase 4/4: Registering application...';
  rsTestmodeDNRegister = 'Testmode: Do not register package.';
  rsRightsAssignedToX = 'Rights assigned to %a.';
  rsAddUpdSrc = 'The software provides the following update source:';
  rsQAddUpdSrc = 'Should this repository be registered to get software updates?';
  rsFinished = 'Finished';
  rsSecurityInfo = 'Please install this package only if you got it from a '#10 +
    'trusted source. There is a serious risk that the software in this package'#10 +
    'damages your system. (The package could also contain malware)'#10 +
    'Be careful with this package, especially if you install it system-wide'#10 +
    'with superuser rights.'#10;
  rsSecurityWarning = 'Security warning';
  rsIKnowTheRisk = 'I know the risk';
  rsinstAnyway = 'Do you want to install it anyway? (This could cause problems)';
  rsInvArchitecture =
    'The application which this package contains was not built for the current system architecture.';
  rsPackageTypeIsX = 'Package type is "%s"';
  rsRunBinAsRoot = 'Run binary setup file as root.';
  rsFoundInstallProfileX = 'Found installation profile %s.';
  rsWillDLFiles = '(This program will download the needed files from the internet)';
  rsInvalidDVersion = 'Package was not build for your Linux distribution release.';
  rsFTPfailed = 'Problem while downloading the required packages. Could not create working FTP connection.';
  rsSuccess = 'Success!';
  rsMain = 'Main';
  rsDetails = 'Details';
  rsInstPkgFailed = 'Installation of package %s failed!';
  rsECode = 'Code:';
  rsEMsg = 'Error message:';
  rsLookingForRevDeps = 'Looking for reverse-dependencies...';
  rsPackageDetected = 'Package detected: %s';
  rsInstallation = 'Installation';
  rsNoLDSources = 'There are no explicit package sources available for your Linux distribution.'#10'Try to install common packages?';
  rsUseCompPQ = 'Use compatible packages?';
  rsNoComp = 'No compatible packages found!';
  rsActionNotPossiblePkg = 'The selected action is not possible with this package.'#10'Please contact the package maintainer to get more information.';
  rsReInstall = 'Re-install';
  rsPkgDownload = 'The following dependencies will be downloaded:';
  rsGetDependencyFrom = 'Get dependency from';
  rsPlWait2 = 'Please wait...';
  rsDepDLProblem = 'Problem while downloading the dependency.';
  rsLookingForX = 'Looking for %a.';
  rsDownloadingPkg = 'Downloading package...';
  rsInstallingX = 'Installing %a...';
  rsHashError = 'Hash value doesn''t match!'#10'The package may be modified after creation.'#10'Please obtain a new copy!';
  rsInstallationMode = 'Installation mode:';
  rsIModeInstruction = 'Select which parts of the application should be installed.';
  rsMode = 'Mode';
  rsExecNewApp = 'Execute the new application';
  rsInstPerformError = 'Error while performing installation:';
  rsYesNo1 = 'Yes/No?:';
  rsInstallLiBuild = 'You have to install the "libuild" tool before you can build packages!';
  rsFileNotExists = 'The file "%f" does not exists!';

  rsWantToDoQ = 'What do you want to do?';
  rsDevVersion = 'You use a development version of Listaller!'#10'This version is not tested and may cause problems!';
  rsSpkWarning = 'Make sure that you got this package from a save source and from a serious publisher!';
  rsInstallEveryone = 'Install application for everyone';
  rsTestApp = 'Test setup && software';
  rsInstallHome = 'Install into my Home directory';
  rsSelInstMode = 'Select installation mode';

  rsSoftwareManager = 'Software Manager';
  rsUpdSources = 'Update repositories';
  rsClose = 'Close';
  rsThanksTo = 'Thanks to';
  rsDelSrc = 'Delete source';
  rsListofSrc = 'The following update repositories are installed:';
  rsUninstall = 'Uninstall';
  rsShow = 'Show:';
  rsAll = 'All';
  rsEducation = 'Education';
  rsOffice = 'Office';
  rsDevelopment = 'Development';
  rsGraphic = 'Graphic';
  rsNetwork = 'Network';
  rsGames = 'Games';
  rsSystem = 'System';
  rsMultimedia = 'Multimedia';
  rsAddidional = 'Utilities';
  rsOther = 'Other';
  rsVersion = 'Version';
  rsAuthor = 'Autor';
  rsUsername = 'Username';
  rsPassword = 'Password';
  rsProxySettings = 'Proxy-Settings';
  rsEnableProxy = 'Enable Proxy-Server';
  rsLOKIError = 'Unable to load information about LOKI-Setup.';
  rsCannotLoadIcon =
    'Unable to load the icon of %a. Please notify the developers of Listaller or of this application!';
  rsAutoLoadDep = 'Load dependencies from included webserver-urls automatically';
  rsReady = 'Ready.';
  rsConvertPkg = 'You want to install an %x-Package, but your Linux-distribution''s package management system is based on %y.'#10 + 'This package can be converted into a compatible format using "alien", but this will take some time and eventually the installed application won''t work properly'#10 + 'Do you want to convert the package now?';
  rsConvertPkgQ = 'Convert package?';
  rsConvTitle = 'Converting %p package...';
  rsFiltering = 'Filtering...';
  rsFilter = 'Filter...';
  rsLoading = 'Loading...';
  rsSkippedX = 'Skipped %a';
  rsApplications = 'Applications';
  rsInstalledApps = 'Installed applications';
  rsInstallPkg = 'Install package';
  rsRepositories = 'Repositories';
  rsPackageLists = 'Package lists';
  rsSettings = 'Settings';
  rsMyApps = 'My apps';
  rsSharedApps = 'Shared apps';
  rsNoGUIPkgManFound = 'Could not find usable GUI package manager.'#10'Please install a PackageKit-GUI!';
  rsVersionUnknown = 'Version: unknown';
  rsUseLaunchpadForBugs = 'Please use https://bugs.launchpad.net/listaller'#10'for bug reports.';
  rsListallerAlien = 'Listaller uses "alien" to convert foreign packages, but the tool is not installed'#10'Do you want to install "alien" now to continue?';
  rsInstPkgQ = 'Install package?';
  rsPkgInstFail = 'Package %p could not be installed.';
  rsShowPkMon = 'Start PackageKit monitor before running transactions';
  rsStartLiTray = 'Start Listaller tray icon application (e.g. to check dependencies)';
  rsAboutListaller = 'About Listaller';
  rsOpenDirsiCatalog = 'Open your distribution''s package catalog';
  rsAbout = 'About';
  rsAuthors = 'Authors';
  rsRmSrcQ = 'Are you really sure that you want to delete this source?';
  rsRmSrcQC = 'Delete source?';
  rsPkitProbPkMon = 'Problem while communicating with PackageKit. Run "pkmon" to get further information.';
  rsCallingPackageKitPKMonExecActions = 'Calling PackageKit... (run "pkmon" to monitor the executed actions)';
  rsDetectingPackage = 'Detecting package...';
  rsSourceDeleted = 'Source was removed.';
  rsPleaseSelectListItem = 'Please select an item from the list!';
  rsLiUpdateAccessFailed = 'Cannot access the Listaller Update tool. Maybe it is not installed?';
  rsChangePkgManSettings = 'Change package manager settings';

  rsRealUninstQ = 'Do you really want to uninstall %a?';
  rsUnistSuccess = 'Application was successfully removed!';
  rsRMerror = 'Error while uninstalling!';
  rsUnableToRemoveApp = 'Unable to remove this application!';
  rsCannotHandleRM =
    'This application does not look like a MoJo-Installation and it is not installed via any other package-type Listaller can handle.'#10'Get instructions how to remove this application from the program author.';
  rsReadingAppInfo = 'Reading application information...';
  rsAppRegistBroken = 'The registration of this package is broken!';
  rsRemovingApp = 'Removing application...';
  rsLOKISetupFound = 'LOKI setup found.';
  rsPkgCouldBeInstalledWithLoki = 'Package could be installed with MoJo/LOKI.';
  rsRMUnsdDeps = 'Uninstalling unused dependencies...';
  rsStartingUninstall = 'Starting uninstallation.';
  rsUninstalling = 'Uninstalling...';
  rsRMPkg = 'Do you really want to remove "%p", containing %a?'#10'The following package(s) will be removed also: %pl'#10'If you''re not sure if you need those packages, press "No"!';
  rsCheckDepsRegisteredApps = 'Checking dependencies of all registered '
    +'applications...';
  rsYouScanOnlyRootInstalledApps = 'You are scanning only the ROOT installed '
    +'applications.';
  rsYouScanOnlyLocalInstalledApps = 'You are scanning your local installed '
    +'applications.';
  rsInstalledDepX = 'Installed dependency %s';
  rsDepXIsNotInstall = 'Dependency "%s" is not installed!';
  rsRmPkgQ = 'Really remove?';
  rsWaiting = 'Waiting...';
  rsRMAppC = 'Uninstalling %a';
  rsLinDesk = 'Running under %s.';
  rsTranslators = 'List of translators:'#10;

  rsNoUpdates = 'There are no updates available!';
  rsLogUpdInfo = 'Update info:';
  rsFilesChanged = '%f files will be changed.';
  rsUpdTo = 'The application will be updated to version %v';
  rsCheckForUpd = 'Check for updates';
  rsInstUpd = 'Install updates';
  rsShowUpdater = 'Show';
  rsQuitUpdater = 'Quit';
  rsUpdInstalling = 'Applying updates...';
  rsUpdConfError = 'Error while unpacking and configuring files.';
  rsQuestion = 'Question:';
  rsYes = 'yes';
  rsY = 'y';
  rsPkQueryFailed = 'PackageKit transaction failed!';
  rsResolveError = 'There was an error during package resolving.';
  rsResolvingDep = 'Resolving dependencies...';
  rsPkgUnsigned = 'This package is not signed!';
  rsPkgUntrusted = 'This package is untrusted!';
  rsCouldNotInstallApp = 'The application %a could not be installed!';
  rsWarning = 'Warning!';
  rsTmpWriteDenied = 'Listaller cannot write into its temporary directory: ' +
    'Access denied!';
  rsError = 'Error';

implementation
end.

