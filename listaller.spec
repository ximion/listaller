Name:             listaller-core
Version:          0.3.77+git20100512
Release:          1
License:          GPLv3
BuildRequires:    fpc >= 2.4.0
BuildRequires:    lazarus >= 0.9.29
%if 0%{?suse_version} >= 1110  
BuildRequires:    libpackagekit-glib2-devel >= 0.5.6
%else
BuildRequires:    PackageKit-glib-devel >= 0.5.6
%endif
BuildRequires:    glib2-devel, gtk2-devel, fpc-src, libQt4Pas5-devel, sqlite-devel, polkit-devel

Source0:          listaller-gitsnapshot.20100512.tar.gz

Requires:         xdg-utils

Provides:         listaller
Group:            Applications/System
Summary:          Listaller core files
Vendor:           Listaller-Project
URL: 		  http://listaller.nlinux.org
BuildRoot:	  %{_tmppath}/build-%{name}-%{version}

%description
Listaller is a cross-distribution install system.
This package contains all files used by every Listaller module.
It provides the Listaller daemon and the
non-gui software installation tool "lipa".


%prep
%setup -c

%build
cd ./listaller-gitsnapshot
#Build for all widgetsets
./configure --enable-gtk --enable-qt --enable-creator
make

%install
cd ./listaller-gitsnapshot

mkdir -p %{_tmppath}/build-%{name}-%{version}/usr/bin
make install DESTDIR=%{buildroot}

%clean
cd ./listaller-gitsnapshot
make clean

%files
%defattr(-,root,root)
%dir "/usr/bin"
/usr/bin/lipa
/usr/share/dbus-1/system-services/org.nlinux.Listaller.service
/etc/dbus-1/system.d/org.nlinux.Listaller.conf
/usr/share/polkit-1/actions/org.nlinux.listaller.policy
/usr/sbin/listallerd

%package -n listaller-data
Group:            Applications/System
Summary:          Listaller data
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-data
Listaller is a distribution-independ software install system.
The application is able to uninstall every application on your
system you want to be removed, including LOKI/Mojo and Autopackages.
This package also includes the IPK installer tool.
Listaller tries to be compatible to all available Linux distributions
and uses PackageKit as backend.
This package contains platform and widgetset independent data of Listaller.

%post -n listaller-data
echo "Installing mime extensions..."
xdg-mime install '/usr/share/listaller/mime/x-ipk.xml'
xdg-mime install '/usr/share/listaller/mime/x-ips.xml'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-installation'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'application-ips-script'
update-desktop-database
echo "Done."


%preun -n listaller-data
echo "Uninstalling mime extension..."
xdg-mime uninstall '/usr/share/listaller/mime/x-ipk.xml'
xdg-mime uninstall '/usr/share/listaller/mime/x-ips.xml'
xdg-icon-resource uninstall --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-installation'
xdg-icon-resource uninstall --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'application-ips-script'
update-mime-database '/usr/share/mime'
echo "Done."

%files -n listaller-data
%defattr(-,root,root)
/usr/share/listaller/graphics
/usr/share/listaller/mime
/usr/share/pixmaps/listaller.png
/usr/share/listaller/locale
/etc/lipa

%package -n libInstaller-0.4
Requires:         listaller-core, PackageKit, PolicyKit
Group:            Applications/System
Summary:          Listaller library
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n libInstaller-0.4
Contains the libInstaller library, which allows
programs to access install/uninstall functions
of Listaller.

%files -n libInstaller-0.4
%defattr(-,root,root)
%dir "/usr/bin"
/usr/lib/libinstaller.so.*

%package -n listaller-gtk
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller-data, listaller-core
Group:            Applications/System
Summary:          Listaller frontends (GTK2)
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-gtk
Listaller is a distribution-independ software install system.
The application can be used to uninstall every application on your
system you want to be removed, including LOKI/Mojo and Autopackages.
This package also includes the IPK installer tool.
Listaller tries to be compatible to all available Linux distributions
and uses PackageKit as backend.
Please note that this alpha-version should only be used for testing purposes.

%files -n listaller-gtk
%defattr(-,root,root)
%dir "/usr/bin"
/usr/lib/listaller/gtk2/listallmgr
/usr/lib/listaller/gtk2/listallgo
/usr/lib/listaller/gtk2/liupdate
/usr/lib/listaller/gtk2/litray

%dir "/usr/share/applications"
/usr/share/applications/listaller-manager-gnome.desktop
/usr/bin/listallmgr-gtk

%package -n listaller-tools
Requires:         listaller-core
Group:            Applications/System
Summary:          Listaller package tools
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-tools
This package contains everything you need to build own IPK packages.
It also contains the needed tools to build RPM and DEB packages from
IPS sources and to  create an "Linux distribution compatible"
button for your software.

%files -n listaller-tools
%defattr(-,root,root)
/usr/bin/libuild
/usr/share/listaller/graphics/libutton

%package -n listaller-qt
Requires:         listaller-core, listaller-data, oxygen-icon-theme
Group:            Applications/System
Summary:          Listaller frontends (Qt4)
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-qt
Listaller is a distribution-independ software install system.
The application can be used to uninstall every application on your
system you want to be removed, including LOKI/Mojo and Autopackages.
This package also includes the IPK installer tool.
Listaller tries to be compatible to all available Linux distributions
and uses PackageKit as backend.
Please note that this alpha-version should only be used for testing purposes.

%files -n listaller-qt
%defattr(-,root,root)
%dir "/usr/bin"
/usr/lib/listaller/qt4/listallmgr
/usr/lib/listaller/qt4/listallgo
/usr/lib/listaller/qt4/liupdate
/usr/lib/listaller/qt4/litray

%dir "/usr/share/applications"
/usr/share/applications/listaller-manager-kde.desktop
/usr/bin/listallmgr-qt

%package -n listaller-creator-qt
Requires:         listaller-core, listaller-data, oxygen-icon-theme, listaller-tools
Group:            Applications/System
Summary:          Listaller Creator (Qt4)
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-creator-qt
Listaller is a cross-distribution software install system.
This is an alpha-release of Listaller's graphical package creation tool.
With liCreator you can easily build your own IPK-packages for
the Listaller system.
Please note that this is an alpha-release!

%files -n listaller-creator-qt
%defattr(-,root,root)
/usr/bin/licreator-qt
/usr/share/applications/licreator-qt.desktop
/opt/appfiles/liCreator/licreator-qt
/opt/appfiles/liCreator/listaller_creator-qt.png

%package -n listaller-creator-gtk
Requires:         listaller-core, listaller-data, listaller-tools
Group:            Applications/System
Summary:          Listaller Creator (GTK2)
Vendor:           Listaller-Project
URL:              http://listaller.nlinux.org

%description -n listaller-creator-gtk
Listaller is a cross-distribution software install system.
This is an alpha-release of Listaller's graphical package creation tool.
With liCreator you can easily build your own IPK-packages for the
Listaller system.
Please note that this is an alpha-release!

%files -n listaller-creator-gtk
%defattr(-,root,root)
/usr/bin/licreator-gtk
/usr/share/applications/licreator-gtk.desktop
/opt/appfiles/liCreator/licreator-gtk
/opt/appfiles/liCreator/listaller_creator-gtk.png
