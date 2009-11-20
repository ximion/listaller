Name:             listaller-core
Version:          0.3.00b
Release:          1
License:          GPLv3
BuildRequires:    fpc >= 2.2.4, lazarus >= 0.9.27, glib2-devel, gtk2-devel, glib-devel, glib2, glib, fpc-src, gtk2, libqt4intf, sqlite-devel
Source0:          listaller-0.3.00b.tar.gz
%if 0%{?fedora_version} >= 10
Requires:         xdg-utils, PackageKit, beesu
%else
Requires:         xdg-utils, PackageKit
%endif

Provides:         listaller
Group:            Applications/System
Summary:          Listaller basic tools and libraries
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org
BuildRoot: %{_tmppath}/build-%{name}-%{version}

%description
This package contains the 'lipa' command line tool.
It allows installing/uninstalling/building IPK packages without GUI.


%prep
%setup -c

%build
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

cd ./listaller-0.3.00b

# Create GTK+ applications
make WIDGET=gtk2 all
make WIDGET=gtk2 licreator
# Now build Qt applications
make WIDGET=qt4 all
make WIDGET=qt4 licreator

%install
cd ./listaller-0.3.00b
# Necessary to get the right directories
%if 0%{?fedora_version} >= 10
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

mkdir -p /home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH/usr/bin
# Install architecture independend files
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH install-data

# Install cmd utilities
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH libuildtools-inst
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH install-lipa

# Install GTK+ binaries
make WIDGET=gtk2 DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH install
make WIDGET=gtk2 DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH licreator-inst
# Install Qt4 binaries
make WIDGET=qt4 DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH install
make WIDGET=qt4 DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH licreator-inst
%else
mkdir -p %{_tmppath}/build-%{name}-%{version}/usr/bin

# Install architecture independend files
make DESTDIR=%{_tmppath}/build-%{name}-%{version} install-data

# Install cmd utilities
make DESTDIR=%{_tmppath}/build-%{name}-%{version} libuildtools-inst
make DESTDIR=%{_tmppath}/build-%{name}-%{version} install-lipa
# Install GTK+ binaries
make WIDGET=gtk2 DESTDIR=%{_tmppath}/build-%{name}-%{version} install
make WIDGET=gtk2 DESTDIR=%{_tmppath}/build-%{name}-%{version} licreator-inst
# Install Qt4 binaries
make WIDGET=qt4 DESTDIR=%{_tmppath}/build-%{name}-%{version} install
make WIDGET=qt4 DESTDIR=%{_tmppath}/build-%{name}-%{version} licreator-inst
%endif

%clean
cd ./listaller-0.3.00b
make clean

%files
%defattr(-,root,root)
%dir "/usr/bin"
/usr/bin/lipa

%package -n listaller-data
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller
Group:            Applications/System
Summary:          Listaller data
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

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
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-ipk'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'text-ips-script'
update-desktop-database
echo "Done."


%preun -n listaller-data
echo "Uninstalling mime extensions..."
xdg-mime uninstall '/usr/share/listaller/mime/x-ipk.xml'
xdg-mime uninstall '/usr/share/listaller/mime/x-ips.xml'
xdg-icon-resource uninstall --context mimetypes --size 64 'mime-ipk' 'application-x-ipk'
xdg-icon-resource uninstall --context mimetypes --size 64 'mime-ips' 'text-ips-script'
update-mime-database '/usr/share/mime'
echo "Done."

%files -n listaller-data
%defattr(-,root,root)
/usr/share/listaller/graphics
/usr/share/listaller/mime
/usr/share/listaller/pkitbind
/usr/share/pixmaps/listaller.png
/usr/share/listaller/locale
/usr/share/mime-info/listaller-pack.mime
/etc/lipa

%package -n listaller-gtk
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller-data, listaller-core
Group:            Applications/System
Summary:          Listaller frontends (GTK2)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

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
Summary:          Command-line tools for Listaller package handling
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-tools
This package contains everything you need to build own IPK packages.
It also contains the needed tools to build RPM and DEB packages from
IPS sources and to  create an "Linux distribution compatible"
button for your software.

%files -n listaller-tools
%defattr(-,root,root)
/usr/bin/libuild
/usr/lib/listaller/unibuild
/usr/share/listaller/graphics/libutton

%package -n listaller-qt
Requires:         listaller-core, listaller-data, kdesudo, libqt4intf, kde-icons-oxygen
Group:            Applications/System
Summary:          Listaller frontends (Qt4)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

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
Requires:         listaller-core, listaller-data, kdesudo, libqt4intf, kde-icons-oxygen, listaller-tools
Group:            Applications/System
Summary:          Listaller Creator (Qt4)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-creator-qt
Listaller is a cross-distribution software install system.
This is an alpha-release of Listaller's graphical package creation tool.
With liCreator you can easily build your own IPK-packages for
the Listaller system.
Please note that this is an alpha-release!

%files -n listaller-creator-qt
%defattr(-,root,root)
/opt/appfiles/liCreator/
/usr/share/applications/licreator.desktop
/usr/bin/licreator

%package -n listaller-creator-gtk
Requires:         listaller-core, listaller-data, listaller-tools
Group:            Applications/System
Summary:          Listaller Creator (GTK2)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-creator-gtk
Listaller is a cross-distribution software install system.
This is an alpha-release of Listaller's graphical package creation tool.
With liCreator you can easily build your own IPK-packages for the
Listaller system.
Please note that this is an alpha-release!

%files -n listaller-creator-gtk
%defattr(-,root,root)
/opt/appfiles/liCreator/
/usr/share/applications/licreator.desktop
/usr/bin/licreator
