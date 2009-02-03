Name:             listaller-gtk
Version:          0.1.87a
Release:          1
License:          GPLv3 and LGPLv3
BuildRequires:    fpc, lazarus, wget, glib2-devel, gtk2-devel, glib-devel, glib2, glib, fpc-src, gtk2, xorg-x11-devel, gdk-pixbuf, gdk-pixbuf-devel
Source0:          listaller-gtk2_0.1.87.tar.gz
Requires:         gtk2, cairo, glib2, gdk-pixbuf, xdg-utils, libgnomesu, lsb, glib
Provides:         listaller
Group:            Applications/System
Summary:          Listaller main package (GTK)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org
BuildRoot: %{_tmppath}/build-%{name}-%{version}

%description
Listaller is a distribution-independ software install system.
This is an alpha-release of Listaller. With this package you can uninstall every application on your system and install ipk-packages, LOKI/Mojo and Autopackages easily. 
Listaller tries to be compatible to all available Linux distributions and install- and package-systems.
  Please note that this alpha-version should be used for testing purposes.

%post
echo "Installing mime extensions..."
xdg-mime install '/usr/share/mime/packages/x-ipk.xml'
xdg-mime install '/usr/share/mime/text/x-ips.xml'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-ipk'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'text-x-ips'
echo "Done."


%preun
echo "Uninstalling mime extension..."
xdg-mime uninstall '/usr/share/mime/packages/x-ipk.xml'
xdg-mime uninstall '/usr/share/mime/text/x-ips.xml'
xdg-icon-resource uninstall --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-ipk'
xdg-icon-resource uninstall --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'text-x-ips'
update-mime-database '/usr/share/mime'
echo "Done."


%prep
%setup -c

%build
ARCH=$(uname -m)
case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;

esac

cd ./listaller-base.gtk2
make all
make licreator

%install
cd ./listaller-base.gtk2
./install.sh DESTDIR=%{_tmppath}/build-%{name}-%{version}
make DESTDIR=%{_tmppath}/build-%{name}-%{version}-%{rel} licreator-inst
make DESTDIR=./debian/listaller-tools litools-inst

%clean
cd ./listaller-base.gtk2
make clean

%files
%dir "/usr/bin"
/etc/lipa/blacklist
/usr/share/listaller/listallgo
/usr/share/listaller/listallmgr
/usr/share/listaller/liupdate
/usr/share/listaller/graphics
/usr/share/listaller/pkitbind
/usr/share/listaller/lang

%dir "/usr/share/applications"
/usr/share/applications/listaller-manager.desktop
/usr/share/mime/packages/x-ipk.xml
/usr/share/mime/text/x-ips.xml
/usr/share/mime-info/listaller-pack.mime
/usr/share/listaller/listallmgr
/usr/bin/lipa
/usr/bin/listallmgr
/usr/share/pixmaps/listaller.png

%package -n listaller-creation-gtk
Release:          1
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller
Group:            Applications/System
Summary:          Listaller creator (GTK)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-creation-gtk
Listaller is a distribution-independ software install system.
This is an alpha-release of Listaller's package creation tool.
With liCreator you can easily build your own IPK-packages for the Listaller system.
 Please note that this alpha-version should be used for testing purposes.

%files -n listaller-creation-gtk
%defattr(-,root,root)
/usr/appfiles/liCreator/
/usr/share/applications/licreator.desktop
/usr/bin/licreator

%package -n listaller-tools
Release:          1
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller
Group:            Applications/System
Summary:          Command-line tools for Listaller and IPK packages
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-tools
This package contains everything you need to build own IPK packages.
It also contains the needed tools to build RPM and DEB packages from IPS sources and to create an "Linux distribution compatible" button.

%files -n listaller-tools
%defattr(-,root,root)
/usr/share/listaller/graphics/libutton/
/usr/bin/lipa
/usr/share/listaller/unibuild
