Name:             listaller-gtk
Version:          0.2.00a
Release:          1
License:          GPLv3
BuildRequires:    fpc, lazarus, wget, glib2-devel, gtk2-devel, glib-devel, glib2, glib, fpc-src, gtk2
Source0:          listaller_0.2.00a.tar.gz
%if 0%{?fedora_version} >= 10
Requires:         gtk2, glib2, xdg-utils, PackageKit, redhat-config-rpm
%else
Requires:         gtk2, glib2, xdg-utils, PackageKit, libgnomesu
%endif

Provides:         listaller
Group:            Applications/System
Summary:          Listaller (GTK)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org
BuildRoot: %{_tmppath}/build-%{name}-%{version}

%description
Listaller is a distribution-independ software install system. The application can be used to uninstall every 
application on your system you want to be removed, including LOKI/Mojo and Autopackages.
This package also includes the IPK installer tool. Listaller tries to be compatible to 
all available Linux distributions and uses PackageKit as backend.
 Please note that this alpha-version should only be used for testing purposes.

%post
echo "Installing mime extensions..."
xdg-mime install '/usr/share/listaller/mime/x-ipk.xml'
xdg-mime install '/usr/share/listaller/mime/x-ips.xml'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ipk.png' 'application-x-ipk'
xdg-icon-resource install --context mimetypes --size 64 '/usr/share/listaller/graphics/mime-ips.png' 'text-ips-script'
update-desktop-database
echo "Done."


%preun
echo "Uninstalling mime extensions..."
xdg-mime uninstall '/usr/share/listaller/mime/x-ipk.xml'
xdg-mime uninstall '/usr/share/listaller/mime/x-ips.xml'
xdg-icon-resource uninstall --context mimetypes --size 64 'mime-ipk' 'application-x-ipk'
xdg-icon-resource uninstall --context mimetypes --size 64 'mime-ips' 'text-ips-script'
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

cd ./trunk
make all
make licreator

%install
cd ./trunk
%if 0%{?fedora_version} >= 10
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

mkdir -p /home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH/usr/bin
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH install
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH licreator-inst
make DESTDIR=/home/abuild/rpmbuild/BUILDROOT/%{name}-%{version}-%{release}.$ARCH litools-inst
%else
mkdir -p %{_tmppath}/build-%{name}-%{version}/usr/bin
make DESTDIR=%{_tmppath}/build-%{name}-%{version} install
make DESTDIR=%{_tmppath}/build-%{name}-%{version} licreator-inst
make DESTDIR=%{_tmppath}/build-%{name}-%{version} litools-inst
%endif

%clean
cd ./trunk
make clean

%files
%defattr(-,root,root)
%dir "/usr/bin"
/etc/lipa/blacklist
/usr/lib/listaller/listallmgr
/usr/lib/listaller/listallgo
/usr/lib/listaller/liupdate
/usr/share/listaller/

%dir "/usr/share/applications"
/usr/share/applications/listaller-manager.desktop
/usr/share/mime-info/listaller-pack.mime
/usr/bin/lipa
/usr/bin/listallmgr
/usr/share/pixmaps/listaller.png

%package -n listaller-creation-gtk
Release:          1
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller
Group:            Applications/System
Summary:          Listaller Creator (GTK)
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-creation-gtk
Listaller is a cross-distribution software install system.
This is an alpha-release of Listaller's graphical package creation tool.
With liCreator you can easily build your own IPK-packages for the Listaller system.
 Please note that this is an alpha-release!

%files -n listaller-creation-gtk
%defattr(-,root,root)
/usr/appfiles/liCreator/
/usr/share/applications/licreator.desktop
/usr/bin/licreator

%package -n listaller-tools
Release:          1
Requires:         gtk2, cairo, glib2, gdk-pixbuf, listaller
Group:            Applications/System
Summary:          Command-line tools for Listaller package handling
Vendor:           Listaller-Project
URL: http://listaller.nlinux.org

%description -n listaller-tools
This package contains everything you need to build own IPK packages.
It also contains the needed tools to build RPM and DEB packages from IPS sources and to 
create an "Linux distribution compatible" button for your software.

%files -n listaller-tools
%defattr(-,root,root)
/usr/share/listaller/graphics/libutton/
/usr/bin/lipa
/usr/lib/listaller/unibuild
