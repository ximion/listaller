cd "./"
debuild -rfakeroot
cd "./debian"
rpmbuild -ba '../listaller-0.1.16a.spec'
