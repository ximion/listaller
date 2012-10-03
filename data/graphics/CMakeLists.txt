# CMakeLists to install graphics for Listaller

# Mimetypes
install(FILES package-x-ipk_32.png
	RENAME package-x-ipk.png
	DESTINATION share/icons/hicolor/32x32/mimetypes)
install(FILES package-x-ipk_48.png
	RENAME package-x-ipk.png
	DESTINATION share/icons/hicolor/48x48/mimetypes)

# Graphics for "Distribution Tested" button
#install(FILES
#	libutton/left.png
#	libutton/firstblock.png
#	libutton/block.png
#	libutton/lastblock.png
#	DESTINATION share/listaller/graphics/libutton/
#)
#
#install(DIRECTORY libutton/distro
#	DESTINATION share/listaller/graphics/libutton
#	FILES_MATCHING PATTERN "*.png"
#)
