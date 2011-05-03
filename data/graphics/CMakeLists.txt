# CMakeLists to install graphics for Listaller

# Mimetypes
install(FILES package-x-ipk.png
	DESTINATION share/icons/hicolor/32x32/mimetypes)
# Install to /pixmaps too
# FIXME: Should not install to two directories, find policy for this
install(FILES package-x-ipk.png
	DESTINATION share/pixmaps)

# Graphics for "Distribution Tested" button
install(FILES
	libutton/left.png
	libutton/firstblock.png
	libutton/block.png
	libutton/lastblock.png
	DESTINATION share/listaller/graphics/libutton/
)

install(DIRECTORY libutton/distro
	DESTINATION share/listaller/graphics/libutton
	FILES_MATCHING PATTERN "*.png"
)
