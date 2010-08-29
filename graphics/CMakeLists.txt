# CMakeLists to install graphics for Listaller

# Mimetypes
install(FILES
	mime-ipk.png
	mime-ips.png
	DESTINATION share/pixmaps/)
	
# Graphics for "Distribution Tested" button
install(FILES
	libutton/left.png
	libutton/firstblock.png
	libutton/block.png
	libutton/lastblock.png
	DESTINATION share/listaller/graphics/libutton/
)
install(DIRECTORY libutton/distro
	DESTINATION share/listaller/graphics/libutton/distro/
	FILES_MATCHING PATTERN "*.png"
)

# Frontend graphics	
if(FRONTENDS)

  install(FILES
	  wizardimage.png
	  spackage.png
	  throbber.gif
	  icon48-appremove.png
	  icon48-catalog.png
	  icon48-repository.png
	  icon48-settings.png
	  DESTINATION share/listaller/graphics/
	  COMPONENT frontends)

  install(FILES
	  categories/accessories.png
	  categories/all.png
	  categories/development.png
	  categories/games.png
	  categories/graphics.png
	  categories/internet.png
	  categories/multimedia.png
	  categories/office.png
	  categories/other.png
	  categories/science.png
	  categories/system.png
	  DESTINATION share/listaller/graphics/categories/
	  COMPONENT frontends)
  
  install(FILES listaller.png DESTINATION share/pixmaps/)

endif(FRONTENDS)