# Install standard configuration

# changeable config files
install(FILES Listaller.conf
	      depscan-ignore.list
	DESTINATION ${SYSCONF_INSTALL_DIR}/listaller
	COMPONENT config
)
