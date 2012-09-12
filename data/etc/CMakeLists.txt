# Install standard configuration

# not-to-be-edited config files
install(FILES default-dependencies.list
	DESTINATION ${DATA_INSTALL_DIR}
	COMPONENT config
)

# changeable config files
install(FILES dependencies.list
	  Listaller.conf
	DESTINATION ${SYSCONF_INSTALL_DIR}/listaller
	COMPONENT config
)

# GPG stuff for Listaller keyring
install(FILES gpg.conf
	DESTINATION /var/lib/listaller/keyring
	COMPONENT config
)
