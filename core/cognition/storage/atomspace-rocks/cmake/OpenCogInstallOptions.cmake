# OpenCogInstallOptions.cmake - Installation options for OpenCog
#
# This file sets up installation paths and options

include(GNUInstallDirs)

# Set default installation prefix
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "Installation prefix" FORCE)
endif()

# Installation directories
set(DATADIR "${CMAKE_INSTALL_DATAROOTDIR}/opencog" CACHE PATH "Data directory")
set(CONFDIR "${CMAKE_INSTALL_SYSCONFDIR}/opencog" CACHE PATH "Configuration directory")

# Install paths for different components
set(INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_INCLUDEDIR}")
set(LIB_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}")
set(BIN_INSTALL_DIR "${CMAKE_INSTALL_BINDIR}")
set(DATA_INSTALL_DIR "${DATADIR}")
set(CONF_INSTALL_DIR "${CONFDIR}")

# CMake config files installation directory
set(CMAKE_INSTALL_DIR "${LIB_INSTALL_DIR}/cmake/AtomSpace")
