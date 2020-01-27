# Add the source files
SET(GUI_src ${SRCGUI}/gui.c)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${GUIEXE} ${GUI_src})

TARGET_LINK_LIBRARIES(${GUIEXE} ${IPFGUILIB} ${IPCLIB} m Mrm Xm Xmu Xt Xext X11 SM ICE)

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INCLUDE_DIRECTORIES(${SRC}/include)

INSTALL(TARGETS ${GUIEXE} RUNTIME DESTINATION bin)
