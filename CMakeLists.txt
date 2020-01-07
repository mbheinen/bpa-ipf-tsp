CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(bpa-ipf-tsp Fortran C)

# Set bpa-ipf-foo version
SET(VERSION 0.0.2)

# Add local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

# Define libraries
SET(IPFLIB ipf)
SET(IPCLIB ipc)
SET(TSPLIB libtsp)

# Define executables
SET(BPFEXE bpf)
SET(IPFBATEXE ipfbat)
SET(IPFREPORTSEXE ipf_reports)
SET(IPFTESTEXE ipf_test)
SET(IPFPLOTEXE ipfplot)
SET(IPFSRVEXE ipfsrv)
SET(GUIEXE gui)
SET(PUFEXE puf)
SET(NETDATEXE netdat)
SET(CUTTINGEXE cutting)
SET(TSPEXE tsp)

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
SET(SRCIPF ${SRC}/ipf)
SET(SRCIPC ${SRC}/ipc)
# SET(SRCLIBTSP ${SRC}/libtsp)
SET(SRCBPF ${SRC}/bpf)
# SET(SRCIPFBAT ${SRC}/ipfbat)
# SET(SRCIPFREPORTS ${SRC}/ipf_reports)
# SET(SRCIPFTEST ${SRC}/ipf_test)
# SET(SRCIPFPLOT ${SRC}/ipfplot)
# SET(SRCIPFSRV ${SRC}/ipfsrv)
# SET(SRCGUI ${SRC}/gui)
# SET(SRCPUF ${SRC}/puf)
# SET(SRCNETDAT ${SRC}/netdat)
# SET(SRCCUTTING ${SRC}/cutting)
# SET(SRCTSP ${SRC}/tsp)

# Put .mod files in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

# The source for the libraries put in the lib folder
ADD_SUBDIRECTORY(${SRCIPF} ${LIB}/ipf)
ADD_SUBDIRECTORY(${SRCIPC} ${LIB}/ipc)

# The source for the executables put in the bin folder
ADD_SUBDIRECTORY(${SRCBPF} ${BIN})
