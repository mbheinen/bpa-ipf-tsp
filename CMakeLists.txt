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

# Since gcc 4.9 the LTO format is non-standard (slim), so we need the gcc-specific ar and ranlib binaries
IF(CMAKE_C_COMPILER_ID STREQUAL "GNU" AND NOT (CMAKE_C_COMPILER_VERSION VERSION_LESS 4.9.0) AND NOT OPENBSD AND NOT DRAGONFLY)
    # When invoking cmake on distributions on which gcc's binaries are prefixed
    # with an arch-specific triplet, the user must specify -DCHOST=<prefix>
    if (DEFINED CHOST)
        set(CMAKE_AR "${CHOST}-gcc-ar")
        set(CMAKE_RANLIB "${CHOST}-gcc-ranlib")
    else()
        set(CMAKE_AR "gcc-ar")
        set(CMAKE_RANLIB "gcc-ranlib")
    endif()
ENDIF()

#set(RELEASE_FLAGS "${RELEASE_FLAGS} -flto")

#set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${DEBUG_FLAGS}")
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${DEBUG_FLAGS}")
#set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${RELEASE_FLAGS}")

# Define libraries
SET(IPFLIB ipf)
SET(IPCLIB ipc)
SET(TSPLIB libtsp)
SET(CFLOWLIB cflow)
SET(IPFGUILIB ipfgui)

# Define executables
SET(BPFEXE bpf)
SET(IPFBATEXE ipfbat)
SET(IPFREPORTSEXE ipf_reports)
SET(IPFTESTEXE ipf_test)
SET(IPFPLOTEXE ipfplot)
SET(IPFSRVEXE ipfsrv)
SET(GUIEXE gui)
SET(FINDOUTEXE findout)
SET(MIMICEXE mimic)
SET(PVCURVEEXE pvcurve)
SET(POSTPVCURVEEXE post_pvcurve)
SET(QVCURVEEXE qvcurve)
SET(LINEFLOWEXE lineflow)
SET(GPFEXE gpf)
SET(PUFEXE puf)
SET(NETDATEXE netdat)
SET(IPFCUTEXE ipfcut)
SET(TSPEXE tsp)

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR})
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
SET(SRCIPF ${SRC}/ipf)
SET(SRCIPC ${SRC}/ipc)
SET(SRCGUI ${SRC}/gui)
SET(SRCLIBTSP ${SRC}/libtsp)
SET(SRCLIBCFLOW ${SRC}/cflow)
SET(SRCLIBGUI ${SRC}/libgui)
SET(SRCBPF ${SRC}/bpf)
SET(SRCIPFBAT ${SRC}/ipfbat)
SET(SRCIPFREPORTS ${SRC}/ipf_reports)
SET(SRCIPFSRV ${SRC}/ipfsrv)
SET(SRCIPFTEST ${SRC}/ipf_test)
SET(SRCIPFPLOT ${SRC}/ipfplot)
SET(SRCGPF ${SRC}/gpf)
SET(SRCGUI ${SRC}/gui)
SET(SRCPUF ${SRC}/puf)
SET(SRCNETDAT ${SRC}/netdat)
SET(SRCIPFCUT ${SRC}/ipfcut)
SET(SRCTSP ${SRC}/tsp)

# Put .mod files in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

# The source for the libraries put in the lib folder
ADD_SUBDIRECTORY(${SRCIPF} ${LIB}/ipf)
ADD_SUBDIRECTORY(${SRCIPC} ${LIB}/ipc)
ADD_SUBDIRECTORY(${SRCLIBTSP} ${LIB}/libtsp)
ADD_SUBDIRECTORY(${SRCLIBGUI} ${LIB}/libgui)
ADD_SUBDIRECTORY(${SRCLIBCFLOW} ${LIB}/cflow)

# The source for the executables put in the bin folder
ADD_SUBDIRECTORY(${SRCBPF} ${BIN})
ADD_SUBDIRECTORY(${SRCIPFBAT})
ADD_SUBDIRECTORY(${SRCIPFREPORTS})
ADD_SUBDIRECTORY(${SRCIPFSRV})
ADD_SUBDIRECTORY(${SRCGUI})
ADD_SUBDIRECTORY(${SRCIPFTEST})
ADD_SUBDIRECTORY(${SRCIPFPLOT})
#ADD_SUBDIRECTORY(${SRCGPF})
ADD_SUBDIRECTORY(${SRCPUF})
ADD_SUBDIRECTORY(${SRCNETDAT})
#ADD_SUBDIRECTORY(${SRCIPFCUT})
ADD_SUBDIRECTORY(${SRCTSP})

# Tests
enable_testing()
add_test(NAME "bpf-bench.pfc"
         COMMAND bpf bench.pfc WORKING_DIRECTORY ../data)

add_test(NAME "bpf-new_system1.pfc"
         COMMAND bpf new_system1.pfc WORKING_DIRECTORY ../data)

add_test(NAME "bpf-new_system2.pfc"
         COMMAND bpf new_system2.pfc WORKING_DIRECTORY ../data)

add_test(NAME "bpf-test_gov.pfc"
         COMMAND bpf test_gov.pfc WORKING_DIRECTORY ../data)

add_test(NAME "bpf-testdc3.pfc"
         COMMAND bpf testdc3.pfc WORKING_DIRECTORY ../data)

add_test(NAME "ipf_reports-bench-selected-bus-branches"
         COMMAND echo -e "bench.bse\nO\nT\n1\nBUS1,230,1\nQ\nQ\nQ" | ipf_reports )

add_test(NAME "ipf_reports-bench-transmission-line-loadings"
         COMMAND echo -e "bench.bse\nO\nT\n5\nAll\n95\n\nQ\nQ\nQ" | ipf_reports )

add_test(NAME "ipf_reports-bench-transformer-loadings"
         COMMAND echo -e "bench.bse\nO\nT\n6\nAll\n95\n0\nQ\nQ\nQ" | ipf_reports )