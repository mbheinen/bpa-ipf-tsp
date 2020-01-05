#

TOP=/usr
TSPLIBDIR = /shr/tsp-6.07/tsp

CC=cc
FOR=g77

CINCLUDES =	-I.\
		-I.. \
		-I$(TOP)/include \
		-I$(TOP)/lib \
		-I/usr/include/mit \
		-I/shr/tsp-6.07/utils \
		-I/shr/tsp-6.07/tsp

CFLAGS =        -g $(CINCLUDES) -DDUNDERSCORE
FFLAGS =        -g -DDUNDERSCORE -fno-f2c
LINTFLAGS=      $(CINCLUDES)
LDFLAGS =       -g -L../tsp
 
LIBS = -lg2c -lm
TSPLIBS =		-ltsp

OBJS = sds.o \
	gtswdta.o

# The following are powerflow modules to be included from the LIBIPF
# file.

all : sds

sds: $(OBJS)
	echo "Linking sds"
	$(CC) $(LDFLAGS) \
	$(OBJS) \
	$(TSPLIBS) \
	-o sds
	@size sds

.f.o:
	echo "Compiling $*.f "
	$(FOR) $(FFLAGS) -c $*.f

.c.o:
	echo "Compiling $*.c "
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -c $*.c


# DO NOT DELETE THIS LINE -- make depend depends on it.
