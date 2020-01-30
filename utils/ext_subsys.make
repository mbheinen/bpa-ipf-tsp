#

TOP=//d/cygnus/cygwin32-B20
TSPLIBDIR = //d/shr/tsp-6.07/tsp
IPFLIBDIR = //d/shr/ipf-3.25/ipf
LIBIPF = //d/shr/ipf-3.25/ipf/libipf.a

CC=gcc
FOR=g77

CINCLUDES =	-I.\
		-I.. \
		-I$(TOP)/lib \
		-I$(TOP)/lib/Xt

CFLAGS =        -g $(CINCLUDES) -DDUNDERSCORE
FFLAGS =        -g -DDUNDERSCORE -fno-f2c
LINTFLAGS=      $(CINCLUDES)
LDFLAGS =       -g -L../tsp -L../ipf -L$(TOP)
 
TSPLIBS =		-ltsp
IPFLIBS =		-lipf -lg2c -lm

OBJS = ext_subsys.o \
	getsubsys1.o \
	getsubsys2.o \
	load_sdi.o \
	newexctyp.o \
	is_it_vms.o \
	c_err_exit.o \
	icode.o

ext_subsys: $(OBJS)
	echo "Linking ext_subsys"
	$(CC) $(LDFLAGS) \
	$(OBJS) \
	-L$(TSPLIBDIR) -L$(IPFLIBDIR) \
	$(TSPLIBS) \
	$(IPFLIBS) \
	-o ext_subsys.exe
	@size ext_subsys.exe

.f.o:
	echo "Compiling $*.f "
	$(FOR) $(FFLAGS) -c $*.f

.c.o:
	echo "Compiling $*.c "
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -c $*.c


# DO NOT DELETE THIS LINE -- make depend depends on it.
