#

TOP=/usr
TSPLIBDIR = /shr/tsp-6.07/tsp
IPFLIBDIR = /shr/ipf-3.27/ipf
LIBIPF = /shr/ipf-3.27/ipf/libipf.a

CC=cc

CINCLUDES =	-I.\
		-I.. \
		-I$(TOP)/include \
		-I$(TOP)/lib \
		-I/usr/include/mit \
		-I/shr/tsp-6.07/tsp

CFLAGS =        -g $(CINCLUDES) -DDUNDERSCORE
FFLAGS =        -g -DDUNDERSCORE -fno-f2c
LINTFLAGS=      $(CINCLUDES)
LDFLAGS =       -g -L../tsp
 
LIBS = -lipf -lg2c -lm
TSPLIBS =		-ltsp
IPFLIBS =		-lipf -lg2c -lm

OBJS = main_tsp.o \
	set_exit.o \
	rdbsemvs.o \
	nambas.o \
	tapewk.o \
	rdbsemvr.o \
	proc_sdf.o \
	is_it_vms.o \
	ge_utils.o

# The following are powerflow modules to be included from the LIBIPF
# file.

IPFOBJS = rddtai.o \
	ldoldbse.o \
	bushinit.o \
	bus_hash.o \
	getput.o \
	opnfil.o \
	prterx.o \
	rddac.o \
	rddat.o \
	rddatx.o \
	loaddc.o \
	loadcc.o \
	get_user.o \
	n_date.o

all : get_ipf_objs tsp

get_ipf_objs: $(IPFOBJS)

$(IPFOBJS):
	echo "archiving IPF module $@ from $(LIBIPF)"
	ar x $(LIBIPF) $@

tsp: $(OBJS)
	echo "Linking tsp"
	$(CC) $(LDFLAGS) \
	$(OBJS) \
	$(IPFOBJS) \
	-L$(TSPLIBDIR) -L$(IPFLIBDIR) \
	$(TSPLIBS) \
	$(IPFLIBS) \
	-o tsp
	@size tsp

.f.o:
	echo "Compiling $*.f "
	$(FC) $(FFLAGS) -c $*.f

.c.o:
	echo "Compiling $*.c "
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -c $*.c


# DO NOT DELETE THIS LINE -- make depend depends on it.
