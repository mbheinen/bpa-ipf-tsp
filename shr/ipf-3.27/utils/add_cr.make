
# File: add_cr.make
# Purpose: compile and link add_cr.c.
# 
 
CC=/usr/bin/gcc
 
TOP=/usr/lib/DXM

CINCLUDES =     -I.\
                -I.. \
                -I$(TOP)/lib \
                -I$(TOP)/lib/Xt

CFLAGS =        -g $(CINCLUDES)

LINTFLAGS=      $(CINCLUDES)

LDFLAGS =       -g -L$(TOP)/lib/Mrm \
                -L$(TOP)/lib/DXm \
                -L$(TOP)/lib/Xm \
                -L$(TOP)/lib/Xt

LIBS =

#
# the IMAGE variable is used to create the executable name
 
IMAGE = add_cr

#
# OBJS is a customized list of modules that are being worked on
# and are present in the current directory
  
OBJS = \
add_cr.o

#
# This is the dependancy relationship for powerflow which
# specifies the link and causes the use of the utility compile 
# procedures below.
# WARNING: 
# The <tab> as the first character signifies the action to
# take for a given dependancy.
# The back slash causes a continuation of the current line 
# even if the line is commented out. 

$(IMAGE) : $(OBJS)
	$(CC) -o $(IMAGE) $(OBJS) $(LDFLAGS) $(LIBS)

#
#
# The following are the compile procedures for any source code specified
# in the dependancy.  These override the system defaults.
# the first procedure is for any fortran code (appendix .f creating .o)
# the second is for an C code (appendix .c creating .o)

.c.o:
	$(CC) -c $(CFLAGS) $< 

# DO NOT DELETE THIS LINE -- make depend depends on it.GS) $<
