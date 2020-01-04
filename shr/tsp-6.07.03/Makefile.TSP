# This Makefile is for all TSP-6.07 executables for use with IPF-3.27.xx.
# 22-April-2002

IPFDIR = $(IPFROOTDIR)
IPFDIR = /shr/ipf-3.27
TSPDIR = $(TSPROOTDIR)
TSPDIR = /shr/tsp-6.07
SFAPDIR = $(SFAPROOTDIR)
SFAPDIR = /shr/sfap-1.01

all:
	pwd
	cd $(TSPDIR)/tsp; pwd; ./build_libtsp.bash
	cd $(TSPDIR)/exe; pwd; rm -f ipfinc; ln -s /shr/ipf-3.27/ipf ipfinc
	cd $(TSPDIR)/exe; pwd; rm -f tspinc; ln -s /shr/tsp-6.07/tsp tspinc
	cd $(TSPDIR)/exe; pwd; make -f tsp.make; cp tsp /bin 

clean:
	cd $(TSPDIR)/tsp; pwd; rm -f *.o *.a
	cd $(TSPDIR)/exe; pwd; rm -f tsp *.o ipfinc tspinc
	
