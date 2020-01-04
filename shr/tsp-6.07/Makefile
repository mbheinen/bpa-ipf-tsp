# This Makefile is for all TSP executables
#

IPFDIR = /shr/ipf-3.27
TSPDIR = /shr/tsp-6.07

all:
	pwd
	cd $(TSPDIR)/tsp; pwd; ./build_libtsp.bash
	cd $(TSPDIR)/exe; pwd; rm -f ipfinc; ln -s /shr/ipf-3.27/ipf ipfinc
	cd $(TSPDIR)/exe; pwd; rm -f tspinc; ln -s /shr/tsp-6.07/tsp tspinc
	cd $(TSPDIR)/exe; pwd; make -f tsp.make 

clean:
	cd $(TSPDIR)/tsp; pwd; rm -f *.o *.a
	cd $(TSPDIR)/exe; pwd; rm -f tsp *.o ipfinc tspinc
	
