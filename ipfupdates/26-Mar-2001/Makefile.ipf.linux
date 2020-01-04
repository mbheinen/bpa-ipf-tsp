# This Makefile is for all IPF executables
#

IPFDIR = $(IPFROOTDIR)

all:
	pwd
	cd $(IPFDIR)/ipf; pwd; ./build_libipf.bash
	cd $(IPFDIR)/ipc; pwd; ./build_libipc.bash
	cd $(IPFDIR)/gui; pwd; ./build_libgui.bash
	cd $(IPFDIR)/exe; pwd; make -f bpf.make; cp bpf /bin
	cd $(IPFDIR)/exe; pwd; make -f ipfsrv.make; cp ipfsrv /bin
	cd $(IPFDIR)/exe; pwd; make -f ipf_reports.make; cp ipf_reports /bin
	cd $(IPFDIR)/exe; pwd; make -f cutting.make; cp cutting /bin
	cd $(IPFDIR)/exe; pwd; make -f netdat.make; cp netdat /bin
	cd $(IPFDIR)/exe; pwd; make -f ipfplot.make; cp ipfplot /bin
	cd $(IPFDIR)/exe; pwd; make -f ipfbat.make; cp ipfbat /bin
	cd $(IPFDIR)/exe; pwd; make -f puf.make; cp puf /bin
	cd $(IPFDIR)/exe; pwd; make -f gui.make; cp gui /bin
	cd $(IPFDIR)/cflow; pwd; make -f findout.make; cp findout /bin
	cd $(IPFDIR)/cflow; pwd; make -f lineflow.make; cp lineflow /bin
	cd $(IPFDIR)/cflow; pwd; make -f pvcurve.make; cp pvcurve /bin
	cd $(IPFDIR)/cflow; pwd; make -f qvcurve.make; cp qvcurve /bin
	cd $(IPFDIR)/cflow; pwd; make -f mimic.make; cp mimic /bin
	cd $(IPFDIR)/cflow; pwd; make -f post_pvcurve.make; cp post_pvcurve /bin

clean:
	cd $(IPFDIR)/ipf; pwd; rm -f *.o *.a
	cd $(IPFDIR)/ipc; pwd; rm -f *.o *.a
	cd $(IPFDIR)/gui; pwd; rm -f *.o *.a
	cd $(IPFDIR)/exe; pwd; rm -f *.o bpf
	cd $(IPFDIR)/exe; pwd; rm -f ipfsrv
	cd $(IPFDIR)/exe; pwd; rm -f ipf_reports
	cd $(IPFDIR)/exe; pwd; rm -f cutting
	cd $(IPFDIR)/exe; pwd; rm -f ipfbat
	cd $(IPFDIR)/exe; pwd; rm -f netdat
	cd $(IPFDIR)/exe; pwd; rm -f ipfplot
	cd $(IPFDIR)/exe; pwd; rm -f puf
	cd $(IPFDIR)/exe; pwd; rm -f gui gui.uid
	cd $(IPFDIR)/cflow; pwd; rm -f *.o findout lineflow pvcurve qvcurve mimic post_pvcurve

