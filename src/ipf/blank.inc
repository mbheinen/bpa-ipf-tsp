
      common /blank/ krun1(20), kase1(40), kspare(40), mainsw,
     1               ksy, knew, knum, ntot_alf, kabort,         
     2               minerr, lskp, no1, svolt, sang, bmva, kchgsw,
     3               ktchge, ntot, ntot2,
     4               ltot, ntota, ntotb, ntotc,
     5               ntotcs, jtie, nshift, jphno, kdtot, kxtot,
     6               kdchg, nztot, natot, mtdcbs, mtdcln,
     7               nbslck, nslkxx(4,10),
     8               vlimit(5,100), nvlim, kbsknt, kbrknt
      common /blankc/ buf, chase1(40), crun1(20), oldcse, dte,
     1                cspare(40), card, vlimzn(10,100), srtlst

      integer   krun1, kase1, kspare, mainsw,
     1               ksy, knew, knum, ntot_alf, kabort,         
     2               minerr, lskp, no1, kchgsw,
     3               ktchge, ntot, ntot2,
     4               ltot, ntota, ntotb, ntotc,
     5               ntotcs, jtie, nshift, jphno, kdtot, kxtot,
     6               kdchg, nztot, natot, mtdcbs, mtdcln,
     7               nbslck, nslkxx,
     8               nvlim, kbsknt, kbrknt
      real      svolt, sang, bmva, vlimit
      character buf*120, chase1*10, crun1*10, oldcse*10, dte*10,
     1          cspare*10, card*1, vlimzn*2, srtlst*8
      integer kvlimt(5,100)
      real spare(40), run1(20), case1(40), slkxx(4,10)
      equivalence (krun1,run1), (kase1,case1), (spare,kspare),
     1            (nslkxx,slkxx), (vlimit,kvlimt)

