c pf_blank.cmn
      common /blank/ pf_krun1(20), pf_kase1(40), pf_kspare(40), 
     1 pf_mainsw, pf_ksy, pf_knew, pf_knum, pf_ntot_alf, 
     2 pf_kabort, pf_minerr, pf_lskp, pf_no1, pf_svolt,pf_sang,
     3 pf_bmva,pf_kchgsw, pf_ktchge, pf_ntot, pf_ntot2,
     4 pf_ltot, pf_ntota, pf_ntotb, pf_ntotc,
     5 pf_ntotcs, pf_jtie, pf_nshift, pf_jphno, pf_kdtot, pf_kxtot,
     6 pf_kdchg, pf_nztot, pf_natot, pf_mtdcbs, pf_mtdcln,
     7 pf_nbslck, pf_nslkxx(4,10),
     8 pf_vlimit(5,100), pf_nvlim, pf_kbsknt, pf_kbrknt
      common /blankc/ pf_buf, pf_chase1(40), pf_crun1(20), 
     1 pf_oldcse, pf_dte,
     2 pf_cspare(40), pf_card, pf_vlimzn(10,100), pf_srtlst
      integer pf_krun1, pf_kase1, pf_kspare, pf_mainsw, 
     1 pf_ksy, pf_knew, pf_knum, pf_ntot_alf, pf_kabort,
     2 pf_minerr, pf_lskp, pf_no1, pf_kchgsw,
     3 pf_ktchge, pf_ntot, pf_ntot2,
     4 pf_ntotcs, pf_jtie, pf_nshift, pf_jphno, pf_kdtot, pf_kxtot,
     5 pf_kdchg, pf_nztot, pf_natot, pf_mtdcbs, pf_mtdcln,
     6 pf_nbslck, pf_nslkxx,
     7 pf_nvlim, pf_kbsknt, pf_kbrknt
       real pf_svolt, pf_sang, pf_bmva, pf_vlimit
       real pf_run1, pf_case1, pf_spare, pf_slkxx
       integer pf_kvlimt, pf_ltot, pf_ntota,pf_ntotb,pf_ntotc,pf_brknt
      character pf_buf*120, pf_chase1*10, pf_crun1*10, 
     1 pf_oldcse*10, pf_dte*10,
     2 pf_cspare*10, pf_card*1, pf_vlimzn*2, pf_srtlst*8
      dimension pf_spare(40),pf_run1(20),pf_case1(40),pf_kvlimt(5,100),
     1          pf_slkxx(4,10)
      equivalence (pf_krun1,pf_run1), (pf_kase1,pf_case1), 
     1 (pf_spare,pf_kspare), (pf_nslkxx,pf_slkxx),(pf_vlimit,pf_kvlimt)

C end pf_blank.inc
