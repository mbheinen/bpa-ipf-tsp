C    @(#)bldxtb.f	20.3 2/13/96
      subroutine bldxtb
      
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/komps.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'

      ntbxtr = 0
      if (ntota.eq.0) go to 1590
      if (ntotb.eq.0) go to 1590
        
      do 1480 i = 1,ntot
         nbsort(i) = 0
 1480 continue
 
      do 1490 jt = 1,ntotb
         ltyp=tbx(1,jt)
         if (ltyp .eq. 2 .or. ltyp .eq. 5) then
            kt=tbx(2,jt)
            nbsort(kt)=jt
         endif
 1490 continue
 
      do 1580 jt = 1,ntota
      ityp=ltran(10,jt)
      if (ityp.ne.1) go to 1580
      kt=ltran(1,jt)
      mt=ltran(9,jt)
      jk = nbsort(kt)   
      jm = nbsort(mt)   
      ksw = 0   
C       
C     KSW = 0: No BQ or BX busses connected to LTC JT.  
C           1: BQ or BX bus at KT only. 
C           2: BQ or BX bus at MT only. 
C           3: BQ or BX busses at KT and MT.
C       
      if (jk.ne.0) ksw = 1  
      if (jm.ne.0) ksw = ksw + 2
      if (ksw.eq.0) goto 1580   
      go to (1500,1510,1530) ksw
 1500 kx = mt   
      go to 1520
 
 1510 kx = kt
C
C     Exclude any d-c commutating LTC's.
C       
 1520 continue  
      if (ntypu(kt) .eq. 5 .or. ntypu(kt) .eq. 12) go to 1580  
      if (ntypu(mt) .eq. 5 .or. ntypu(mt) .eq. 12) go to 1580  
 1530 ntbxtr=ntbxtr+1   
      tbxtr(1,ntbxtr)=jt   
      tbxtr(2,ntbxtr)=jk   
      tbxtr(3,ntbxtr)=jm   
 1580 continue  
        
 1590 continue  
        
      return
      end   
