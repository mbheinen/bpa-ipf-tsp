C    @(#)anal.f	20.7 6/27/97
      subroutine anal
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lodtyp.inc'
      include 'ipfinc/mwm.inc'
      include 'ipfinc/outpt2.inc'
      include 'ipfinc/prt.inc'

c     set up record type code parameters..

      include 'ipfinc/bstype.inc'
      include 'ipfinc/brtype.inc'

      common /jbr/ jbr, ptr

      integer ptr
      real temp_vmin, temp_vmax
      character ljstfy*3, kown*3, tmpchr*3
 
      save
 
      nba = 0
      noleff = 0
      ntxeff = 0
      nolbr = 0 
      noltr = 0 
      ncomp = 0 
      ntran = 0 
      novls = 0 
      ndcln = 0 
      jowner = 0
      jvolt = 0 
      jyear = 0 
C       
C     INITIALIZE SYST(I,J),OVLOS(I,J,),AND KOV(I,J) TO 0
C     INITIALIZE FIRM(I),SECN(I),FIRMI(I),RUPTI(I),POTI(I) TO 0
C
      do 100,i=1,MAXOWN
         do 100,j=1,10
            syst(j,i)=0
  100    continue
C
      do 110,i=1,MAXOVL
         do 110, j=1,8
            ovlos(j,i)=0
  110    continue
C
      do 120,i=1,MAXOWN
         do 120,j=1,MAXVLT
            kov(j,i)=0
  120    continue
C
      do 130,i=1,10
         firm(i)=0
         secn(i)=0
         firmi(i)=0
         rupti(i)=0
         poti(i)=0
  130 continue
C
      do 140,i=1,14
         total(i)=0
  140 continue
C
      ncon = 0
      return
C       
C       
      entry bsanal  
C       
C       
      nba = nba + 1 
      kabus(1,nba) = nb 
      read (type,160)abus(2,nba)
  160 format (a1)   
      kt=inp2opt(nb)                        
      abus(3,nba) = 0.0 
      abus(13,nba) = 0.0
      abus(4,nba) = voltpu  
      abus(14,nba) = degree 
      abus(5,nba) = pgen
      abus(6,nba) = qgen
      basex = base(nb)  
      kt = inp2opt(nb)
      abus(7,nba) = pload   
      abus(8,nba) = qload
c
c	Glbvlt is expecting single precision.
c   
      call glbvlt(nb,temp_vmin,temp_vmax)
      vmin = temp_vmin
      vmax = temp_vmax
      abus(12,nba) = vmin
      abus(15,nba)= vmax
      kown = ljstfy (kowner)
      if (jowner.eq.0) go to 190
      do 180 i=1,jowner 
         if (kown.eq.lowner(i)) go to 220  
  180 continue  
  190 jowner = jowner + 1   
      if (jowner .eq. MAXOWN+1) then
         write (errbuf(1),200) MAXOWN   
  200    format('0 Overflow of "OWNER" array (maximum =',i4,')')
         call prterx ('W',1)
         go to 230  
      else if (jowner .gt. MAXOWN) then 
         go to 230  
      else  
         i = jowner 
         lowner(i) = kown   
      endif 
  220 syst(1,i) = syst(1,i) + pgen  
      syst(2,i) = syst(2,i) + qgen  
      syst(3,i) = syst(3,i) + pload 
      syst(4,i) = syst(4,i) + qload 
      syst(5,i) = syst(5,i) + pload 
      syst(6,i) = syst(6,i) + qload 
      if (kown.ne.'BPA') go to 230  
      firm(1) = firm(1) + pgen  
      firm(2) = firm(2) + qgen  
      firm(3) = firm(3) + pload 
      firm(4) = firm(4) + qload 
      firm(5) = firm(5) + pload 
      firm(6) = firm(6) + qload 
  230 if (ktype.ne.BSTYP_BD .and. ktype.ne.BSTYP_BM) go to 260 
      ndcln = ndcln + 1 
      ladc(1,ndcln) = nb
      if (ktype.eq.BSTYP_BM) then
         write(tmpchr  ,240) dcmtbs(4,idc)
  240       format('M_',a1)
         read (tmpchr  ,241) j1
241         format(a3)
         j2 = 0
         if (dcmtbs(3,idc).eq.0) go to 250
         j2 = 1
         if (dcmtbs(19,idc).lt.0) j2 = 2
  250    ladc(2,ndcln)=j1   
         ladc(11,ndcln)=j2  
         adc(3,ndcln) = dcmtbs(25,idc)  
         adc(4,ndcln) = dcmtbs(26,idc)  
         adc(5,ndcln) = 57.2957795*dcmtbs(13,idc)   
         adc(6,ndcln) = dcmtbs(19,idc)  
         adc(8,ndcln) = dcmtbs(20,idc)  
         ladc(7,ndcln) = dcmtbs(27,idc) 
         ladc(12,ndcln) = dcmtbs(31,idc)
         ladc(13,ndcln) = dcmtbs(32,idc)
         ladc(14,ndcln) = dcmtbs(33,idc)
         ladc(9,ndcln) = dcmtbs(28,idc) 
         adc(10,ndcln) = 1000.0*dcmtbs(19,idc)/dcmtbs(20,idc)   
      else  
         tmpchr ='D-2' 
         read (tmpchr, 242) ladc(2,ndcln)   
  242    format(a3) 
         kstat=dc2t(20,idc)
         if(dc2t(1,idc).eq.nb)then 
            ladc(11,ndcln)=1
            adc(3,ndcln)=dc2t(42,idc)   
            adc(4,ndcln)=dc2t(44,idc)   
            adc(5,ndcln)=57.2957795*dc2t(22,idc)
            adc(6,ndcln)=0.001*dc2t(39,idc)*dc2t(40,idc)
            adc(8,ndcln)=dc2t(40,idc)   
            ladc(7,ndcln)=kstat/10000   
            ladc(9,ndcln)=mod(kstat/1000,10)
            if (dc2t(7,idc).eq.1) then 
              ladc(12,ndcln)=mod(kstat/100,10)  
              ladc(13,ndcln)=mod(kstat/10,10)   
              ladc(14,ndcln)=mod(kstat,10)  
            else
              ladc(12,ndcln) = 0
              ladc(13,ndcln) = 0
              ladc(14,ndcln) = 0
            endif   
         else   
            ladc(11,ndcln)=2
            adc(3,ndcln)=dc2t(43,idc)   
            adc(4,ndcln)=dc2t(45,idc)   
            adc(5,ndcln)=57.2957795*dc2t(26,idc)
            adc(6,ndcln)=-0.001*dc2t(39,idc)*dc2t(41,idc)   
            adc(8,ndcln)=dc2t(41,idc)   
            ladc(7,ndcln)=0 
            ladc(9,ndcln)=0 
            if (dc2t(7,idc).eq.2) then 
              ladc(12,ndcln)=mod(kstat/100,10)  
              ladc(13,ndcln)=mod(kstat/10,10)   
              ladc(14,ndcln)=mod(kstat,10)  
            else
              ladc(12,ndcln) = 0
              ladc(13,ndcln) = 0
              ladc(14,ndcln) = 0
            endif   
         endif  
         adc(10,ndcln)=dc2t(39,idc) 
      endif 
  260 return
C       
      entry cbanal  
C       
      abus(5,nba) = abus(5,nba) + pgen2 
      abus(6,nba) = abus(6,nba) + qgen2 
      abus(7,nba) = abus(7,nba) + pload2
      abus(8,nba) = abus(8,nba) + qload2
      kown = ljstfy (kowner)
  270 do 280 i=1,jowner 
         if (lowner(i).eq.kown) go to 300  
  280 continue  
      jowner = jowner + 1   
      if (jowner .eq. MAXOWN+1) then
         write (errbuf(1),200) MAXOWN   
         call prterx ('W',1)
         go to 400  
      else if (jowner .gt. MAXOWN) then 
         go to 400  
      else  
         i = jowner 
         lowner(i) = kown   
      endif 
  300 syst(1,i) = syst(1,i) + pgen2 
      syst(2,i) = syst(2,i) + qgen2 
C       
C     Use composite loads: Constant power, constant current, and
C     constant impedance, all expressed in MWs, MVARs.  
C       
      syst(3,i) = syst(3,i) + lodtyp(1) + lodtyp(3) + lodtyp(5) 
      syst(4,i) = syst(4,i) + lodtyp(2) + lodtyp(4) + lodtyp(6) 
      syst(5,i) = syst(5,i) + lodtyp(1) 
      syst(6,i) = syst(6,i) + lodtyp(2) 
      syst(7,i) = syst(7,i) + lodtyp(3) 
      syst(8,i) = syst(8,i) + lodtyp(4) 
      syst(9,i) = syst(9,i) + lodtyp(5) 
      syst(10,i) = syst(10,i) + lodtyp(6)   
  310 ild = 0   
      if (kown .ne. 'BPA') go to 400
      if (kode .eq. 'P') then   
         poti(1) = poti(1) + pgen2  
         poti(2) = poti(2) + qgen2  
         poti(3) = poti(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)  
         poti(4) = poti(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)  
         poti(5) = poti(5) + lodtyp(1)  
         poti(6) = poti(6) + lodtyp(2)  
         poti(7) = poti(7) + lodtyp(3)  
         poti(8) = poti(8) + lodtyp(4)  
         poti(9) = poti(9) + lodtyp(5)  
         poti(10) = poti(10) + lodtyp(6)
         ild = 13   
      else if (kode .eq. 'N') then  
         firm(1) = firm(1) + pgen2  
         firm(2) = firm(2) + qgen2  
         firm(3) = firm(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)  
         firm(4) = firm(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)  
         firm(5) = firm(5) + lodtyp(1)  
         firm(6) = firm(6) + lodtyp(2)  
         firm(7) = firm(7) + lodtyp(3)  
         firm(8) = firm(8) + lodtyp(4)  
         firm(9) = firm(9) + lodtyp(5)  
         firm(10) = firm(10) + lodtyp(6)
      else if (kode .eq. 'S') then  
         secn(1) = secn(1) + pgen2  
         secn(2) = secn(2) + qgen2  
         secn(3) = secn(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)  
         secn(4) = secn(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)  
         secn(5) = secn(5) + lodtyp(1)  
         secn(6) = secn(6) + lodtyp(2)  
         secn(7) = secn(7) + lodtyp(3)  
         secn(8) = secn(8) + lodtyp(4)  
         secn(9) = secn(9) + lodtyp(5)  
         secn(10) = secn(10) + lodtyp(6)
         secn(8) = secn(8) + lodtyp(6)  
      else if (kode .eq. 'F') then  
         firmi(1) = firmi(1) + pgen2
         firmi(2) = firmi(2) + qgen2
         firmi(3) = firmi(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)
         firmi(4) = firmi(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)
         firmi(5) = firmi(5) + lodtyp(1)
         firmi(6) = firmi(6) + lodtyp(2)
         firmi(7) = firmi(7) + lodtyp(3)
         firmi(8) = firmi(8) + lodtyp(4)
         firmi(9) = firmi(9) + lodtyp(5)
         firmi(10) = firmi(10) + lodtyp(6)  
         ild = 1
      else if (kode .eq. 'I') then  
         rupti(1) = rupti(1) + pgen2
         rupti(2) = rupti(2) + qgen2
         rupti(3) = rupti(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)
         rupti(4) = rupti(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)
         rupti(5) = rupti(5) + lodtyp(1)
         rupti(6) = rupti(6) + lodtyp(2)
         rupti(7) = rupti(7) + lodtyp(3)
         rupti(8) = rupti(8) + lodtyp(4)
         rupti(9) = rupti(9) + lodtyp(5)
         rupti(10) = rupti(10) + lodtyp(6)  
         ild = 7
      else  
         firm(1) = firm(1) + pgen2  
         firm(2) = firm(2) + qgen2  
         firm(3) = firm(3) + lodtyp(1) + lodtyp(3) + lodtyp(5)  
         firm(4) = firm(4) + lodtyp(2) + lodtyp(4) + lodtyp(6)  
         firm(5) = firm(5) + lodtyp(1)  
         firm(6) = firm(6) + lodtyp(2)  
         firm(7) = firm(7) + lodtyp(3)  
         firm(8) = firm(8) + lodtyp(4)  
         firm(9) = firm(9) + lodtyp(5)  
         firm(10) = firm(10) + lodtyp(6)
      endif
  360 if (ild .eq. 0) go to 400
      if (ncon.eq.0) go to 361
      if (load(1,ncon) .eq. nb) then
         do  i = 1,6
            aload(ild+i,ncon) = aload(ild+i,ncon) + lodtyp(i)
         enddo
         return
      endif
  361 ncon = ncon + 1
      if (ncon .eq. MAXLOD+1) then
         write (errbuf(1),390) MAXLOD
  390    format(' OVERFLOW OF " LOAD " ARRAY (MAXIMUM =',i4,')')
         call prterx ('W',1)
      else if (ncon .gt. MAXLOD) then   
      else  
         do 364 i = 1,20
  364    aload(i,ncon) = 0.0
         load(1,ncon) = nb
         do i = 1,6
            aload(ild+i,ncon) = aload(ild+i,ncon) + lodtyp(i)
         enddo
      endif
  400 return
C       
      entry lnanal  
C       
      basex = amax1(base1,base2)
      ixit = 1
  410 continue  
      if (jvolt.eq.0) go to 430 
      do 420 i = 1,jvolt
         if (avolt(i).eq.basex) go to 460  
  420 continue  
  430 jvolt = jvolt + 1 
      if (jvolt .eq. MAXVLT+1) then 
         write (errbuf(1),440) MAXVLT   
  440    format('0 OVERFLOW OF " AVOLT" ARRAY - (MAXIMUM = ',i4,')')
         call prterx ('W',1)
         go to 560  
      else if (jvolt .gt. MAXVLT) then  
         go to 560  
      else  
         i = jvolt  
         avolt(i) = basex   
      endif 
  460 kown = ljstfy (kowner)
      do 470 j=1,jowner 
         if (lowner(j).eq.kown) go to 490  
  470 continue  
      jowner = jowner + 1   
      if (jowner .eq. MAXOWN+1) then
         write (errbuf(1),200) MAXOWN   
         call prterx ('W',1)
         go to 560  
      else if (jowner .le. MAXOWN) then 
         j = jowner 
         lowner(j) = kown   
      else  
         go to 560  
      endif 
  490 lov=kov(i,j)  
      if (lov.gt.0) go to 520   
      novls = novls + 1 
      if (novls .eq. MAXOVL+1) then 
         write (errbuf(1),500)MAXOVL
  500    format('0 OVERFLOW OF "OWNER-VOLTAGE" LOSS ARRAY', 
     1          '         (MAXIMUM = ',i4,')')
         call prterx ('W',1)
         go to 560  
      else if (novls .le. MAXOVL) then  
         kov(i,j)=novls 
         lov = novls
      else  
         go to 560  
      endif 
  520 if (ltype) 540,530,550

  530 continue  
      ovlos(1,lov) = ovlos(1,lov) + gequiv  
      ovlos(2,lov) = ovlos(2,lov) + bequiv  
      go to 560 

  540 ovlos(5,lov) = ovlos(5,lov) + pvlv
      ovlos(6,lov) = ovlos(6,lov) + qvlv
      go to 560 

  550 continue  
      if (ltype.eq.BRTYP_T.or.ltype.eq.BRTYP_TP) then
           ovlos(3,lov)=ovlos(3,lov)+ploss  
           ovlos(4,lov)=ovlos(4,lov)+qloss  
      else  
           ovlos(1,lov) = ovlos(1,lov) + ploss  
           ovlos(2,lov) = ovlos(2,lov) + qloss  
           ovlos(7,lov) = ovlos(7,lov) + amiles 
           ovlos(8,lov) = ovlos(8,lov) + pav*amiles 
      endif 

  560 go to (570,640,650) ixit  

  570 return
C       
      entry dclnal  
C       
         ndcln = ndcln + 1 
         ladc(1,ndcln) = k1
         ladc(2,ndcln) = k2
         adc(3,ndcln) = pin
         adc(4,ndcln) = ploss  
         ladc(5,ndcln) = pctol 
      return
C       
      entry olanal  
c       
      if(ltype.eq.BRTYP_T .or. ltype.eq.BRTYP_TP) then  
         noltr=noltr+1  
         if (noltr .eq. MAXOLT+1) then  
            write (errbuf(1),580)MAXOLT 
  580       format('0 OVERFLOW OF" OVERLOADED TRANSFORMERS " ARRAY ',   
     1             '(MAXIMUM = ',i4,')')
            call prterx ('w',1) 
         else if (noltr .le. MAXOLT) then   
            koltr(1,noltr) = ptr  
            oltr(2,noltr)  = vamag 
            oltr(3,noltr)  = amag
            oltr(4,noltr)  = abs(pwrfk)
         endif  
      else  
         nolbr=nolbr+1  
         if (nolbr .eq. MAXOLB+1) then  
            write (errbuf(1),600)MAXOLB 
  600       format('0 OVERFLOW OF "OVERLOADED LINES" ARRAY (MAXIMUM =', 
     1             i4,')')  
            call prterx ('w',1) 
         else if (nolbr .le. MAXOLB) then   
            kolbr(1,nolbr) = ptr  
            olbr(2,nolbr)  = amag  
            olbr(3,nolbr)  = vamag 
            if (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_LD) then
               pwrfk = 1.0  
            endif   
            olbr(5,nolbr)=tang  
            olbr(6,nolbr)=abs(pwrfk)
         endif  
      endif 
      return
C       
      entry effanl  
C       
      if (ltype.ne.BRTYP_T .and. ltype.ne.BRTYP_TP) then   
         noleff = noleff+1  
         if (noleff .eq. MAXBRN + 1) then   
            write (errbuf(1),670) noleff
  670       format('0 OVERFLOW OF "LINE EFFICIENCY" ARRAY (MAXIMUM=',   
     1              i4,')') 
            call prterx ('W',1) 
         else if (noleff .le. MAXBRN) then  
            read (kowner,682) leff(1,noleff)
  682       format (a3) 
            leff(2,noleff)=k1   
            leff(3,noleff)=k2   
            read (id,590) leff(4,noleff)
  590       format (a1) 
            leff(5,noleff)=isect
            eff(6,noleff)=amiles
            eff(9,noleff)=ploss 
            eff(12,noleff)=rating
            if (amiles.gt.0.0) then 
               if (pav .ne. 0.0 .and. amiles .ne. 0.0) then 
                  eff(13,noleff) = (ploss/pav)*(100./amiles)*100.   
               else 
                  eff(13,noleff) = 0.0  
               endif
            else
               eff(13,noleff) = 0.0 
            endif   
            if (amiles.eq.0) then   
               eff(10,noleff) = 0   
            else
               eff(10,noleff) = ploss/amiles
            endif   
            if (pav .ne. 0.0) then 
               eff(11,noleff)=100.0*ploss/pav  
            else
               eff(11,noleff)=0.0
            endif
            eff(7,noleff)=vamag 
            eff(8,noleff)=amag  
         endif  
      endif 
      return
C       
      entry txefan  
C       
      if (ltype.eq.BRTYP_T) then  
         if ( brnch_ptr(ptr).gt.0 ) then
c           input order
            tk = brnch(9,jbr)/base(k1) 
            tm = brnch(10,jbr)/base(k2)
         else
c           transpose the data
            tk = brnch(10,jbr)/base(k1) 
            tm = brnch(9,jbr)/base(k2)
         endif
         tkm = 1.0/(tk*tm)  
         mt = inp2opt(k2) 
         plsirn =0.5*bmva*tkm*brnch(7,jbr)*(e(kt)**2+f(kt)**2+e(mt)**2  
     1     + f(mt)**2)  
         plscop=ploss-plsirn
         if (ratxef.gt.100.0*plscop/vamag.and.ratcef.gt.100.0*plsirn)   
     1      go to 780   
         ntxeff = ntxeff+1  
        
         if (ntxeff .eq. 2 * MAXOLT + 1) then   
            write (errbuf(1),710) 2 * MAXOLT
  710       format('0 OVERFLOW OF "TX EFFICIENCY" ARRAY (MAXIMUM =',
     1             i4,')')  
            call prterx ('W',1) 
        
         else if (ntxeff .le. 2 * MAXOLT) then  
            read (kowner,682) ltxeff(1,ntxeff)  
            if (base(k1).gt.base(k2)) then  
               ltxeff(2,ntxeff)=k1  
               ltxeff(3,ntxeff)=k2  
            else
               ltxeff(2,ntxeff)=k2  
               ltxeff(3,ntxeff)=k1  
            endif   
            read(id,590)ltxeff(4,ntxeff)
            ltxeff(5,ntxeff)=isect  
            txeff(6,ntxeff)=vamag   
            txeff(7,ntxeff)=rating
            txeff(8,ntxeff)=plsirn  
            txeff(9,ntxeff)=plscop  
         endif  
      endif 
        
  780    continue   
         return 
        
C       
      entry cpanal(pctcp)   
C       
      ncomp = ncomp + 1 
      if (ncomp .le. MAXCMP) then   
         komp(1,ncomp)=k1   
         komp(3,ncomp)=k2   
         read (id,590)komp(4,ncomp) 
         comp(2,ncomp)=pctcp
         read(kowner,682)comp(5,ncomp)  
        
      else if (ncomp .eq. MAXCMP+1) then
         write (errbuf(1),620) MAXCMP   
  620    format('0 OVERFLOW OF "SERIES COMPENSATION" ARRAY',
     1   ' (MAXIMUM = ',i4,')') 
         call prterx ('W',1)
      endif 
      return
C       
      entry fianal  
C       
      abus(9,nba) = skcap   
      abus(10,nba) = qcap   
      abus(11,nba) = qunsk  
      abus(16,nba) = skreak 
      abus(17,nba) = qreak  
      do 630 i=1,4  
         total(i) = total(i) + abus(i+4,nba)   
  630 continue
      total(7) = total(7) + dmax1(0.0d0,qunsk)
      total(10) = total(10) + dmin1(0.0d0,qunsk)  
      total(5) = total(5) + skcap   
      total(6) = total(6) + qcap
      total(8) = total(8) + skreak  
      total(9) = total(9) + qreak   
      total(13) = total(13) + skreak
      total(14) = total(14) + qreak 
      if(abus(6,nba).le.0.0)then
          total(12)=total(12)+abus(6,nba)   
      else  
          total(11)=total(11)+abus(6,nba)   
      endif 
      if (bequiv.eq.0.and.gequiv.eq.0) go to 640
      ltype = 0 
      basex = base(nb)  
      kowner = '***'
      kown = kowner 
      ixit = 2
      go to 410 

  640 if (pvlv.eq.0.and.qvlv.eq.0) go to 650
      ltype = -1
      basex = base(nb)  
      kowner = owner(nb)
      kown = ljstfy(kowner) 
      ixit = 3
      go to 410 

  650 return
C       
      entry txanal  
C       
      if(abus(3,nba).eq.0.0)then
          if(type.eq.'G')then   
              if(base2.eq.500.0)then
                  abus(3,nba)=trx1  
                  abus(13,nba)=trx2 
              endif 
          else if(base1.eq.500.0.and.base2.eq.230.0)then
              abus(3,nba)=trx1  
              abus(13,nba)=trx2 
          endif 
      endif 
      return
      end   
