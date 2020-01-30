C    %W% %G%
      subroutine initl1
C     
C     This subroutine forms the internal swing bus ordering.
C     It makes the necessary changes in data tables to
C     accommodate the intermediate bus if one has been
C     requested.  It is called by SWINGM.
C     
      include 'tspinc/params.inc'
      equivalence (ktemp,temp)
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/ldrep.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/ecstbg.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/znox.inc'
      dimension idum(25)
      include 'tspinc/busvolt.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/indn2x.inc'
      include 'tspinc/brnch.inc'

C     -    Create a one-dimensional vector for bus reordering index
C     -    equivalencing.
c dlc get the kk,vold1,vold2 values from rddtai_mv
      include 'tspinc/rddtai_mv.inc'
c dlc      DIMENSION BRVCTR(54*MAXBUS)
c dlc      EQUIVALENCE (BRVCTR,JBRNCH)
c dlc      DIMENSION INDN2O(MAXBUS),INDO2X(MAXBUS),INDX2O(MAXBUS),
c dlc     1          INDO2N(MAXBUS),VOLD1(MAXBUS),VOLD2(MAXBUS),
c dlc     2          CAPOLD(2,MAXBUS),KK(2,MAXBUS),KTEMPN(3,MAXBUS),
c dlc     3          IXN(MAXBUS)
c dlc      EQUIVALENCE (INDO2N,BRVCTR(1)), (INDN2O,BRVCTR(MAXBUS+1)),
c dlc     1  (INDO2X,BRVCTR(2*MAXBUS+1)), (INDX2O,BRVCTR(3*MAXBUS+1)),
c dlc     2  (VOLD1,BRVCTR(4*MAXBUS+1)), (VOLD2,BRVCTR(5*MAXBUS+1)),
c dlc     3  (CAPOLD,BRVCTR(6*MAXBUS+1)), (KK,BRVCTR(8*MAXBUS+1)),
c dlc     4  (KTEMPN,BRVCTR(10*MAXBUS+1)), (IXN,BRVCTR(13*MAXBUS+1))
      dimension ktemp1(100), temp1(100), lrrecs(400), bsbr(200),
     &  jbus(200), gij(199), bij(198), ibrbn(25), ibrs(25), jphase(50),
     &  buffer(8)
      equivalence(bsbr,jbus)
      equivalence(bsbr(2),gij)
      equivalence(bsbr(3),bij,gii)
      equivalence(bsbr(4),bii)
      character*10 buffer
      include 'tspinc/bypass.inc'

c dlc      DIMENSION JPHID(8,50)
      include 'tspinc/dcinfo.inc'
      character*8 ipcd
      include 'tspinc/bname.inc'

c     -  Functions
      logical dbghere

c     -  Local variables
      character*8 nac(MAXBUS)
      equivalence(bname,nac)
      logical debug                                                     !dem

c     -      begin     begin     begin     begin     begin     begin
      debug = dbghere ('INITL1  ')                                      !dem
      call mpost('INITL1')
      isg=isgg
C     
C     THIS BLOCK OF LOGIC READS THE REMAINDER OF THE PERTINENT DATA
C     FROM THE POWER FLOW TAPE
C     
      kcor=kphase+1
      kpdc=0
c dlc      READ (L3) (INDO2X(I),I=1,NTOT)
c dlc      READ (L3) (INDX2O(I),I=1,NTOTD)
C     
C     RENUMBER BUSES FOR LINES WITH ZNO CAPACITORS FROM EXTERNAL
C     ORDER TO OLD OR POWER FLOW ORDER
C     
      ncomp = iznmax
      if(iznmax .ne. 0)then
         do 110 i= 1,iznmax
         ib = iznbus(i)
         jb = jznbus(i)
         iznbus(i) = indx2o(ib)
         jznbus(i) = indx2o(jb)
        idum(2*i-1) = iznbus(i)
  110   idum(2*i  ) = jznbus(i)
      endif
c dlc      READ (L3) (VOLD1(I),I=1,NTOT)
c dlc      READ (L3) (VOLD2(I),I=1,NTOT)
c dlc      READ (L3) ((CAPOLD(J,I),J=1,2),I=1,NTOT)
      if (jphno.eq.0) go to 180
c dlc      READ(L3) ((JPHID(I,J),I=1,8),J=1,JPHNO)
      lpht=lphase
      i=0
   6  i=i+1
      if(i .gt. lpht) go to 10
      iph=jphid(1,i)
      jph=jphid(2,i)
      if((iph .gt. 0) .and. (jph .gt. 0) ) go to 11
      lpht = lpht-1
      if(lpht .eq. 0) go to 10
      do 70 jj=1,lpht
      do 70 nn=1,8
      jphid(nn,jj)=jphid(nn,jj+1)
   70 continue
      i=i-1
      go to 6
   11 iph = indx2o(iph)
      jph = indx2o(jph)
      itbphn(1,i)=iph
      itbphn(2,i)=jph
      go to 6
   10 lphase = lpht
C     
C     READ BUS DATA TABLE FROM POWER FLOW HISTORY FILE
C     AND STORE IN ECS. THIS TABLE IS CALLED BSBR.
C     KK(I,J) IS THE INDEX TO THE BSBR FOR EACH BUS
C     AND THE LENGTH OF TABLE FOR THAT BUS.
C     
  180 continue
c dlc  180 READ (L3) ((KK(I,J),I=1,2),J=1,NTOT)
c dlc  200 CONTINUE
c dlc  210 IF(KECSY.LE.4000)GO TO 220
c dlc      READ(L3) (STORE(I),I = 1,4000)
c dlc      CALL RITECS(STORE,KECSX,4000)
c dlc      KECSX=KECSX+4000
c dlc      KECSY=KECSY-4000
c dlc      GO TO 210
c dlc  220 READ(L3) (STORE(I),I=1,KECSY)
c dlc      CALL RITECS(STORE,KECSX,KECSY)
c dlc      KECSX=KECSX+KECSY
c dlc      KECST=KECST-1
c dlc  235 KECSY=KECSYM
C     
C     START OF PHASE SHIFTER BLOCK
C     THIS BLOCK SAVES THE AVERAGE ADMITTANCE OF THE PHASE SHIFTER
C     BRANCHES
C     
      if (lphase.eq.0) go to 460
      lpht=lphase
      if (lphase.gt.50) lphase=50
      do 440 i=1,lphase
      iph=itbphn(1,i)
      jph=itbphn(2,i)
      gijph(i)=0.0
      bijph(i)=0.0
      kph=jph
  360 no=kk(2,iph)
      call redecs (bsbr,kk(1,iph)+kecst,no)
      do 380 j=13,no,3
      if (jbus(j).eq.jph) go to 420
  380 continue
      write (errbuf(1),400) jph
      call prterr ('E',1)
  400 format (28h phase-shifter search error ,i5)
      iabort=1
      go to 440
  420 gijph(i)=gijph(i)+0.5*gij(j)
      bijph(i)=bijph(i)+0.5*bij(j)
      if (kph.eq.iph) go to 440
      jph=itbphn(1,i)
      iph=itbphn(2,i)
      kph = iph
      go to 360
  440 continue
      lphase=lpht
  460 continue
C     
C     START OF INTERMEDIATE BUS BLOCK
C     THIS BLOCK FORMS THE INTERMEDIATE BUS FOR A TYPE 3 FAULT
C     
      nmx=kbsknt
      ib=iftabn(1)
      jb=jftabn(1)
      iba=iabs(ib)
      jba=iabs(jb)
      ksect=ipcdtn(1)
      ipcd=ipcdtc(1)
      mde=iabs(mflt(1))
      if (mde.ne.3) go to 1040
      if (kbsknt .gt. MAXBUS) then
         write (errbuf(1), 480) MAXBUS
         call prterr ('E',1)
  480    format ('0NUMBER OF BUSES EXCEEDS ',i5,' WITH MODE 3 FAULT.')
         go to 990
      endif
  500 if (perct(1).eq.0.0) go to 680
      if (perct(1).gt.100.) go to 640
      nmx=kbsknt+1
      do 520 ifl=lst,iline
        if(ipcd.ne.ijslc(ifl).or.ksect.ne.ijsln(3,ifl)) goto 520
        if(ib.eq.ijsln(1,ifl).and.jb.eq.ijsln(2,ifl))goto 540
        if(jb.eq.ijsln(1,ifl).and.ib.eq.ijsln(2,ifl))goto 540
  520 continue
      ib = iabs(ib)
      jb = iabs(jb)
      write (outbuf,525) exnamc(ib),basekv(ixnamn(ib)),
     1                   exnamc(jb),basekv(ixnamn(jb)),ipcd,ksect
      call prtout (1)
  525 format (1h0,'LINE ',a8,1x,f5.1,1x,a8,1x,f5.1,1x,a1,1x,i1,
     1          ' CANNOT BE FOUND IN BRANCH TABLE')
      call erexit
  540 p800 =perct(1) * 0.01
      write (outbuf, 560)
      call prtout (1)
  560 format (1h0,5x,'PERCENT',f8.4)
      if (gsl(2*ifl).ne.gsl(2*ifl-1)) go to 760
      if (bsl(2*ifl).ne.bsl(2*ifl-1)) go to 760
      gi1=gsl(2*ifl-1)
      bi1=bsl(2*ifl-1)
      gj1=gijs(ifl)
      bj1=bijs(ifl)
      braddc=bradd(ifl)
      noleg=1
      if (gi1.ne.0.0) go to 580
      if (bi1.ne.0.0) go to 580
      noleg=2
      gijnl=gj1
      bijnl=bj1
      go to 600
C     
C     CALCULATE CONSTANTS OF LINE FROM ORIGINAL PI EQUIVALENT
C     
  580 gtmp1=gi1+2.0*gj1
      btmp1=bi1+2.0*bj1
      ysqr=gi1*gtmp1-bi1*btmp1
      ysqi=gi1*btmp1+bi1*gtmp1
      ysqm=sqrt(ysqr**2+ysqi**2)
      gcnst=sqrt(0.5*(ysqr+ysqm))
      bcnst=sqrt(0.5*(-ysqr+ysqm))
      if (ysqi.lt.0.0) bcnst=-bcnst
      if (ysqi.lt.0.0) bcnst=-bcnst
      gtmp1=gi1+gj1+gcnst
      btmp1=bi1+bj1+bcnst
      denom=1.0/(gj1**2+bj1**2)
      varr=(gj1*gtmp1+bj1*btmp1)*denom
      vari=(gj1*btmp1-bj1*gtmp1)*denom
      angl=atan2(vari,varr)
      vlog=alog(sqrt(varr**2+vari**2))
  600 if (braddc.eq.(-1000.0)) go to 800
C     
C     IF BRANCH IS A NEW BRANCH, THEN THERE IS NO OLD BRANCH TO
C     REMOVE AND THE NEW BRANCH DOES NOT HAVE TO BE ADDED
C     IN MATMOD.
C     
      gi1=0.0
      bi1=0.0
      gj1=0.0
      bj1=0.0
      braddc=-1000.0
      write (outbuf,620) exnamc(iba),basekv(ixnamn(iba)),
     1  exnamc(jba),basekv(ixnamn(jba)),ipcd,ksect
      call prtout (1)
  620 format (1h0,'PARALLEL ',a8,1x,f5.1,1x,a8,1x,f5.1,1x,a1,1x,i1,' WAS
     1 added to the network')
      go to 800
  640     write (errbuf(1), 660)
          call prterr ('E',1)
  660 format (15x,'PERCENT FIGURE EXCEEDS 100')
      go to 990
  680     write (errbuf(1), 700)
          call prterr ('E',1)
  700 format (15x,18h no percent figure)
      call erexit
  760     write (errbuf(1), 780)
          call prterr ('E',1)
  780 format (35h0type 3 fault on unsymmetrical line)
      go to 990

C     PROCEED TO FORM INTERMEDIATE BUS
  800 gii3=0.0
      bii3=0.0

C     LOCATE IJ BRANCH INTBL
      ibt=iba
      ibold=indx2o(iba)
      jbold=indx2o(jba)
      iboldt=ibold
      jboldt=jbold
      jbt=jba
      ifltx=ifl
  820 kk1=kk(1,iboldt)+kecst
      kk2=kk(2,iboldt)
      call redecs (bsbr,kk1,kk2)
      do 840 i870=13,kk2,3
      if (jboldt.eq.jbus(i870)) go to 880
  840 continue
      write (errbuf(1), 860)
      call prterr ('E',1)
  860 format (15x,26h do loop exhausted, st-870)
      link=3
      go to 990
  880 go to (920,900), noleg
  900 gii1=0.0
      bii1=0.0
      gij1=gijnl/p800
      bij1=bijnl/p800
      go to 940
C     
C     CALCULATE NEW PI EQUIVALENT FROM CONSTANTS OF PERCENT OF LINE
C     
  920 angl1=p800*angl
      vmag1=exp(p800*vlog)
      varr1=vmag1*cos(angl1)
      vari1=vmag1*sin(angl1)
      vtmpr=varr1**2+vari1**2-1.0
      vtmpi=2.0*vari1
      denom1=1.0/(vtmpr+2.0*(varr1+1.0))
      gii1=(gcnst*vtmpr-bcnst*vtmpi)*denom1
      bii1=(gcnst*vtmpi+bcnst*vtmpr)*denom1
      denom2=1.0/(vtmpr**2+vtmpi**2)
      gij1=2.0*(gcnst*varr1*vtmpr+bcnst*vari1*(vtmpr+2.0))*denom2
      bij1=2.0*(-gcnst*vari1*(vtmpr+2.0)+bcnst*varr1*vtmpr)*denom2
C     
C     TEST FOR PARALLEL LINES
C     
  940 diff=abs(gij(i870)+gj1)+abs(bij(i870)+bj1)
      if (diff.lt..0001) go to 960
C     
C     REMOVE SWITCH LINE FROM PARALLEL
C     
      gij(i870)=gij(i870)+gj1
      bij(i870)=bij(i870)+bj1
      i870=kk2+1
      kk1=kecsx
      kk2=kk2+3
      kk(1,iboldt)=kk1-kecst
      kk(2,iboldt)=kk2
      kecsx=kecsx+kk2
C     
C     ADD NEW BUS J INTO TABLE AND FOR INTERMEDIATE BUS
C     
  960 jbus(i870)=nmx
C     
C     ADD NEW LINE TO DEFAULT DISTANCE RELAY BYPASS TABLE
C     
      if(iswbps .ne. 0)then
         nbypas = nbypas + 1
         kbpass(1,nbypas) = ibt
         kbpass(2,nbypas) = nmx
      endif
      gij(i870)=-gij1
      bij(i870)=-bij1
      ijsln(1,ifltx)=kbsknt+1
      ijsln(2,ifltx)=ibt
      ijsln(3,ifltx)=ksect
      ijslc(ifltx)=ipcd
      gsl(2*ifltx-1)=gii1
      bsl(2*ifltx-1)=bii1
      gsl(2*ifltx)=gii1
      bsl(2*ifltx)=bii1
      gijs(ifltx)=gij1
      bijs(ifltx)=bij1
      bradd(ifltx)=braddc
      gii3=gii3+gii1+gij1
      bii3=bii3+bii1+bij1
      gii=gii-gi1-gj1+gij1+gii1
      bii=bii-bi1-bj1+bij1+bii1
      call ritecs (bsbr,kk1,kk2)
      if (ibt.eq.jba) go to 1000
      gij3=gij1
      bij3=bij1
      ibt=jba
      jbt=iba
      iboldt=jbold
      jboldt=ibold
      p800=1.0-p800
      lst=lst-1
      ifltx=lst
      if (lst.gt.imon) go to 820
      write (errbuf(1), 980)
      call prterr ('E',1)
  980 format (26h0 line data table exceeded)
  990 continue
      iabort=1
      go to 1040
C     
C      BUILD INTERMEDIATE BUS DATA
C     
 1000 do 1020 i=1,12
 1020 bsbr(i)=0.0
      gii=gii3
      bii=bii3
      jbus(13)=ibold
      gij(13)=-gij3
      bij(13)=-bij3
      jbus(16)=jbold
      gij(16)=-gij1
      bij(16)=-bij1
      call ritecs (bsbr,kecsx,18)
C     
C     SAVE ONE ADJACENT BUS FOR INTERMED. BUS ID.
C     
      intbus = ibold
C     
C     NOTE THAT  THE KK TABLE ENTRIES FOR THE NEW BUS GO IN THE
C     RENUMBERED TABLE.
C     
      kk(1, nmx )=kecsx-kecst
      kk(2, nmx )=18
      kecsx=kecsx+18
 1040 continue
C     
C     ERROR CHECK ON BOTH COMPENSATED LINES  & FAULTED BUS WHICH ARE
C     RELOCATED TO THE BOTTOM
C     
      if (ifltsw .eq. 1 .and. lcomp .eq. 2 ) then
        write (errbuf(1),1025)
 1025   format ( ' Both flt bus and compensated lines reorder violation.
     &..job aborted' )
        call prterr ('E',1)
        iabort = 1
      endif
C     
C     START OF BUS RENUMBER BLOCK                           *
C     THIS BLOCK OF LOGIC RENUMBERS THE BUSES BY HOLDING THE       *
C     BUSES NEAR THE FAULT TO THE BOTTOM OF THE MATRIX.  BUSES   * *
C     WHICH WERE NOT ACTIVE IN THE POWER FLOW ARE ELIMINATED.    * *
C     INITIALIZE TEMPORARY BRANCY COUNT TABLE
C     
      ibs=0
      kkhold=-10000
C     
C     OPTION TO RELOCATE COMPENSATED LINES
C     
      if (lcomp .eq. 1  .or. ncomp .eq. 0 )  go to 1060
      iduml = 2*ncomp
      ilast =0
      idummy = 0
      idmx = nmx +1
 1028 do 1030 i= 1, iduml
      if ( idum(i) .eq. 0 ) go to  1030
      if ( idum(i) .eq. idmx ) then
        idum(i) =  0
      else
        if ( idum(i) .gt. idmx ) go to 1030
        ilast = i
        idmx = idum(i)
      endif
 1030 continue
      if ( ilast .eq. 0 ) go to 1035
      idummy  = idummy +1
      ibrbn(idummy) = idmx
      idmx = nmx + 1
      idum(ilast) = 0
      ilast =  0
      go to 1028
C     
C     BEGIN PREPARING TABLE FOR REORDERING
C     
 1035 if ( lcomp .ne. 1 ) then
        do 1054 i = 1, idummy
        jj = ibrbn(i)
        ibs = ibs + 1
        ibrs (ibs) =  kk( 2,jj)
 1054   kk( 2,jj) = kkhold
      endif
C     
C     OPTION TO IGNORE RELOCATION OF FLT BUS TO THE BOTTOM ROW
C     
 1060 if (ifltsw .ne. 1 ) go to 1140
      ib=iabs(iftabn(1))
      jb=iabs(jftabn(1))
      ibold=indx2o(ib)
      jbold=indx2o(jb)
C     
C     DETERMINE FAULT TYPE
C     
      go to (1100,1100,1100,1120,1140),mde
C     
C     SET UP FOR BUS FAULT
C     REMOVED REORDERING OF ADJACENT BUS ON BUS FLTS
C     
 1100 ibs=ibs+1
      ibrbn(ibs)=jbold
      ibrs(ibs)=kk(2,jbold)
      kk(2,jbold)=kkhold
 1120 ibs=ibs+1
      ibrbn(ibs)=ibold
      ibrs(ibs)=kk(2,ibold)
      kk(2,ibold)=kkhold
 1140 indmx=kslack
      indn=1
      indo=2
      if (kslack.eq.1) go to 1220
 1160 if (kk(2,indo).lt.0) go to 1180
      if( kk(1,indo) .lt. 0) go to 1180
C     
C *   IF KK(2,INDO) = 8, INDO IS A PASSIVE DC BUS WHICH MUST BE
C *   IGNORED
C     
      if(kk(2,indo) .gt. 12) go to 1170
      write (outbuf,1165)
      call prtout (1)
      write (outbuf,1166) (bsbr(lm),lm=1,8)
      call prtout (1)
      write (outbuf,1167) (bsbr(lm),lm=9,12)
      call prtout (1)
 1165 format('0STMT 1160 =')
 1166 format(1x,8e15.5)
 1167 format(31x,4e15.5)
      kbsknt = kbsknt-1
      nmx=nmx-1
      kpdc=kpdc+1
      jndo=indo2x(indo)
      kpsdcn(1,kpdc)=jndo
      write (outbuf,1177) kpdc,indo,jndo,exnamc(jndo),
     1  basekv(ixnamn(jndo))
      call prtout (1)
 1177 format(1h0,'PASSIVE DC BUS ',3i10,5x,a8,2x,f6.1)
      kpsdcc(2,kpdc)=exnamc(jndo)
      kpsdcn(2,kpdc)=ixnamn(jndo)
      go to 1180
 1170 indn2o(indn) = indo
      indn=indn+1
 1180 if (indo.eq.indmx) go to 1200
      indo=indo+1
      go to 1160
 1200 if (indo.gt.kslack) go to 1260
 1220 if (kk(2,1).lt.0) go to 1240
      indn2o(indn)=1
      indn=indn+1
 1240 if (indmx.eq.ntot) go to 1260
      indmx=ntot
      if (kslack.ne.1) indo=indo+1
      go to 1160
 1260 if (ibs.eq.0) go to 1300
      if (ibs.gt.25) go to 1340
      do 1280 i=1,ibs
      indo=ibrbn(i)
      indn2o(indn)=indo
      indn=indn+1
 1280 kk(2,indo)=ibrs(i)
 1300 continue
C     
C     OPTION TO REORDER INTERMED. FLT BUS TO 1ST ROW
C     
      nmxsel = nmx
      if ( mde .ne. 3 ) go to 1315
      if ( ifltsw .ne. 1 ) then
        nmxp1=nmx+1
        nmxm1=nmx-1
        do 1310 i = 1, nmxm1
 1310   indn2o(nmxp1-i) = indn2o(nmx-i)
        indn2o (1) = nmx
        indo2n(nmx) = 1
        nmxsel = nmx
      else
        indo2n(nmx) = nmx
        indn2o(nmx) = nmx
      endif
 1315 if (indn-1.eq.kbsknt) go to 1380
      write (errbuf(1), 1320)
 1320 format ('0RENUMBERING ERROR, PROGRAM STOPPED.')
      call prterr ('E',1)
      call erexit
 1340 write (errbuf(1),1360)
 1360 format (1h0,'MORE THAN 25 FAULTED OR MODIFIED LINES')
      call prterr ('E',1)
      call erexit
 1380 continue
      if (keybrd(7).eq.0) go to 1420
      call forbtm
      call fortop
      write (outbuf, 1400)
 1400 format ('0INDN2O')
      call prtout (1)
      do 1402 jjj = 1,nmxsel,10
        kkk = min0 (jjj+9,nmxsel)
        write (outbuf, 1401) (indn2o(i), i = jjj,kkk)
 1401   format (10i10)
        call prtout (1)
 1402 continue
C     
C     END OF BUS RENUMBER BLOCK
C     START OF TABLE REORDER BLOCK
C     
 1420 do 1440 indn=1,nmx
      indo=indn2o(indn)
      kkp(1,indn)=kk(1,indo)
 1440 kkp(2,indn)=kk(2,indo)
      do 1460 indn=1,nmx
      indo=indn2o(indn)
      indo2n(indo)=indn
 1460 continue
      ktbl=kecst-2*nmx-2
C     
C     CHECK ADEQUACY OF ECS FIELD LENGTH.
C     
      kerr = kcor + 4*nmx + isg + 1 - ktbl
      if (kerr.lt.0) go to 2178
      write (buffer,2172) kerr
 2172 format (' ECS INSUFFICIENCY ',i6)
      call prterr ('E',1)
      write (errbuf(1), 2174) (buffer(i), i = 1, 4)
 2174 format (1x,4a10)
      call prterr ('E',1)
      write (buffer,2176) kerr
 2176 format (' MIN ECS REQUIRED  ',i6)
      write (outbuf, 2174) (buffer(i), i = 1, 4)
      call prtout (1)
 2178 continue
      do 1500  ind = 1, nmx
      kk1=kkp(1,ind)
      kk2=kkp(2,ind)
      call redecs (bsbr,kk1+kecst,kk2)
      iend=kk2-2
      do 1480 ind1=13,iend,3
 1480 jbus(ind1)=indo2n(jbus(ind1))
 1500 call ritecs (bsbr,kk1+kecst,kk2)
      if (lphase.eq.0) go to 1540
      do 1520 i=1,lphase
      ii=itbphn(1,i)
      jj=itbphn(2,i)
      ii=indo2n(ii)
      jj=indo2n(jj)
      if(jj .lt. ii) then
        itbphn(1,i) = jj
        itbphn(2,i) = ii
      else
        itbphn(2,i) = jj
        itbphn(1,i) = ii
      endif
 1520 continue
 1540 if (keybrd(7) .ne. 0) then
         write (outbuf, 1560)
         call prtout (1)
         do 1562 jjj = 1,nmx,10
         kkk = min0 (jjj+9,nmx)
         write (outbuf, 1561) (indo2n(i), i = jjj,kkk)
         call prtout (1)
 1562    continue
 1560    format (1h0,5x,'INDO2N')
 1561    format (10i10)
      endif
C     
C     CONVERT VOLTAGE,REAL AND REACTIVE, TO NEW ORDER
C     
      if( mde.eq.3 .and. ifltsw.eq.2 )  then
C     
C       SET VOLTAGE FROM ADJACENT BUS FOR NEW INTERMEDIATE BUS
C     
        vold1(nmx) = vold1(intbus)
        vold2(nmx) = vold2(intbus)
      endif
      do indn=1,nmxsel
        indo=indn2o(indn)
        eyr(indn)=vold1(indo)
        eyi(indn)=vold2(indo)
      enddo
      if (keybrd(11).eq.0) go to 1610
      write (outbuf,1601)
      call prtout (1)
 1601 format (1h0,10x,5hvolts)
      do 1602 i1=1,nmxsel,4
      k = min0 (i1+3,nmxsel)
      write (outbuf,1603) i1,(eyr(j),eyi(j),j=i1,k)
      call prtout (1)
 1602 continue
 1603 format(1x,i4,8e13.6)
C     
C     RENUMBER BUSES FOR LINES WITH ZNO CAPACITORS FROM OLD
C     POWER FLOW ORDER TO NEW SWING ORDER
C     
 1610 lcompf = nmx
      if(iznmax .ne. 0)then
        do 1615 i= 1,iznmax
          ib = iznbus(i)
          jb = jznbus(i)
          iznbus(i) = indo2n(ib)
          jznbus(i) = indo2n(jb)
C     
C         GET LOW BUS NO. FOR COMP. LINES
C     
          if ( iznbus(i) .lt. lcompf ) lcompf = iznbus(i)
          if ( jznbus(i) .lt. lcompf ) lcompf = jznbus(i)
 1615   continue
      endif
C     
C     REORDER CAPCOR TABLES
C     
      if ( mde .eq. 3 ) indo2x(nmx) = nmx
      do 1620 indn=1,nmxsel
      indo=indn2o(indn)
      capcor(1,indn)=capold(1,indo)
      capcor(2,indn)=capold(2,indo)
      indx=indo2x(indo)
 1620 indn2x(indn)=indx
      if (keybrd(7).ne.0) then
        call forbtm
        call fortop
        write (outbuf, 1640)
 1640   format ('0INDN2X')
        call prtout (1)
        do 1642 jjj = 1,nmxsel,10
        kkk = min0 (jjj+9,nmxsel)
        write (outbuf, 1641) (indn2x(i), i = jjj,kkk)
 1641   format (10i10)
        call prtout (1)
 1642   continue
      endif
C     
C      REORDER IPACKT TABLE
C     
      do 1674 indn = 1,nmxsel
      do 1674 i = 1,3
      ktempn(i,indn) = ipactn(i,indn)
 1674 continue
      do 1680 indn=1,nmxsel
      indx=indn2x(indn)
      do 1680 i=1,3
      ipactn(i,indn)=ktempn(i,indx)
1680  continue
      ifirst=3
      jfirst=1
      irectp = 53                                                       !dem
      idesc = 53                                                        !dem
      irecln = 1                                                        !dem
      if (debug) then                                                   !dem
        call dbgeko ('INITL1 - writing bus count to history file.')     !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
        call dbgwri ('  KBSKNT /bus count /  = ',kbsknt)                !dem
      endif                                                             !dem
      call puthisri (irectp,idesc,irecln,kbsknt)                        !dem
c     WRITE (L8) IFIRST,JFIRST,KBSKNT
C     
C     REORDER BUS NAME TABLE
C     AND FORM TABLE INDX2N ( EXTERNAL TO INTERNAL SWING )
C     
      do 1699 i = 1,ntotd
      nac(i) = exnamc(i)
 1699 ixn(i) = ixnamn(i)
      do 1700 indn=1, nmxsel
      indx=indn2x(indn)
      exnamc(indn)=nac(indx)
      ixnamn(indn)=ixn(indx)
 1700 indx2n(indx)=indn
      do 1705 i = 1,ntot
 1705 nac(i) = exzonc(i)
      do 1710 indn = 1,nmxsel
      indx =  indn2x(indn)
 1710 exzonc(indn) = nac(indx)
      if (mde.ne.3) go to 1720
      if ( ifltsw .eq. 1 ) then
        ipr = nmx
      else
        ipr = 1
      endif
      exnamc(ipr) = 'INTRMDTE'
      itrr = indo2n(intbus)
      ixnamn(ipr) = ixnamn(itrr)
 1720 kpandq = kcor
      kvolts=kpandq+1
      kmat=kvolts+1
      knewt=kmat+1
      ksup=knewt+ 1
      kigent=ksup + 1
      irectp = 57                                                       !dem
      idesc = 57                                                        !dem
      irecln = ntotd                                                    !dem
      if (debug) then                                                   !dem
        call dbgeko2 ('INITL1 - writing alpha-to-new bus renum table ', !dem
     +    'to history file.')                                           !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
      endif                                                             !dem
      call puthisri (irectp,idesc,irecln,indx2n)                        !dem
c     WRITE (L8) (INDX2N(I),I=1,NTOTD)
      if (keybrd(7) .ne. 0) then
         write (outbuf, 1740)
 1740    format (1h0,5x,'EXNAME')
         call prtout (1)
         do 1742 jjj = 1,nmx,6
           kkk = min0 (jjj+5,nmx)
           write (outbuf, 1741) (exnamc(izz), ixnamn(izz),
     1          izz = jjj,kkk)
 1741      format (1x,6(a8,i4,2x))
           call prtout (1)
 1742    continue
      endif
C     
C     PRINT ECS POINTERS
C     
 1760 if (keybrd(8) .ne. 0) then
        write (outbuf,1780)
 1780   format ('0',11x,'KCOR',4x,'KPANDQ',4x,'KVOLTS',6x,'KMAT',5x,
     1          'KNEWT',6x,'KIGENT',4x,'KTBL',4x,'KECST')
        call prtout (1)
        write (outbuf,1781) kcor,kpandq,kvolts,kmat,knewt,kigent,ktbl,
     1                      kecst
 1781   format (5x,8i10)
        call prtout (1)
      endif
C     
C      CONVERT VARIABLE ADMITTANCE MODULATION BUS NUMBERS TO NEW ORDER
C     
      if(iznmax .ne. 0)then
         do 1782 ind = 1,iznmax
         if(ivymsw(ind) .ne. 0)then
            ivymb1(ind) = indx2n(ivymb1(ind))
            ivymb2(ind) = indx2n(ivymb2(ind))
         endif
 1782    continue
      endif
C     
C     CONVERT LINE SWITCHING TABLES TO NEW NUMBERS
C     
 1860 if (ifcd.eq.0) go to 1980
      do 1960 i=1,ifcd
C     
C     BYPASS THIS LOGIC FOR MULT DC SWITCHING CARDS
C     
      if(iabs(mflt(i)).eq.6) go to 1960
      i1=iabs(iftabn(i))
      i2=indx2n(i1)
      if (iftabn(i).lt.0) go to 1880
      iftabn(i)=i2
      if(mflt(i).gt.6) go to 1960
      go to 1900
 1880 iftabn(i)=-i2
 1900 if (iabs(mflt(i)).ne.4) go to 1920
      go to 1960
 1920 i1=iabs(jftabn(i))
      i2=indx2n(i1)
      if (jftabn(i).lt.0) go to 1940
      jftabn(i)=i2
      go to 1960
 1940 jftabn(i)=-i2
 1960 continue
C     
C     CONVERT PHASE SHIFTERS TO NEW NUMBERS
C     
 1980 if(lphase.eq.0)go to 2045
      do 2040 i=1,lphase
      ij1=itbphn(1,i)
      ij2=itbphn(2,i)
C     
C     ITBLPH MUST BE ORDERED SO THAT PHASE SHIFTERS WILL BE PICKED UP
C     IN THE SAME ORDER THAT THE BUSES ARE ENCOUNTERED IN THE
C     REDUCTION ROUTINE
C     
      gi1=gijph(i)
      bi1=bijph(i)
      if (i.eq.1) go to 2040
      do 2000 j=2,i
      j1=i-j+1
      if(itbphn(1, j1) - ij1) 2020, 1985, 1987
 1985 if(itbphn(2, j1) - ij2) 2020, 2020, 1987
 1987 itbphn(1, j1 + 1) = itbphn(1, j1)
      itbphn(2,j1+1)=itbphn(2,j1)
      gijph(j1+1)=gijph(j1)
 2000 bijph(j1+1)=bijph(j1)
      j1=j1-1
2020  continue
      itbphn(1,j1+1)=ij1
      itbphn(2,j1+1)=ij2
      gijph(j1+1)=gi1
      bijph(j1+1)=bi1
 2040 continue
C     
C     CALL RLINT1 TO CLEAN-UP DEFAULT DISTANCE RELAY BYPASS TABLE
C     
 2045 if(iswbps .ne. 0)call rlint1
C     
C     CALL ZNOINT TO INITIALIZE ZNO CAPACITOR MODEL
C     
      if(iznmax .ne. 0) call znoint
      write (outbuf,2050)
      call prtout (1)
 2050 format(1h0,'SUBROUTINE INITL1 HAS BEEN PROCESSED.')
      if(kpdc.eq.0) go to 2081
      if(kpdc.le.40) go to  2070
      write (errbuf(1),2060)
      call prterr ('E',1)
 2060 format('0', 76h we can not have more than 40 passive dc busses. --
     1----- job will be aborted)
      iabort=1
2070  continue
      kdc2=kpdc
 2081 continue
      return
      end
