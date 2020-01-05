C    %W% %G%
       subroutine rdbsets
c
c      read data from old format ipf base file into tsp variables
c      read logic was consolidated into a single module
c      from many different tsp modules.
c
       implicit none

c     l3, swcas
      include 'tspinc/blkcom1.inc'
c     pfcase, ...
      include 'tspinc/titles.inc'
c     ntot, mtdcbs, ...
      include 'tspinc/comn34.inc'
c     bmva, ...
      include 'tspinc/param.inc'
c     constants maxbus, maxgen, mxlshd, maxmac 
      include 'tspinc/params.inc'
c     exnamc, exzonec
      include 'tspinc/namec.inc'
c     exbase
      include 'tspinc/in1n.inc'
c     areanz
      include 'tspinc/areanz.inc'
c     brnch, kbrnch
      include 'tspinc/brnch.inc'
c     store
      include 'tspinc/bypass.inc'
c     kecsym
      include 'tspinc/cntrl2.inc'
c     ibxyz
      include 'tspinc/pointr.inc'
c     errbuf
      include 'tspinc/prt.inc'
c     kk,vold1,vold2 - share these with reformat modules
      include 'tspinc/rddtai_mv.inc'

c     real brvctr
c     integer kk,ktempn,ixn,indo2n,indn2o,indo2x,indx2o
c     real vold1, vold2, capold
c     dimension indn2o(MAXBUS),indo2x(MAXBUS),indx2o(MAXBUS),
c    1 indo2n(MAXBUS), ktempn(3,MAXBUS), ixn(MAXBUS),
c    2 vold1(MAXBUS), vold2(MAXBUS), capold(2,MAXBUS), kk(2,MAXBUS)
c     dimension brvctr(54*MAXBUS)
c     equivalence (brvctr,jbrnch)
c     equivalence (indo2n,brvctr(1)), (indn2o,brvctr(MAXBUS+1)),
c    1  (indo2x,brvctr(2*MAXBUS+1)), (indx2o,brvctr(3*MAXBUS+1)),
c    2  (vold1,brvctr(4*MAXBUS+1)), (vold2,brvctr(5*MAXBUS+1)),
c    3  (capold,brvctr(6*MAXBUS+1)), (kk,brvctr(8*MAXBUS+1)),
c    4  (ktempn,brvctr(10*MAXBUS+1)), (ixn,brvctr(13*MAXBUS+1))

c
c     local variables
c
c     tapewk
c
      dimension nslkxx(4,10)
      integer nslkxx
      character*10 auxout
      character*10 pfcas,clbel1,clbel2,pversn,usrnam
      integer count(100), kount
      dimension kount(100)
      integer mtdcbs, mtdcln
      integer ntot2, kbrknt, nztot, natot, ntotcs, nbslck
      integer kslloc
      integer junk
      integer i,j,k
      integer missbr
      integer i1,i2
      real xbase
c      character errbuf*255
c
c    initl1
c
c      integer kk
c      real vold1, vold2, capold
c      dimension vold1(MAXBUS), vold2(MAXBUS),
c     2 capold(2,MAXBUS), kk(2,MAXBUS)
c     integer indx2o, indo2x
c     dimension indx2o(MAXBUS), indo2x(MAXBUS) 
      integer num
c  move these to rddtai_mv.inc
c      integer jphid
c      dimension jphid(8,50) 
c
c    dcinp
c
       integer jjj, kkk
c       common /dcdtanc/dcdtan(45,10)
c       real dcdtan
       integer keybrd
c      character outbuf*255
c
c    mdcinp
c
c      common /mdcinpc/kdcx(36,40),dcline(10,50)
c      integer kdcx, kdclin
c      real dcline, dcx
c      dimension kdclin(10,50), dcx(36,40)
c      equivalence (dcline, kdclin)
c      equivalence (dcx, kdcx)
c
c  code to read the input data from a base file
c 
c
c dlc tapewk.f:      move data read by tapewk to tsp commons
c
      write(*,'(1x,a)') 'rdoldbse: tapewk reads'
C * * *
C * * * IF IREUSE=0, THEN CASE IS A BASE CASE
C * * * IF IREUSE =1, THEN CASE IS A CHANGE CASE
  112 ireuse=0
      if(savin.ne.blnk10)ireuse=1
C     -   Fetch Pwrflo admin data.  Look for case ID match only on
c         first header record (no loop-back for 2nd or later cases).
  120 read (l3,end=240) auxout,pfcas ,pfdate,clbel1,clbel2,
     1                  pversn,usrnam,count
c     write (*,'(1x,a)') ' TAPEWK - dbg - SN 120: display PF header '     
c     write (*,'(3x,8i4)') (ichar (auxout(la:la)), la = 1,8)          
  140 if (auxout .ne. 'PF DATA') then                                
         write (errbuf(1),180)
  180    format (' TAPEWK - Powerflow base file has improper',      
     +     ' header.' )                                            
         call prterr ('E',1)
c 180    FORMAT(5X,'THE FILE ASSIGNED AS THE POWER FLOW DATA HISTORY',
c    1      ' (.BSE) IS NOT FROM THE POWER FLOW.')
c        ERRBUF(1) = ' POWERFLOW NOT FOUND'
c        CALL PRTERR ('E',1)
         call erexit
      endif
C     -   PFCAS is from PF base file.  Force to upper case.
      call upcase (pfcas)
      call upcase (pfcase)
c     IF (PFCAS.EQ.PFCASE) GO TO 250
c     IF (PFCAS.EQ.'END') THEN
c     just jump this check for now
      pfcas = ' '
      if (pfcas .eq. ' ') go to 250                                    
      if (pfcas .ne. pfcase) then                                     
        write (errbuf(1),'(2A)') 'TAPEWK - Requested PF case not ',  
     +    'found in PF base file.'                                  
        call prterr ('E',1)
        write (errbuf(1),220) pfcase,pfcas
 220      format ('  Stab requested ',a10,'.  Base file has ',a10,'.')  
        call prterr ('E',1)
c 220    FORMAT ('  CASE ( ',A10,' ) IS NOT ON THE POWER FLOW DATA ',
c    1           'HISTORY FILE. (',A10,')'  )
         errbuf(1) = ' POWERFLOW NOT FOUND'
         call prterr ('E',1)
         call erexit
      endif
      goto 250                                                     
c     GO TO 120
  240 errbuf(1) = 'TAPEWK - premature end-of-file in PF base.'    
c 240 ERRBUF(1) = ' POWERFLOW NOT FOUND'
      call prterr('E',1)
      call erexit
c 250 IF(COUNT(1).EQ.1) THEN
c        GO TO 300
  250 if (count(1) .ne. 1) then
        write (errbuf(1),260)
        call prterr ('E',1)
  260   format (' THE POWER FLOW SOLUTION FAILED.' )
        errbuf(1) = ' POWERFLOW FAILED '
        call prterr ('E',1)
        call erexit
      endif

C
C     END OF LOGIC TO LOCATE THE PARTICULAR CASE
C
C     READ LIST OF VARIABLES FROM POWER FLOW TAPE
C
       read(l3) ntot,ntot2,ltot,ntota,
     1          ntotb,ntotc,kdtot,mtdcbs,mtdcln,kxtot,
     2          bmva,jtie,kecsy,kbsknt,
     3          kbrknt,jphno,nztot,natot,ntotcs,
     4          nbslck,nslkxx,kount
               kslloc=nslkxx(1,1)
               kslack=nslkxx(2,1)
      kecsym=kecsy
        kmdcbs=mtdcbs
        kmdcbr=mtdcln
C     *
C     * CHECK POWER FLOW VERSION NUMBER
C     *
      if(ntotcs .ne. 0 .and. kmdcbs .ne. 0)then
         write (errbuf(1),325)
         write (errbuf(2),326)
         call prterr ('E',2)
  325    format(1h0,'DC DATA ERRORS IN POWER FLOW HISTORY FILE' )
  326    format(' POWER FLOW IS UNACCEPTABLE FOR TRANSIENT STABLILITY ',
     1           'STUDIES.')
         write (errbuf(1),333)
         call prterr ('E',1)
  333    format(1h0,'POWER FLOW HISTORY FILE CONTAINS  MULTI-TERMINAL',
     1          ' DC DATA WHICH IS NOT ACCEPTABLE.')
         errbuf(1) = ' BAD DC DATA'
         call prterr ('E',1)
         call erexit
      endif
      if(ntot .gt. MAXBUS) then
          write (errbuf(1),336)ntot,MAXBUS
          call prterr ('E',1)
  336     format('BUS LIMIT VIOLATION DETECTED IN TAPEWK.  THIS CASE ',
     1           'HAS ',i5,' BUSES BUT PROGRAM PERMITS ONLY ',i5)
          call erexit
      endif
      if (bmva.eq.0.0) bmva=100.0
      ntotd=ntot
      lphase=jphno
      kecsx=80000
      kecst=kecsx
      kzrecs=1
C
C
C     CONVERT BASE KV TO A CODE AND THEN COMBINE THE BUS NAME AND
C     BASE CODE.  BUS NAME IS LEFT 48 BITS,BASE CODE IS RIGHT 12 BITS
C
c moved above read
      do 520  i = 1, MAXBUS
  520 exbase(i)=0.0
      call ritecs (exbase,kzrecs,2100)
c
c
      read (l3) (exnamc(i),i=1,ntotd)                     
      read (l3) (exbase(i),i=1,ntotd)                    
      read (l3) (exzonc(i),i=1,ntot)                    
      read (l3) junk
      ibxyz=0
      jdelet=0
      do 440 i=1,ntotd
  360 xbase=exbase(i)
      if(ibxyz.ne.0)go to 400
  380 ibxyz=ibxyz+1
      basekv(ibxyz)=xbase
      go to 440
  400 do 420 k=1,ibxyz
      if(basekv(k).eq.xbase)go to 440
  420 continue
      go to 380
  440 continue
c
c dlc
c
      num=(ntot+99)/100+(ntot2+99)/100
      do 560 i=1,num
      read (l3) junk
  560 continue
      if (ntotc .ne. 0) then
         read (l3) (areanc(i),i=1,ntotc)                    
         read (l3) junk
         read (l3) junk
         read (l3) junk
         read (l3) ((areazc(i,j),i=1,10),j=1,ntotc)        
         read (l3) junk
         read (l3) junk
         read (l3) junk
         read (l3) junk
      endif

c
c dlc input2.f
c
      write(*,'(1x,a)') 'rdoldbse: input2 reads'
         if (kxtot.gt.0) read (l3) junk
  680 ibr=ibr-1
        missbr=0
       do 1045 i1 = 1,ltot,100
       i2 = min0(i1+99,ltot)
       read (l3) ((brnch(j,i),j=1,18), i=i1,i2)                         c brnch
 1045 continue

c
c dlc initl1.f
c
      write(*,'(1x,a)') 'rdoldbse: initl1 reads'
      read (l3) (indo2x(i),i=1,ntot)                 
      read (l3) (indx2o(i),i=1,ntotd)               
c
      read (l3) (vold1(i),i=1,ntot)                
      read (l3) (vold2(i),i=1,ntot)               
      read (l3) ((capold(j,i),j=1,2),i=1,ntot)   
      if (jphno.eq.0) go to 1180
      read(l3) ((jphid(i,j),i=1,8),j=1,jphno)
c     see initl1 for logic to shuffle jphid
c * * * READ BUS DATA TABLE FROM POWER FLOW HISTORY FILE
C * * * AND STORE IN ECS. THIS TABLE IS CALLED BSBR.
C * * * KK(I,J) IS THE INDEX TO THE BSBR FOR EACH BUS
C * * * AND THE LENGTH OF TABLE FOR THAT BUS.
C * * *
 1180 read (l3) ((kk(i,j),i=1,2),j=1,ntot)             
 1210 if(kecsy.le.4000)go to 1220
       read(l3) (store(i),i = 1,4000)                 
      call ritecs(store,kecsx,4000)
      kecsx=kecsx+4000
      kecsy=kecsy-4000
      go to 1210
 1220 read(l3) (store(i),i=1,kecsy)                  
      call ritecs(store,kecsx,kecsy)
      kecsx=kecsx+kecsy
      kecst=kecst-1
 1235 kecsy=kecsym
c
c dlc dcinp.f
c
C     *
C     * READING DC INFORMATION FROM POWER FLOW TAPE
C     *
      write(*,'(1x,a)') 'rdoldbse: dcinp reads'
       num=(ntotb+49)/50
       if (num.eq.0) go to 2200
       do 2180 i=1,num
 2180 read (l3) junk
 2200 num=(ntota+49)/50
       if (num.eq.0) go to 2240
       do 2220 i=1,num
       read (l3) junk
 2220 read (l3) junk
 2240 num=(jtie+99)/100
       if (num.eq.0) go to 2280
       do 2260 i=1,num
 2260 read (l3) junk
 2280  continue
       if (kdtot .eq. 0)then
          ldc = 0
          go to 2283
       endif
       read (l3) ((dcdtan(j,k),j=1,45),k=1,kdtot)       
       if ( keybrd(22) .ne. 0) then
          do 22294 k = 1,kdtot
          do 12294 jjj = 1,45,8
          kkk = min0 (jjj+7,45)
          write (outbuf,2294) (j,dcdtan(j,k),j=jjj,kkk)	 
          call prtout (1)
12294    continue
22294    continue
 2294    format(8(i5,e11.4))
       endif
2283   continue

c
c dlc mdcinp.f
c
c       CALL MDCINP TO PROCESS MULTITERMINAL DATA IF IT EXISTS
C * * *
C * * * READ DC BUS DATA KDCX AND DC LINE DATA DCLINE FROM POWER FLOW
C * * *
       if(kmdcbs .ne. 0) then
         write(*,'(1x,a)') 'rdoldbse: mdcinp reads'
         read (l3) ((kdcx(j,i),j=1,36),i=1,kmdcbs)      
         read (l3) ((dcline(j,i),j=1,10),i=1,kmdcbr)   
       endif

       goto 9090
c
c dlc nout1.f and nout2.f also read base - 
c see code in rd2oldbse.f
c 
c     wrapup
c 
 9090 continue

      return
      end
