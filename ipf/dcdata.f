C    @(#)dcdata.f	20.7 10/13/99
      subroutine dcdata
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com009.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/dcsrt.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/work1.inc'
  
      common /is_batch / is_batch
c
c***kln Local variables changed for the single to double conversion.
c
      double precision y, cx, v, vtot, p, error, ang, vdo, gnom, gmin, 
     &       a1, a2, a(30,31), ldcbus(36,50)
      dimension ldc(50), loc(51), ndc(50)
      character busc*8, dc_code*1
      external kmpkl2,swpkl2,kmpmdc,swpmdc
      integer pnxt, shift
c 
      ntotcs = 0
      nldc=0
      nbdc=0
      ntdc=0
      kerrsw=0  
      if (kdtot+mtdcbs .eq. 0) go to 1220 
C            COMBINE TWO TERMINAL DC DATA WITH MULTI-TERMINAL DC DATA   
      if (mtdcbs .eq. 0) go to 140
      do i = 1,mtdcbs
         do j = 1,36
            dcbus(j,i)=dcmtbs(j,i)
         enddo
         dcbus(22,i) = i
      enddo
      if (mtdcln .eq. 0) go to 130
      do i=1,mtdcln
         do j=1,10
            dcline(j,i)=dcmtln (j,i)
         enddo
      enddo
  130 continue
      nldc=mtdcln
      nbdc=mtdcbs

  140 if (kdtot .eq. 0) go to 220
      do 210 i=1,kdtot
         k1=dc2t(1,i)
         ksw=1
  150    nbdc=nbdc+1
         do j=1,22
            k = kdcx(j,ksw)
            if (k .gt. 0) dcbus(j,nbdc) = dc2t(k,i)
         enddo

         call putchr_8 (1, '2', dcbus(4,nbdc))
         dcbus(29,nbdc) = 0.0d0
         dcbus(22,nbdc)=-i
         if (ksw.eq.2) go to 180
         ksw=2
         go to 150
 
  180    dcbus(6,nbdc)=0.0d0
         if (dc2t(7,i).eq.1) go to 190
         dcbus(5,nbdc-1)=0.0d0
         dcbus(5,nbdc)=-dc2t(5,i)
         go to 200
 
  190    dcbus(5,nbdc-1)=dc2t(5,i)
         dcbus(5,nbdc)=0.0d0
  200    nldc=nldc+1
         dcline(1,nldc)=dc2t(1,i)
         dcline(2,nldc)=dc2t(3,i)
         dcline(3,nldc)=dc2t(11,i)
         dcline(4,nldc)=dc2t(8,i)
         dcline(7,nldc) = 0
  210 continue
C
C     BUILD DC CONNECTION MATRIX
C
  220 do 230 i=1,nldc
         k1=dcline(1,i)
         k2=dcline(2,i)
         kolum(2*i-1) = ipack_2 (k1, k2)
         kolum(2*i) = ipack_2 (k2, k1)
  230 continue
      ltotdc=2*nldc
      call qiksrt (1,ltotdc,kmpkl2,swpkl2)
      klast=0
      ntotdc=0
      do 240 i=1,ltotdc
         k1 = shift (kolum(i), -16)
         k2 = shift ( shift (kolum(i), 16), -16)
         kolum(i) = k2
         if (k1.eq.klast) go to 240
         ntotdc=ntotdc+1
         ldc(ntotdc)=k1
         loc(ntotdc)=i
         klast=k1
  240 continue
      i=ltotdc+1
      loc(ntotdc+1)=i   
      ltotdc=i-1
C       
C     Search for disconnected d-c buses 
C       
      do 256 i = 1, nbdc
         nb = dcbus(1,i)
         do 252 j = 1, ntotdc
            if (ldc(j) .eq. nb) go to 256
  252    continue
 
      if (nb .eq. 0) then
         j = -dcbus(22,i)
         if (j .gt. 0) then
            nb = dmax1 (dc2t(1,j), dc2t(3,j))
            write (errbuf(1),253) bus(nb),base(nb)
  253       format (' Incomplete 2-terminal d-c system ',a8,f6.1)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw=1
         else
            j = -j
            nb = dcmtbs(1,j)
            write (errbuf(1),254) bus(nb),base(nb)
  254       format (' Disconnected d-c bus ',a8,f6.1)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw=1
         endif
      else
         write (errbuf(1),255) bus(nb),base(nb)
  255    format (' Disconnected d-c bus ',a8,f6.1)
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerrsw=1
      endif
 
  256 continue
C
C     FIND ID OF EACH DC CIRCUIT
C
      idckt=0
      n1=0
      n2=0
      if (ntotdc .eq. 0) go to 1220
  260 if (n1.lt.n2) go to 300
      if (n2.eq.ntotdc) go to 370
      do 270 i=1,ntotdc
         k1=ldc(i)
         if (k1.gt.0) go to 280
  270 continue
      call erexit
  280 n2=n2+1
      mdc(n2)=k1
      ldc(i)=-k1
      idckt=idckt+1 
      ndc(i)=idckt  
      if (iopton(15) .ne. 0) write (dbug,290) bus(k1),base(k1),idckt  
  290 format(' DC BUS ',a8,f7.1,' IS ASSIGNED TO DC CIRCUIT NO.',i2)
  300 n1=n1+1   
      k1=mdc(n1)
      do 310 j = 1,ntotdc
         if (iabs(ldc(j)).eq.k1) go to 320
  310 continue
      call erexit
  320 j1 = loc(j)
      j2 = loc(j+1)
  330 if (j1.ge.j2) go to 260
      k2=kolum(j1)
      do 340 j=1,ntotdc
         if (ldc(j).eq.k2) go to 350
  340 continue
      go to 360
 
  350 n2=n2+1
      mdc(n2)=k2
      ldc(j)=-k2
      ndc(j)=idckt
      if (iopton(15) .ne. 0) write (dbug,290) bus(k2),base(k2),idckt
  360 j1=j1+1
      go to 330
 
  370 if (idckt.le.15) go to 390
      write (errbuf(1),380)
  380 format('0 MORE THAN 15 SEPARATE DC CIRCUITS. ')
      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      kerrsw=1
C
C     REORDER DC BUS ARRAY BY BOTH CIRCUIT ID AND SCHEME I
C
  390 do 400 i=1,ntotdc
         k1=-ldc(i)
         mdc(i)=10000000*ndc(i)+20000*(loc(i+1)-loc(i))+k1
  400 continue
      call qiksrt(1,ntotdc,kmpmdc,swpmdc)
      nckt(1)=1
      j1old=0
      do 480 i=1,ntotdc
         jckt=mdc(i)/10000000
         k1=mod(mdc(i),20000)
         if (jckt .ne. j1old) go to 410
         k1old=k1old+1
         nckt(jckt+1)=nckt(jckt+1)+1
         go to 420
 
  410    k1old=1
         j1old=jckt
         nckt(jckt+1)=nckt(jckt)+1
  420    mdc(i)=jckt
         ndc(i)=k1old
         ldc(i) = k1
         if (k1old.le.10) go to 440
         write (errbuf(1),430) jckt
  430    format ('0 MORE THAN 10 DC BUSSES ON DC CIRCUIT NO. ',i2)
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerrsw=1
  440    do 450 j=1,nbdc
            if (dcbus(1,j).eq.k1) go to 460
  450    continue
         call erexit
  460    dcbus(30,j) = k1old
         dcbus(21,j) = jckt
         do 470 k=1,36
            ldcbus(k,i)=dcbus(k,j)
  470    continue
  480 continue
C
C     REORDER "DCBUS" ARRAY (BY SCHEME 1) AND INITIALIZE CONTROLS
C
      do 490 i=1,ntotdc
         do 482 j=1,26
  482    dcbus(j,i)=ldcbus(j,i)
         dcbus(27,i) = 0
         dcbus(28,i) = 0
         dcbus(29,i) = ldcbus(29,i)
         dcbus(30,i) = ldcbus(30,i)
         do 484 j = 31,36
  484    dcbus(j,i) = 0
  490 continue
C
C     ADD CROSS INDEX TO DC LINE ARRAY
C
      do 540 i=1,nldc
         k1=dcline(1,i)
         do 500 j=1,ntotdc
            if (ldc(j).eq.k1) go to 510
  500    continue
         k2 = dcline(2,i)
         if (k1.gt.0.and.k2.gt.0) then
            write(errbuf(1),502) bus(k1),base(k1),bus(k2),base(k1)
  502       format ('DC TERMINAL BUS IS NOT A DC BUS ',
     &            a8,f6.1,2x,a8,f6.1)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               kerrsw = 1
               go to 540
            else
               call prterx ('F',1)
               call erexit()
            endif
         else
            kkx = k1
            if (kkx .eq. 0) kkx = k2
            write(errbuf(1),504) bus(kkx),base(kkx)
  504       format (' INCOMPLETE DC SYSTEM ',a8,f6.1)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               kerrsw = 1
               go to 540
            else
               call prterx ('F',1)
               call erexit()
            endif
         endif
  510    dcline(8,i)=j
         dcline(7,i)=mdc(j)
         dcline(5,i)=ndc(j)
         k2=dcline(2,i)
         do 520 j=1,ntotdc
            if (ldc(j).eq.k2) go to 530
  520    continue
         k2 = dcline(2,i)
         if (k1.gt.0.and.k2.gt.0) then
            write(errbuf(1),502) bus(k1),base(k1),bus(k2),base(k1)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               kerrsw = 1
               go to 540
            else
               call prterx ('F',1)
              call erexit()
            endif
         else
            kkx = k1
            if (kkx .eq. 0) kkx = k2
            write(errbuf(1),504) bus(kkx),base(kkx)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               kerrsw = 1
               go to 540
            else
               call prterx ('F',1)
               call erexit()
            endif
         endif
  530    dcline(9,i)=j
         dcline(6,i)=ndc(j)
  540 continue
C
C     BUILD DC Y-MATRIX
C
      if (kerrsw .eq. 1) go to 592
      do 590 i=1,idckt
         ntdc = nckt(i+1) - nckt(i)
         do j=1,10 
            do k=1,10 
              dcy(k,j,i)=0.0
            enddo
         enddo
         do 580 j=1,nldc   
            if (dcline(7,j) .ne. i) go to 580   
            if (dcline(4,j).gt.0) go to 570   
            k1=dcline(1,j)
            k2=dcline(2,j)
            write (errbuf(1),560) bus(k1),base(k1),bus(k2),base(k2),
     &                            dcline(4,j)   
  560       format (' D-C line ', a8, f7.1, 2x, a8, f7.1,
     &              ' has an unfeasible resistance ', f11.3) 
            call prterx ('W',1)   
            dcline(4,j)=0.01  
            minerr=1  
  570       j1=dcline(5,j)
            j2=dcline(6,j)
            y=1.0/dcline(4,j) 
            dcy(j1,j1,i)=dcy(j1,j1,i)+y   
            dcy(j1,j2,i)=dcy(j1,j2,i)-y   
            dcy(j2,j1,i)=dcy(j2,j1,i)-y   
            dcy(j2,j2,i)=dcy(j2,j2,i)+y   
  580    continue  
  590 continue  
C       
C     OBTAIN COMMUTATING IMPEDANCE  
C       
  592 do 690 i=1,ntotdc 
         k1x=dcbus(1,i)   
         k2x=dcbus(3,i)   
         n1x = inp2opt(k1x)  
         n2x = 0   
         if (k2x .ne. 0) n2x = inp2opt(k2x)
         cx=1.0
         rat=1.0   
         gkm=0 
         bkm=0 
         it=0  
         if (k2x .ne. 0) go to 610   
         do 600 j = 15,18  
  600       dcbus(j,i) = 0.0d0
         go to 690 
  610    continue  
         pnxt = kbsdta(16,k1x)
         do while (pnxt .gt. 0) 
            if (ky(pnxt) .eq. k2x) then
               ltyp = brtype(pnxt)
               nbrptr = brnch_ptr(pnxt)
               nbr = iabs (nbrptr)
               if (ltyp .eq. 4) then
                  do j = 1, ntota
                     l1x = min0 (ltran(1,j),ltran(9,j))  
                     l2x = max0 (ltran(1,j),ltran(9,j))  
                     if (min0(n1x,n2x) .eq. l1x .and. 
     &                   max0(n1x,n2x) .eq. l2x) then
                        it = j
                        go to 644
                     endif
                  enddo
                  write (errbuf(1),642) bus(k1x), base(k1x), bus(k2x),
     &                  base(k2x)   
  642             format('0 MISSING D-C COMMUTATING LTC ',a8,f6.1,
     &                   ' TO ',a8,f6.1)
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  kerrsw=1
  644             continue
               else if (ltyp .eq. 5) then
                  if (nbrptr .gt. 0) then
                     e1 = brnch(9,nbr)/base(k1x)
                     e2 = brnch(10,nbr)/base(k2x)
                  else
                     e1 = brnch(10,nbr)/base(k1x)
                     e2 = brnch(9,nbr)/base(k2x)
                  endif
                  rat = e1/e2 
                  cx = cx*e1**2   
                  rt = brnch(5,nbr)   
                  xt = brnch(6,nbr)   
                  zt = rt**2 + xt**2
                  gkm = gkm + rt/zt
                  bkm = bkm - xt/zt
               else if (ltyp .eq. 3 .or. ltyp .eq. 8) then
                  rt = brnch(5,nbr)   
                  xt = brnch(6,nbr)   
                  zt = rt**2 + xt**2
                  gkm = gkm + rt/zt
                  bkm = bkm - xt/zt
               endif
            endif
            pnxt = brnch_nxt(pnxt)
         enddo
 
         yt = gkm**2+bkm**2
         yt = amax1(yt,10e-10)
         rt = gkm/yt
         xt = -bkm/yt
         cx = cx*base(k1x)**2 / bmva * dcbus(7,i)

         dcbus(15,i) = it   
         dcbus(16,i) = rat   
         dcbus(17,i) = cx*rt 
         dcbus(18,i) = cx*xt 
  690 continue  
C       
C     COMPUTE HYBRID MATRIX FOR DETERMINING THE INITIAL CURRENTS AND
C     VOLTAGES  
C       
      if (kerrsw .eq. 1) go to 1182
      do 1180 jckt=1,idckt  
         ntdc=nckt(jckt+1)-nckt(jckt)  
         nec = nckt(jckt) - 1  
         do j=1,30 
            do k=1,30 
              a(k,j)=0.0d0
            enddo
         enddo
         do j=1,ntdc   
            do k=1,ntdc   
               a(k,j)=dcy(k,j,jckt)  
               a(j,j+ntdc)=-1.0d0  
            enddo
         enddo
C       
C        DETERMINE ACTIVE VOLTAGE CONSTRAINTS  
C       
         m=0   
         vtot=0.0d0
         do 740 i=1,ntdc   
            m1=0  
            do j = 19,20  
              dcbus(j,i+nec) = 0.0d0
            enddo
            do j = 23,28  
              dcbus(j,i+nec) = 0.0d0
            enddo
            v=dcbus(6,i+nec)  
            if (v .gt. 0) then
               vtot=vtot+v
               m=m+1  
               dcbus(20,i+nec)=v  
               a(m+ntdc,i)=1.0d0
               m1=m+ntdc  
               dcbus(27,i+nec) = m1  
            endif 
            dcbus(31,i+nec)=0
            dcbus(32,i+nec)=0
            dcbus(33,i+nec)=0
  740    continue  
         if (vtot.gt.0.0d0) go to 760  
         write (errbuf(1),750) jckt
  750    format ('0 DC CIRCUIT NO. ',i2,' HAS NO SCHEDULED VOLTAGE. ') 
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerrsw=1  
         go to 940 
C       
C        DETERMINE ACTIVE CURRENT CONSTRAINTS  
C       
  760    if (ntdc .eq. m) then 
            write (errbuf(1),780) jckt 
            call prterx ('W',1)
            m = m - 1  
         endif 
         vtot=vtot/(ntdc-m)
         do 810 i=1,ntdc   
            if (dcbus(20,i+nec) .eq. 0.0d0) dcbus(20,i+nec)=vtot
            p=dcbus(5,i+nec)  
            if (p .ne. 0.0) go to 770 
            if (dcbus(3,i+nec) .ne. 0) go to 810   
  770       m=m+1 
            if (m.le.ntdc) go to 800  
            write (errbuf(1),780) jckt
  780       format (' DC CIRCUIT NO. ',i2,
     &              ' IS EXCESSIVELY CONSTRAINED.') 
            call prterx ('W',1)   
C       
C           THE PRESENT CURRENT CONSTRAINT MUST BE ABANDONED.  
C           HOWEVER, IF IT IS A PASSIVE NODE, IT MUST DISPLACE 
C           SOME OTHER ACTIVE CURRENT CONSTRAINT.   
C       
            if (dcbus(3,i+nec) .ne. 0) go to 810   
            j = i 
  790       j = j - 1 
            if (j .eq. 0) go to 810 
            if (dcbus(3,j+nec) .eq. 0) go to 790   
            j2=dcbus(31,j+nec)   
            if (j2 .eq. 0) go to 790
            a(j2,j+ntdc) = 0.0d0
            a(j2,i+ntdc) = 1.0d0
            dcbus(31,j+nec) = 0  
            dcbus(31,i+nec) = j2 
            j3=dcbus(1,j+nec)
            write (errbuf(1),792) bus(j3),base(j3),dcbus(31,j+nec)
  792       format(' SCHEDULED DC POWER AT CONVERTER ',a8,f6.1,' IS ',
     1              f7.1,' MW,BUT MUST BE RELAXED.')
            call prterx ('W',1)
            go to 810
 
  800       a(m+ntdc,i+ntdc)=1.0d0
            dcbus(31,i+nec) = m+ntdc
  810    continue
         if (m.ge.ntdc) go to 840
         write (errbuf(1),820) jckt,ntdc,m
  820    format ('DC CIRCUIT NO. ',i2,
     1           ' IS INSUFFICIENTLY CONSTRAINED: ',
     1           i2,' NODES AND ',i2,' CONSTRAINTS. ')
         call prterx ('W',1)
         m=m+1
         do 830 i=m,ntdc
            a(i+ntdc,i+ntdc)=1.0d0
  830    dcbus(31,i+nec) = i+ntdc
  840    m=2*ntdc
         if (iopton(15) .eq. 0) go to 870
         do 850 i=1,m  
  850    write (dbug,860) (a(i,j),j=1,m)   
  860    format (1x,20f6.3)
  870    continue  
         itcnt=1   
C       
C        SOLVE FOR INITIAL DC VOLTAGES AND CURRENTS
C
  880    do 890 i=1,ntdc
            x(i) = 0
  890    x(i+ntdc) = 0
         do 910 i=1,ntdc
            i1=dcbus(27,i+nec)
            i2=dcbus(31,i+nec)
            if (i1 .ne. 0) x(i1)=dcbus(6,i+nec)
            if (i2 .eq. 0) go to 900
            x(i2) = dcbus(5,i+nec)/dcbus(20,i+nec)
            go to 910
  900       dcbus(5,i+nec) = 0.0d0
  910    continue
         do 920 i=1,m
            do 920 j=1,m
  920    aa(j,i)=a(j,i)
         call invert (m)
         if (ierr .eq. 0) go to 940
         write (errbuf(1),930) jckt,m,ierr
  930    format ('0 DC CIRCUIT NO. ',i2,' IS A SINGULAR SYSTEM. ORDER ',
     1            i2,' RANK ',i2)
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerrsw=1
  940    error=0
         do 950 i=1,ntdc
            dcbus(20,i+nec)=x(i)
            dcbus(19,i+nec)=x(i)*x(i+ntdc)
            if(dcbus(31,i+nec) .eq. 0) go to 950
            error=error+dabs(dcbus(19,i+nec)-dcbus(5,i+nec))
  950    continue
         if (iopton(15) .ne. 0) write(dbug,960) jckt,itcnt,error
  960    format ('  INITIALIZATION: CIRCUIT ',i2,' ITERATION ',i2,
     1           ' ERROR ',e10.2)
         if (error.le.1.0) go to 970
         if (kerrsw .ne. 0) go to 970
         itcnt=itcnt+1 
         if (itcnt.le.10) go to 880
  970    write (outbuf,980) jckt   
  980    format ('0 DC CIRCUIT NO. ',i2)   
         call prtout (1)   
         write (outbuf,990)
  990    format ('0 CONVERTER ',10x,
     1           '--SCHEDULED---',4x,'--- INITIAL ---   COMMUTATOR ',
     2           'BUS     DC LINE TERMINALS  BRIDGES    VDO')
         call prtout (1)   
         write (outbuf,1000)   
 1000    format (1h ,21x,'(MW)    (KV)      (MW)    (KV)', t107, 
     1           '(KV)')   
         call prtout(1)
         do 1090 i=1,ntdc  
            j1=dcbus(1,i+nec)
            j2=dcbus(3,i+nec)
            do 1010 k=1,ntdc  
 1010       a(k,i)=dcy(k,i,jckt)  
            if (j2 .eq. 0) go to 1020   
            busc=bus(j2)  
            basec=base(j2)
            go to 1030

 1020       busc='        '   
            basec=0.0 

 1030       ksw=0 
            do 1080 j=1,ntdc  
               if (j.eq.i) go to 1080
               if (a(i,j) .eq. 0.0) go to 1080   
               j3 = dcbus(1,j+nec)  
               if (ksw .eq. 0) then   
                  write (outbuf,1040) bus(j1),base(j1),
     1               (dcbus(k,i+nec),k=5,6),(dcbus(k,i+nec),k=19,20),
     2                busc,basec,bus(j3),base(j3)
 1040             format ('0',a8,f7.1,3x,2f8.1,2x,2f8.1,5x,a8,f7.1,4x,
     &                    a8,f7.1)   
                  ksw = 1
               else  
                  write (outbuf,1060) bus(j3),base(j3)   
 1060             format (77x,a8,f7.1)   
               endif 
               if (j2 .gt. 0) then   
                  ang = dcbus(12,i+nec)  
                  adi = dcbus(19,i+nec) / dcbus(20,i+nec)
                  rci = 0.95492 * dcbus(18,i+nec) * dcbus(7,i+nec)
                  vdo = dcbus(20,i+nec) + abs (adi) * rci
                  vdo = vdo / cos (ang)
                  write (outbuf(96:),1072) dcbus(7,i+nec), vdo   
 1072             format (f6.0, f12.3)   
               endif 
               call prtout (1)   
 1080       continue  

 1090    continue  
C       
C        CONVERT DC BUS TO INTERNAL ORDER  
C       
         if (iopton(15) .ne. 0) write (dbug,1100)
 1100                         format('0 DUMP OF ARRAY "DCBUS" '/)   
         do 1170 i=1,ntdc  
            k = dcbus(1,i+nec)   
            dcbus(1,i+nec) = inp2opt(k)
            j = dcbus(3,i+nec)   
            if (j .ne. 0) dcbus(3,i+nec) = inp2opt(j)
            gnom=dcbus(12,i+nec)  
            gmin=dcbus(29,i+nec)  

            call getchr_8 (1, dc_code, dcbus(4,i+nec))
            if (dc_code .eq. 'I') then   
               if (gnom .le. gmin) then   
                  a1 = 57.29577951*gnom   
                  a2 = 57.29577951*gmin   
                  write (errbuf(1),1120) bus(k), base(k), dc_code, 
     &               a1, a2 
 1120             format(' DC CONVERTER ',a8,f7.1,' TYPE ',a1,
     1                   ' HAS IMPROPER EXTINCTION ANGLES: GNOM ',
     2                   f6.1,' GMIN ',f6.1)  
                  call prterx ('W',1) 
                  gnom = gmin 
                  dcbus(12,i+nec) = gnom  
               endif  
            else if (dc_code .eq. 'M') then  
               if (gnom .lt. gmin) then   
                  a1 = 57.29577951*gnom   
                  a2 = 57.29577951*gmin   
                  write (errbuf(1),1140) bus(k), base(k), dc_code, 
     &                a1, a2 
 1140             format(' DC CONVERTER ',a8,f7.1,' TYPE ',a1,
     1                   ' HAS IMPROPER EXTINCTION ANGLES: GNOM ',
     2                   f6.1,' GMIN ',f6.1)  
                  call prterx ('W',1) 
                  if (gnom .ne. 0.0 .and. gmin .ne. 0.0) then 
                     a1 = dmin1 (gnom,gmin)   
                     a2 = dmax1 (gnom,gmin)   
                  else if (gnom .eq. 0.0 .or. gmin .eq. 0.0) then 
                     a1 = dmax1 (gnom,gmin)   
                     a2 = dmax1 (gnom,gmin)   
                  endif   
                  dcbus(29,i+nec) = a1
                  dcbus(12,i+nec) = a2
               endif  
            endif 
            if (iopton(15) .ne. 0) then
                write (dbug,1160) i, ifix(sngl(dcbus(1,i+nec))), 
     &              dcbus(2,i+nec), ifix(sngl(dcbus(3,i+nec))),
     &              (dcbus(k,i+nec),k=4,11), gnom, gmin,
     &              (dcbus(k,i+nec),k=13,14), 
     &              ifix(sngl(dcbus(15,i+nec))),
     &              (dcbus(k,i+nec),k=16,20),
     &              ifix(sngl(dcbus(21,i+nec))),
     &              ifix(sngl(dcbus(30,i+nec))),
     &              ifix(sngl(dcbus(22,i+nec))) 
 1160           format(1x, i2, i5, f6.1, i5, 1x, a1, f6.0, f6.0, f4.0,
     &              2f6.1, 5f6.3, f6.1, i4, f6.3, 2f6.1, 2f7.0, 3i2)  
            endif
 1170    continue  

 1180 continue  
 1182 continue
C       
C     CONVERT DC LINE TO INTERNAL ORDER 
C       
      if (iopton(15) .ne. 0) write (dbug,1190)
 1190 format('0 DUMP OF ARRAY "DCLINE" '/)  
      do i=1,nldc  
         k1=dcline(1,i)
         dcline(1,i)=inp2opt(k1) 
         k2=dcline(2,i)
         dcline(2,i)=inp2opt(k2)
         if (iopton(15) .ne. 0) then
            write (dbug,1200) i,(ifix(sngl(dcline(k,i))),k=1,2),
     1                        (dcline(k,i),k=3,6),
     2                        (ifix(sngl(dcline(k,i))),k=7,9)
 1200       format(1x,i2,2i5,4f7.1,3i5)
         endif
      enddo

 1220 return
      end   
