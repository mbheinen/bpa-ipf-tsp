C    @(#)reordr.f	20.10 5/3/00
      integer function reordr (temp)
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/elim.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/renum.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'

      common /scratch/ nbr, array(2,100)
      integer array, komparbr

      common /is_batch / is_batch

      dimension jorder(MAXBUS), jbnew(MAXBUS), ksystm(3,10)
      integer   brtot

      character type*1

      external komparbr, swap_br

      reordr = 0         ! set success flag
      kerrsw = 0         ! set error flag to no errors

      call forbtm
      outbuf = ' * * * Pre-solution processing * * *'
      call rpnlod
      call fortop
      outbuf = ' * * Reordering * *'
      call shdlod(1)
      outbuf = ' '
      call shdlod(2)
      call shdlod(3)
      call shdlod(4)
      call shdlod(5)
      call shdprt
c
      write (outbuf,485) ltot,MAXBRN
  485 format ('  Number of branch items:',i5,' (maximum: ',i5,')')
      call dbgprt(1)
      call prtout(1)
      call dbgprt(0)
c
      nsize = 2.2*(float(ltot)+5.0*float(jtie)+2.0*float(ntotb)) + 10
      if (nsize .gt. MXORDR) then
         write (errbuf(1),150) nsize, MXORDR
  150    format (' Reordering array overflow: allocated:',i5,
     1           ', required: ',i5,')')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerrsw = 1
         go to 1500
      endif

      do i = 1, ntot
         loc1(i) = 0
      enddo

      if (knew .eq. 2) then
c
c        Reorder all y-matrix data to external order
c
         if (ordvlt .eq. 2) then
            ordvlt = 1
            call mvnew1d (e,opt2inp,ntot)
            call mvnew1d (f,opt2inp,ntot)
         endif
         if (ordcap .eq. 2) then
            ordcap = 1
            call mvnew2d (capcor,opt2inp,ntot)
         endif

         ordymx = 1
         ordltc = 1
         ordtbx = 1
         orddc  = 1
         ordtie = 1

         call mvnew1  (km,opt2inp,ntot)
         call mvnew1  (kmlen,opt2inp,ntot)
         call mvnew1d (pnetu,opt2inp,ntot)
         call mvnew1d (qnetu,opt2inp,ntot)
         call mvnew1d (gkku,opt2inp,ntot) 
         call mvnew1d (bkku,opt2inp,ntot) 
         call mvnew1  (ntypu,opt2inp,ntot)
         call mvnew1  (nspar,opt2inp,ntot)   
         call mvnew1  (vlimn,opt2inp,ntot)   
         call mvnew1  (vlimx,opt2inp,ntot)   
         call mvnew1d (ploadu,opt2inp,ntot)   
         call mvnew1d (qloadu,opt2inp,ntot)   
         call mvnew1d (inetr,opt2inp,ntot)   
         call mvnew1d (ineti,opt2inp,ntot)   
      endif

      if (kase1(40) .ne. 0) then
         write (dbug, 190)
  190    format (' Debug dump of renumber overlay ')
         write (dbug, 200) (i, bus(i), base(i), i = 1, ntot)
  200    format (5(i6,2x,a8,f7.1))
      endif
c
c     kolum, korder, and loc arrays are assembled from y-matrix.
c
      mptr = 0
      do nb = 1, ntot
         loc1(nb) = mptr+1
         nbr = kmlen(nb)
         kownt(2,nb) = nbr
         if (nbr .eq. 0) then
            if (kbsdta(1,nb) .ne. 12 .and. bus(nb) .ne. srtlst) then
               call typno (type, kbsdta(1,nb))
               write (errbuf(1), 270) bus(nb), base(nb), type
  270          format(' A-C bus ',a8,f6.1,' type ',a1,
     &                ' has no branches')
               call prterx ('W',1)
               kerrsw = 1
            endif
            loc1(nb) = 0
            kownt(2,nb) = 0
         else
c 
c           Retrive branches in input order...
c
            brtot = 0
            do i = km(nb), km(nb)+nbr-1
               brtot = brtot + 1
               array(1,brtot) = i
               j = ikmu(i) 
               if (knew .eq. 2) then
                  j = opt2inp(j)
                  ikmu(i) = j
               endif
               array(2,brtot) = j
            enddo

            call shellsrt (1, brtot, komparbr, swap_br)
 
            do i = 1, brtot
               m = array(2,i)
               mptr = mptr + 1
               kolum1(mptr) = m
               kordr1(mptr) = mptr+1
            enddo
            kordr1(mptr) = 0
         endif
      enddo
      kbrknt = mptr / 2
      next1 = mptr + 1
      next2 = mptr + 1
      last1 = nsize
      last2 = nsize
      do k = next1, last1
         kolum1(k) = 0
         kordr1(k) = k+1
      enddo
      kordr1(last1) = 0
      do k = 1, ntot
         jorder(k) = kownt(2,k)
         loc2(k) = loc1(k)
      enddo
      do k = 1, nsize
         kordr2(k) = kordr1(k)
         kolum2(k) = kolum1(k)
      enddo
c
c     check system connectivity
c
      isystm = 0
      islknb = 0
      istart = 0
      nsyst = 0
      nindex = 1
      ioldst = 1
c
c     Exempt passive multi-terminal dc busses from ac connectivity test
c
      do i = 1, mtdcbs
         if (dcmtbs(3,i) .eq. 0) then
            nb = dcmtbs(1,i)
            if (jorder(nb) .eq. 0) then
               jbnew(nindex) = nb
               nindex = nindex + 1
            endif
         endif
      enddo
c
c     Exempt deleted buses from ac connectivity test
c
      do i = ntot_alf+1, ntot
         nb = alf2inp(i)
         if (jorder(nb) .eq. 0) then
            jbnew(nindex) = nb
            nindex = nindex + 1
         endif
      enddo

      do 500 nb = 1, ntot
         if (jorder(nb) .gt. 0) then
            jorder(nb) = -jorder(nb)
            if (kbsdta(1,nb) .eq. 3) islknb = islknb+1
            jbnew(nindex) = nb
            istart = nindex + 1
            ksystm(2,nsyst+1) = nindex
            do while (nindex .lt. istart)
               kt = jbnew(nindex)
               l1 = loc1(kt)
               do while (l1 .gt. 0)
                  k2 = kolum1(l1)
                  if (jorder(k2) .gt. 0) then
                     jorder(k2) = -jorder(k2)
                     if (kbsdta(1,k2) .eq. 3) islknb = islknb+1
                     jbnew(istart) = k2
                     istart = istart+1
                  endif
                  l1 = kordr1(l1)
               enddo
               nindex = nindex+1
            enddo
c
c           print out sub-system statistics
c
            isystm = isystm+1
            j = nindex-ioldst
            if (islknb .ne. 1) then
               write (errbuf(1),490) isystm, j, islknb
  490          format(' A-C system ',i2, ' with ', i4,
     1                ' buses has ', i2, ' system slack bus(es) ')
               call prterx ('W', 1)
               ksystm(1,nsyst+1) = isystm
               ksystm(3,nsyst+1) = nindex
               nsyst = nsyst + 1
            endif
            if (nindex .gt. ntot) go to 560
            ioldst = istart
            islknb = 0
         endif
  500 continue
      write (errbuf(1), 512)
  512 format(' Disconnected subsystem. Composition of smaller ',
     &       'subsystem (partial if more than 48 buses) follows.')
      errbuf(2) = ' '
      j = 0
      k = 0
      if (nindex .gt. ntot/2) then
         do 530 i = 1, ntot
            if (jorder(i) .gt. 0) then
               j = j + 1
               if (j .gt. 48) go to 552
               k = (j + 5)/6
               l = mod (j-1,6) + 1
               if (l .eq. 1) errbuf(k+2) = ' '
               write (errbuf(k+2)(16*l-15:), 520) bus(i), base(i)
  520          format (1x,a8,f7.1)
            endif
  530    continue
      else
         do 550 i = 1, ntot
         if (jorder(i) .lt. 0) then
            j = j + 1
            if (j .gt. 48) go to 552
            k = (j + 5)/6
            l = mod (j-1,6) + 1
            if (l .eq. 1) errbuf(k+2) = ' '
            write (errbuf(k+2)(16*l-15:), 520) bus(i), base(i)
         endif
  550    continue
      endif
  552 call prterx ('W',k+2)
      kerrsw = 1
      go to 1500
  560 continue
c
c     "NSYST" is the subsystem number without a slack bus.
c
      if (nsyst .gt. 0) then
         do i = 1, nsyst
            write (errbuf(1), 562) ksystm(1,i)
  562       format(' Subsystem ',i2,' has no slack bus. Composition '
     1            ,'(partial if more than 48 buses) follows.')
            errbuf(2) = ' '
            k1 = ksystm(2,i)
            k2 = min0(ksystm(3,i)-1,k1+47)
            j = 0
            do m = k1, k2
               j = j + 1
               k = (j + 5)/6
               l = mod (j-1,6) + 1
               if (l .eq. 1) errbuf(k+2) = ' '
               n = jbnew(m)
               write (errbuf(k+2)(16*l-15:), 520) bus(n), base(n)
            enddo
            if (is_batch .eq. 0) then
               call prterx ('E',k+2)
            else
               call prterx ('F',k+2)
            endif
            kerrsw = 1
         enddo
      endif
c
c     Perform incidence-symmetric check on a-c connection matrix
c
      if (kase1(40) .ne. 0) then
C         call rdump
C         call symchk
          call cktopol2 (kerr)
          if (kerr .eq. 1) kerrsw = 1
      endif
c
c     The function I(i) = Y(i,j)*V(j) is depicted in a linear
c     digraph as
c
c                   i <--- j
c
c     "i" is the positive-incident branch with respect to "j"
c     "j" is the negative-incident branch with respect to "i".
c
c     The positively incident graph is defined with arrays: "loc1",
c        "kolum1", and "kordr1";
c     The negative incident graph is defined with arrays: "loc2",
c        "kolum2", and "kordr2".
c
      if (jtie .eq. 0) go to 920
      if (kabort .ne. 0) go to 920
c
c     Replace area slack node injection constraints with tie line
c     control constraints.
c
      do i = 1, ntotc
         nb = karea(1,i)
         do j = 1, nbslck
            if (nb .eq. nslkxx(1,j)) go to 580
         enddo
         kownt(2,nb) = -i
  580    continue
      enddo
c
c     Pass 1: remove all negatively directed branches from
c     area slack busses.
c
      write (outbuf,600)
  600 format ('  Non-symmetric reordering performed on area ',
     &   'interchange. ')
      call prtout(1)
      do 690 k = 1, ntot
         if (kownt(2,k) .lt. 0) then
            l2 = loc2(k)
            lx = 0
            do while (l2 .gt. 0)
               k2 = kolum2(l2)
               l1 = loc1(k2)
               last = 0
               do while (l1 .gt. 0)
                  k1 = kolum1(l1)
                  if (k1 .lt. k) then
                     last = l1
                     l1 = kordr1(l1)
                  else if (k1 .eq. k) then
                     ly = kordr1(l1)
                     if (last .gt. 0) then
                        kordr1(last) = ly
                     else
                        loc1(k2) = ly
                     endif
                     kordr1(last1) = l1
                     kordr1(l1) = 0
                     last1 = l1
                     go to 620
                  endif
               enddo
  620          continue
               lx = l2
               l2 = kordr2(l2)
            enddo
            if (loc2(k) .gt. 0) then
               kordr2(last2) = loc2(k)
               kordr2(lx) = 0
               last2 = lx
               loc2(k) = 0
            endif
         endif
  690 continue
c
c     pass 2: add negatively directed branches from
c     "tie" array to area slack busses
c
      do 910 k = 1, ntot
         i = -kownt(2,k)
         if (i .le. 0) go to 910
         if (loc2(k) .ne. 0) call rdump
c
c        "kdiag" -- 0/1 node k has no/has diagonal element
c
         kdiag = 0
         nbr = 0
         do 730 jt = 1, jtie
            k1 = tie(1,jt)
            k2 = tie(7,jt)
            i1 = tie(2,jt)
            i2 = tie(8,jt)
            i3 = tie(9,jt)
            if ((i1 .eq. i .or. i2 .eq. i) .and. i3 .eq. 0) then
               kx12 = min0(k1,k2)
               if (kx12 .eq. k) then
                  kdiag = 1
               else
                  nbr = nbr + 1
                  jorder(nbr) = kx12
               endif
               kx12 = max0(k1,k2)
               if (kx12 .eq. k) then
                  kdiag = 1
               else
                  nbr = nbr + 1
                  jorder(nbr) = kx12
               endif
            endif
  730    continue
         if (nbr .eq. 0) then
            write (errbuf(1),740) arcnam(i)
  740       format (' Area ', a10, ' has no a-c intertie branches.',
     &              ' Interchange control aborted.')
            call prterx ('W',1)
            kabort = 1
            iopton(17) = 2
c
c           Restore branches previously deleted
c
            kdiag = 1
            l2 = loc1(k)
            do while (l2 .gt. 0)
               nbr = nbr + 1
               jorder(nbr) = kolum1(l2)
               lx = kordr1(l2)
               l2 = lx
            enddo
         endif
         if (nbr .eq. 1) go to 840
c
c        A total of NBR busses are stored in JORDER. These identify
c        the negatively directed terminals for the slack busses. This
c        array is to be sorted with the redundant entities removed.
c
  770    khi = jorder(1)
         jhi = 1
         ksw = 1
         do 830 j = 2, nbr
  780       if (j .gt. nbr) go to 832
            if (jorder(j) .lt. khi) then
c
c              exchange jorder(j) ---> jorder(jhi)
c
               jorder(jhi) = jorder(j)
               jorder(j) = khi
               jhi = j
               ksw = 2
c
c              redefine 'khi' and 'jhi'
c
            else if (jorder(j) .gt. khi) then
               khi = jorder(j)
               jhi = j
c
c              duplicate entities are found. move the list up one
c              entitity, from "j" to "nbr"
c
            else
               nbr = nbr-1
               if (j .le. nbr) then
                  do l = j, nbr
                     jorder(l) = jorder(l+1)
                  enddo
                  go to 780
               endif
            endif
  830    continue
  832    if (ksw .eq. 2 .and. nbr .ge. 2) go to 770
c
c        The first NBRT busses in JORDER identify the negatively
c        incident branches from area slack bus K.
c
  840    nbrt = nbr
         if (kdiag .ne. 1) nbr = -nbrt
         loc2(k) = next2
         kownt(2,k) = nbr
         lastlx = 0
         do 900 j = 1, nbrt
            k1 = jorder(j)
c
c           add branch k <-- k1
c
            if (next2 .eq. 0) then
               write (errbuf(1), 150) nsize, MXORDR
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               kerrsw = 1
               go to 1500
            endif
            kolum2(next2) = k1
            lastlx = next2
            next2 = kordr2(next2)
            l1 = loc1(k1)
            last = 0
c
c           add branch k1 --> k
c
  850       if (l1 .gt. 0) then
               k2 = kolum1(l1)
               if (k2 .lt. k) then
                  last = l1
                  l1 = kordr1(l1)
                  go to 850
               else if (k2 .eq. k) then
                  go to 900
               endif
            endif
            if (next1 .eq. 0) then
               write (errbuf(1), 150) nsize, MXORDR
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               kerrsw = 1
               go to 1500
            endif
            if (last .gt. 0) then
               kordr1(last) = next1
            else
               loc1(k1) = next1
            endif
            kolum1(next1) = k
            lx = kordr1(next1)
            kordr1(next1) = l1
            next1 = lx
  900    continue
         kordr2(lastlx) = 0
  910 continue
      if (kase1(40) .ne. 0 .and. jtie .gt. 0) then
C         call rdump
C         call symchk
          call cktopol2 (kerr)
          if (kerr .eq. 1) kerrsw = 1
      endif
  920 continue
c
c     Replace AGC node injection constraints with associated system
c     slack bus injection constraint.
c
      if (numagc .gt. 0) then
         write (outbuf, 922)
  922    format (' Non-symmetric reordering performed on AGC.')
         call prtout(1)
c
c        Pass 1: Store slack bus P-constraints. Note that "loc1, kolum1,
c        kordr1, etc." may have been replaced with area tie constraints
c        requiring use of y-matrix to retrieve topology of p-constraints
c
         nbagc = 0
         nbr = 0
         do 940 i = 1, numagc
            if (kagc(9,i) .ne. 0) then
               nbagc = kagc(1,i)
               brtot = 0
               do j = km(nbagc), km(nbagc)+kmlen(nbagc)-1
                  brtot = brtot + 1
                  array(1,brtot) = j
                  array(2,brtot) = ikmu(j)
               enddo

               call shellsrt (1,brtot,komparbr,swap_br)
 
               do j = 1, brtot
                  m = array(2,j)
                  nbr = nbr + 1
                  jorder(nbr) = m
               enddo
c
c              Insert "diagonal" element.
c
               nbrx = nbr
               do 934 j = 1, nbrx
                  if (jorder(j) .gt. nbagc) then
                     do 932 k = nbrx, j, -1
  932                jorder(k+1) = jorder(k)
                     jorder(j) = nbagc
                     nbr = nbr + 1
                     go to 936
                  endif
  934          continue
               nbr = nbr + 1
               jorder(nbr) = nbagc
  936          continue
               go to 950
            endif
  940    continue
  950    if (nbr .eq. 0) go to 1040
c
c        Pass 2: Replace each AGC's P-constraint with the P_constaint
c        for the slack node NBAGC.  Area interchange constraints are
c        exempted. The system slack bus is replaces with with an
c        identity constraint.
c
         do 1030 i = 1, numagc
            nb = kagc(1,i)
            if (kagc(9,i) .ne. 0) then
               do 952 j = 1, nbslck
                  if (nslkxx(1,j) .eq. nb) go to 954
  952          continue
               go to 1030
  954          continue
            endif
            l2 = loc2(nb)
            lx = 0
  960       if (l2 .gt. 0) then
               k2 = kolum2(l2)
c
c              Eliminate K2 --> NB.
c
               l1 = loc1(k2)
               last = 0
  970          if (l1 .gt. 0) then
                  k1 = kolum1(l1)
                  if (k1 .lt. nb) then
                     last = l1
                     l1 = kordr1(l1)
                     go to 970
                  else if (k1 .eq. nb) then
                     ly = kordr1(l1)
                     if (last .gt. 0) then
                        kordr1(last) = ly
                     else
                        loc1(k2) = ly
                     endif
                     kordr1(last1) = l1
                     kordr1(l1) = 0
                     last1 = l1
                  endif
               endif
               lx = l2
               l2 = kordr2(l2)
               go to 960
            endif
c
c           Eliminate NB <-- K2 (by freeing storage).
c
            if (loc2(nb) .gt. 0) then
               kordr2(last2) = loc2(nb)
               kordr2(lx) = 0
               last2 = lx
               loc2(nb) = 0
            endif
c
c           For NB = NBAGC (the system slack bus), replace the
c           P-constraint with an identity constraint, which is
c           conveniently stored in JORDER(NBR+1).
c
c           For NB >< NBAGC, replace the P-constraint with the
c           P-constraint for bus NBAGC.
c
            loc2(nb) = next2
            lastlx = 0
            kdiag = 1
            if (kagc(9,i) .eq. 0) then
               kownt(2,nb) = -nbr
               do 1000 j = 1, nbr
                  if (jorder(j) .eq. nb) kownt(2,nb) = nbr
 1000          continue
               j1 = 1
               j2 = nbr
            else
               j1 = nbr + 1
               j2 = nbr + 1
               jorder(nbr+1) = nb
               kownt(2,nb) = 1
            endif
            do 1020 j = j1, j2
               k1 = jorder(j)
c
c              add branch nb <-- k1
c
               if (next2 .eq. 0) then
                  write (errbuf(1), 150) nsize, MXORDR
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  kerrsw = 1
                  go to 1500
               endif
               kolum2(next2) = k1
               lastlx = next2
               next2 = kordr2(next2)
               l1 = loc1(k1)
               last = 0
c
c              add branch k1 --> nb
c
 1010          if (l1 .gt. 0) then
                  k2 = kolum1(l1)
                  if (k2 .lt. nb) then
                     last = l1
                     l1 = kordr1(l1)
                     go to 1010
                  else if (k2 .eq. nb) then
                     go to 1020
                  endif
               endif
               if (next1 .eq. 0) then
                  write (errbuf(1), 150) nsize, MXORDR
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
                  kerrsw = 1
                  go to 1500
               endif
               if (last .gt. 0) then
                  kordr1(last) = next1
               else
                  loc1(k1) = next1
               endif
               kolum1(next1) = nb
               lx = kordr1(next1)
               kordr1(next1) = l1
               next1 = lx
 1020       continue
            kordr2(lastlx) = 0
 1030    continue
         if (kase1(40) .ne. 0 .and. numagc .gt. 0) then
C            call rdump
C            call symchk
             call cktopol2 (kerr)
             if (kerr .eq. 1) kerrsw = 1
         endif
 1040    continue
      endif
 1320 call number
      do k = 1, ntot
         m = inp2opt(k)
         opt2inp(m) = k
      enddo
c
c     reorder slack nodes to top
c
      do 1350 i = 1,nbslck
         do j = 1,ntot
            if (opt2inp(j) .eq. nslkxx(1,i)) then
               jorder(i) = j
               go to 1350
            endif
         enddo
         j = nslkxx(1,i)
         write (errbuf(1), 1330) bus(j), base(j)
 1330    format(' Cannot find designated slack bus ', a8, f6.1)
         call prterx ('W', 1)
         kerrsw = 1
         go to 1500
 1350 continue
c
c     Reorder each slack node beginning with the lowest ordered node
c
      do 1410 kt = 1,nbslck
         imin = nbslck + 1
         jorder(imin) = 20000
         do k = 1,nbslck
            if (jorder(k) .lt. jorder(imin)) then
               imin = k
            endif
         enddo
         if (imin .gt. nbslck) then
            kerrsw = 1
            go to 1500
         endif
         kslloc = jorder(imin)
         jorder(imin) = 20000
         imove = kslloc - kt
         if (imove .lt. 0) then
            kerrsw = 1
            go to 1500
         else if (imove .gt. 0) then
            do i = 1,imove
               opt2inp(kslloc-i+1) = opt2inp(kslloc-i)
            enddo
            opt2inp(kt) = nslkxx(1,imin)
         endif
         nslkxx(2,imin) = kslloc
 1410 continue
      if (kase1(40) .ne. 0) then
         do i = 1, nbslck
            kslack = nslkxx(1,i)
            write (dbug, 1430) i, bus(kslack), base(kslack),
     &         nslkxx(2,i), (slkxx(j,i),j=3,4)
 1430       format(' System slack buses ',i3,2x,a8,f6.1,i5,2f8.3)
         enddo
      endif
      do k = 1, ntot
         m = opt2inp(k)
         inp2opt(m) = k
      enddo

      if (kase1(36)+kase1(37)+kase1(38)+kase1(39)+kase1(40) .ne. 0) then
         write (dbug,1460) chase1(1), ntot, kbrknt
 1460    format (' Debug of case ',a10,4x,i4,' bus ',i5,
     1           ' equivalent branch problem')
         write (dbug,1470)
 1470    format ('0  No  ext bus    base     inp2opt       opt2inp',
     1           '    bus        base')
         do 1490 k = 1, ntot
         new = opt2inp(k)
         write (dbug,1480) k, bus(k), base(k), inp2opt(k), new,
     &                     bus(new), base(new)
 1480    format (1x ,i4,2x,a8,1x,f6.1,5x,i4,11x,i4,2x,a8,1x,f6.1)
 1490    continue
      endif
 1500 if (kerrsw .ne. 0) then
         reordr = 1
      endif

      return
      end
