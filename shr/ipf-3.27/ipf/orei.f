C    @(#)orei.f	20.4 2/13/96
      integer function orei()

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/vltplr.inc'

      common /time1/time(10), idebug

      common /scratch/numfree, free(2, MAXBUS)
      integer free

      common /is_batch / is_batch

      dimension anet(4, 200), net(2, 200), kolum(MAXBUS), bsname(200), 
     &          bsbase(200), mtrx(MAXBUS)
      character reinam*8, bsname*8, bus_name*8
      integer add_bus
      logical finished, found
      external komp_ykm, swap_ykm

      orei = 0	! Set default return state success

C     IKK assignments: argument  -  description

c      1     0 -> bus is eliminated
c            1 -> bus is retained
c      2          not used
c      3     n -> rei subsystem
c      4     0 -> retained specifically
c            1 -> retained optimally
c      5     0 -> not a REI component
c            1 -> is a REI component

C     DETERMINE SUBNETWORK NUMBER OF ELIMINATED SYSTEM

      do k = 1, ntot
        ikk(4, k) = ikk(3, k)
        ikk(3, k) = 0
        ikk(5, k) = 0

C       INTERPRETATION OF KOLUM:
C       VALUE  -   INTERPRETATION

C          N <1000  PREVIOUSLY ASSIGNED TO REI SYSTEM, WHICH WAS LATER
C                   NULLIFIED BECAUSE OF LACK OF GENERATOR CANDIDATES
C         -1        INELIGIBLE REI COMPONENT (IN RETAINED SYSTEM)
C          0        ELGIBLE REI COMPONENT (IN ELIMINATED SYSTEM)
C          N        ASSIGNED TO REI SYSTEM N

        if (ikk(1, k) .eq. 1) then
          kolum(k) =  - 1
        else
          kolum(k) = 0
        endif
      enddo

C     EXPAND OUTWARDS IN ELIMINATED NETWORK FROM A KERNEL NODE

      lstot = 0
      knt = 0
      ngmax = 0
      if (kase1(22) .ne. 0) then
        do while (.true.)
          if (knt .ne. 0) then
            net(1, knt) = klast
            net(2, knt) = ngen
            lstot = lstot + ngen
            ngmax = max0(ngmax, ngen)
            ksw = 0
            if (ngen .le. 0) then
              ksw = 1

C             NO REI CANDIDATE WAS FOUND IN THE LAST CLUSTER.  NULLIFY THAT
C             CLUSTER ID.

              if (knt .ne. 0) then
                do k = 1, ntot
                  if (kolum(k) .eq. knt) kolum(k) =  - 1000 - knt
                enddo
              endif
              goto 100
            endif
          endif

          ngen = 0
          knt = knt + 1

  100     continue
          do k = 1, ntot
            if (kolum(k) .eq. 0) goto 110
          enddo
          goto 130

  110     if (knt .gt. 200) goto 180

          klast = 1
          mtrx(klast) = k
          kolum(k) = knt
          knext = 0

          do while (.true.)
            knext = knext + 1
            if (knext .gt. klast) goto 120
            k = mtrx(knext)
            ntyp = ntypu(k)
            if (.not. (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 5 
     &       .or. ntyp .eq. 6 .or. ntyp .eq. 10 .or. ntyp .eq. 12)) 
     &       then
              pgen = pnetu(k) + ploadu(k)
              qgen = qnetu(k) + qloadu(k)
              if (sqrt(pgen**2+qgen**2) .ge. 1.000) then
                ngen = ngen + 1
                ikk(5, k) = 1
              endif
            endif

            do l = km(k), km(k) - 1 + kmlen(k)
              m = ikmu(l)
              if (kolum(m) .eq. 0) then

                klast = klast + 1
                mtrx(klast) = m
                kolum(m) = knt
              endif
            enddo
          enddo
  120     continue
        enddo

  130   if (ksw .eq. 0) knt = knt - 1
        do k = 1, ntot
          i4 = kolum(k)
          if (i4 .ge. 0) ikk(3, k) = i4
        enddo

C       CONSOLIDATE SOLITARY GENERATOR CLUSTERS

        do i = 1, knt
          if (net(2, i) .eq. 1) then
            it = 0
            kt = 0
            lt = 0

C           FIND THE BORDER NODE OF CLUSTER "I"

            do j = 1, ntot
              if (ikk(3, j) .eq. i) then
                if (ikk(1, j) .ne. 1) then
                  if (ikk(5, j) .eq. 1) kt = j
                  if (it .le. 0) then
                    do l = km(j), km(j) - 1 + kmlen(j)
                      mt = ikmu(l)
                      if (ikk(1, mt) .ne. 0) then

c                       Node MT is a retained envelope node to cluster I.
c                       examine other adjoint nodes to MT for possible 
c                       adjoint clusters.

                        ksw = 1
                        do m = km(mt), km(mt) - 1 + kmlen(mt)
                          nt = ikmu(m)
                          if (ikk(1, nt) .ne. 1) then
                            i4 = ikk(3, nt)
                            if (i4 .ne. 0) then
                              if (i4 .ne. i) then
                                if (net(2, i4) .gt. 0) goto 140
                              endif
                            endif
                          endif
                        enddo
                      endif
                    enddo
                    goto 150
  140               it = i4
                    lt = nt
                  endif
  150             if (it .gt. 0 .and. kt .gt. 0) goto 160
                endif
              endif
            enddo
            goto 170

c           Transfer solitary generator KT from cluster I into cluster 
c           IT

  160       net(1, i) = net(1, i) - 1
            net(2, i) = net(2, i) - 1
            net(1, it) = net(1, it) + 1
            net(2, it) = net(2, it) + 1
            ikk(3, kt) = it
            if (idebug .ne. 0) write (dbug, 10000) intbus(kt), intbas
     &       (kt), i, it
10000       format (' SOLITARY GENERATOR ', a8, f7.1, 
     &       ' TRANFERED FROM CLUSTER ', i3, ' INTO CLUSTER ', i3)
          endif
  170     continue
        enddo

C       Generate unique upper case rei name EQ[A-Z]

        finished = .false.
        i = ichar('A')
        do while (i .le. ichar('Z') .and. .not. finished)
          reinam = 'EQ' // char(i) // ' '
          i1 = 1
          i2 = ntot_alf
          found = .false.
          do while (i1 .le. i2 .and. .not. found)
            ix = (i1+i2)/2
            iy = alf2inp(ix)
            compare = kompr(bus(iy)(1:4), reinam(1:4), junk)
            if (compare .lt. 0) then
              i1 = ix + 1
            elseif (compare .gt. 0) then
              i2 = ix - 1
            else
              found = .true.
            endif
          enddo
          if (found) then
            i = i + 1
          else
            finished = .true.
          endif
        enddo
        if (finished) then

          do i = 1, knt
            write (bsname(i), 10010) reinam, i
10010       format (a5, i3)
            bsbase(i) = 100.0
          enddo
          goto 190

        else

          write (errbuf(1)(1:120), 10020)
10020     format (' Cannot generate a unique REI name EQ[A-Z]')
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
          kerrsw = 1
          goto 290

        endif

  180   write (errbuf(1)(1:120), 10030)
10030   format (' MORE THAN 200 ELIMINATED SUBNETWORKS IN REI SYSTEM')
        call prterx('W', 1)
        knt = 0
      endif

  190 continue
      if (knt .ne. 0) then
        if (idebug .ne. 0) then
          write (dbug, 10040)
10040     format ('0', 9x, ' SUB NETWORK POPULATIONS ')
          do k = 1, knt, 4
            kend = min0(k+3, knt)
            write (dbug, 10050) (i, net(1, i), net(2, i), i = k, kend)
10050       format (4(i8, 2i5))
          enddo
        endif

        if (ngmax .ge. 1000) then
          write (outbuf, 10060) outbuf
10060     format (' SOME CLUSTERS HAVE MORE THAN 1000 REI CANDIDATES', 
     &     ' AND WILL BE SPLIT INTO SMALLER CLUSTERS. ')
          call prtout(1)

  200     continue
          do i = 1, knt
            if (net(2, i) .gt. 1000) goto 210
          enddo
          goto 240

  210     do while (.true.)
            do j = 1, knt
              if (net(2, j) .eq. 0) goto 220
            enddo
            knt = knt + 1
            j = knt
            if (knt .gt. 200) goto 250

  220       net(1, j) = 0
            net(2, j) = 0
            do kt = 1, ntot
              if (ikk(3, kt) .eq. i) then
                if (ikk(5, kt) .eq. 1) then
                  if (net(2, j) .ge. 1000) goto 230
                  net(1, j) = net(1, j) + 1
                  net(2, j) = net(2, j) + 1
                  net(1, i) = net(1, i) - 1
                  net(2, i) = net(2, i) - 1
                  ikk(3, kt) = j
                  if (idebug .ne. 0) then
                    write (dbug, 10070) intbus(kt), intbas(kt), net(2, 
     &               i), net(2, j)
10070               format (' OVERFLOW CANDIDATE ', a8, f7.1, 
     &               ' TRANSFERRED ', 'FROM CLUSTER', i3, 
     &               ' INTO CLUSTER ', i3, ' -- POPULATIONS', 2i4)
                  endif
                  if (net(2, i) .le. 1000) goto 200
                endif
              endif
            enddo
            goto 200
  230       continue
          enddo

  240     continue
          goto 260
  250     write (errbuf(1)(1:120), 10080)
10080     format (' ERROR -- MORE THAN 200 ELIMINATED SUBNETWORKS')
          call prterx('W', 1)
          knt = 0
          goto 280
        endif

  260   continue

C       Initialize bus and branch data for REI nodes, added in the
C       following order:

C       TERRA<n> (temporary, eliminated)
C       ...
C       EQUIV<n> Equivalent generator
C       ...

        i1 = yptr + 1	        ! Store branches for TERRA nodes
        i2 = i1 + lstot + knt   ! Store branches for EQA G and EQA L
C                               ! nodes
        yptr = i2 + knt  	! Store linked branches from
C                               ! REI-reduced generators/loads to
C                               ! TERRA nodes
        i3 = ntot + 1

C       INITIALIZE "TERRA" NODES

        do i = 1, knt
          j = min0(1, net(2, i))
          k = ntot + i
          alf2inp(k) = k
          inp2alf(k) = k
          inp2opt(k) = k
          opt2inp(k) = k
          km(k) = i1
          kmlen(k) = min0(1000, net(2, i)+1)
          pnetu(k) = 0.0
          qnetu(k) = 0.0
          ploadu(k) = 0.0
          qloadu(k) = 0.0
          vlimn(k) = 0.0
          vlimx(k) = 0.0
          inetr(k) = 0.0
          ineti(k) = 0.0
          ntypu(k) = 0
          nspar(k) = 0
          gkku(k) = 0.0
          bkku(k) = 0.0
          do l = km(k), km(k) - 1 + kmlen(k)
            ikmu(l) = 0
            gkmu(l) = 0.0
            bkmu(l) = 0.0
          enddo
          write (bus_name, 10090) i
10090     format ('TERRA', i3)
          if (j .eq. 0) then
            write (bus_name, 10191) i
10191       format ('~~~T', i4)
            kmlen(k) = 0
            km(k) = 0
          endif
          bus_base = 100.0
          ix = add_bus(bus_name, bus_base, k)
          if (ix .ne. k) then
            write (errbuf(1), 10192) bus_name, bus_base, ix
10192       format (' Error adding REI bus ', a8, f6.1, 1x, i5, 
     &       ' to system ')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw = 1
            goto 290
          endif
          intbus(k) = bus(k)
          intbas(k) = base(k)
          e(k) = 0.0
          f(k) = 0.0
          ikk(1, k) = 0
          ikk(2, k) = 0
          ikk(3, k) = i
          ikk(4, k) = 0
          ikk(5, k) = 0
          i1 = i1 + kmlen(k)
          net(1, i) = 0

C         INITIALIZE "EQUIV" NODES

          k = ntot + knt + i
          km(k) = i2
          kmlen(k) = 1
          inp2opt(k) = k
          opt2inp(k) = k
          pnetu(k) = 0.0
          qnetu(k) = 0.0
          ploadu(k) = 0.0
          qloadu(k) = 0.0
          vlimn(k) = 0.0
          vlimx(k) = 0.0
          inetr(k) = 0.0
          ineti(k) = 0.0
          ntypu(k) = 0
          nspar(k) = 0
          gkku(k) = 0.0
          bkku(k) = 0.0
          do l = km(k), km(k) - 1 + kmlen(k)
            ikmu(l) = 0
            gkmu(l) = 0.0
            bkmu(l) = 0.0
          enddo
          bus_name = bsname(i)
          bus_base = bsbase(i)
          if (j .eq. 0) then
            write (bus_name, 10194) i
10194       format ('~~~G', i4)
            kmlen(k) = 0
            km(k) = 0
          endif
          ix = add_bus(bus_name, bus_base, k)
          if (ix .ne. k) then
            write (errbuf(1), 10195) bus_name, bus_base, ix
10195       format (' Error adding REI bus ', a8, f6.1, 1x, i5, 
     &       ' to system ')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw = 1
            goto 290
          endif
          intbus(k) = bus(k)
          intbas(k) = base(k)
          intbas(k) = 100.0
          bus(k) = intbus(k)
          base(k) = intbas(k)
          e(k) = 0.0
          f(k) = 0.0
          ikk(1, k) = j
          ikk(2, k) = 0
          ikk(3, k) = i
          ikk(4, k) = 0
          ikk(5, k) = 0
          i2 = i2 + kmlen(k)
        enddo

c       Add branches (k,m) from all terra nodes (k) to generator 
c       nodes (m)

        do i = 1, knt
          do j = 1, 4
            anet(j, i) = 0.0
          enddo
        enddo

        numfree = 0
        do k = 1, ntot
          i3 = ikk(3, k)
          if (i3 .ne. 0) then

            if (ikk(1, k) .ne. 1) then
              if (ikk(5, k) .ne. 0) then
                pgen = pnetu(k) + ploadu(k)
                qgen = qnetu(k) + qloadu(k)
                ek = e(k)
                fk = f(k)
                vk = ek**2 + fk**2
                aik = (pgen*ek+qgen*fk)/vk
                bik = (pgen*fk-qgen*ek)/vk
                anet(1, i3) = anet(1, i3) + aik
                anet(2, i3) = anet(2, i3) + bik
                m = i3 + ntot
                yred(1, 1) = m
                yred(2, 1) = pgen/vk
                yred(3, 1) =  - qgen/vk
                gkku(k) = gkku(k) - yred(2, 1)
                bkku(k) = bkku(k) - yred(3, 1)
                ysq = yred(2, 1)**2 + yred(3, 1)**2
                aim = aik**2 + bik**2
                ploss =  - aim*yred(2, 1)/ysq
                qloss = aim*yred(3, 1)/ysq
                anet(3, i3) = anet(3, i3) + ploss
                anet(4, i3) = anet(4, i3) + qloss

C               Sort branches (negative items first) and insert branch 
c               (k,m) from existing generator node k to terra node m.

                lfo = km(k)
                lso = km(k) - 1 + kmlen(k)
                call qiksrt(lfo, lso, komp_ykm, swap_ykm)
                pnetu(k) =  - ploadu(k)
                qnetu(k) =  - qloadu(k)

                numfree = numfree + 1
                free(1, numfree) = lfo
                free(2, numfree) = lso - lfo + 1

                lf = 0
                do while (i .lt. numfree .and. lf .eq. 0)
                  if (free(2, i) .gt. 0 .and. 
     &                free(2, i) .ge. kmlen(k)+1) then
                    lf = free(1, i)
                    ls = lf + kmlen(k)
                    free(1, i) = 0
                    free(2, i) = 0
                  else
                    i = i + 1
                  endif
                enddo
                if (lf .eq. 0) then
                  if (yptr+kmlen(k)+1 .ge. MAXYE) then
                    write (errbuf(1), 10100) MAXYE
10100               format (' More than ', i5, ' Y-matrix entities')
                    if (is_batch .eq. 0) then
                       call prterx ('E',1)
                    else
                       call prterx ('F',1)
                    endif
                    yptr = 1
                    lf = yptr
                    ls = lf + kmlen(k)
                    kmlen(k) = 0
                  else
                    lf = yptr
                    ls = lf + kmlen(k)
                    yptr = yptr + kmlen(k) + 1
                  endif
                endif

                km(k) = lf
                kmlen(k) = kmlen(k) + 1
c
c               Move Y-matrix row to new location
c
                do l = lfo, lso
                   j = l - lfo
                   ikmu(lf+j) = ikmu(lfo+j)
                   gkmu(lf+j) = gkmu(lfo+j)
                   bkmu(lf+j) = bkmu(lfo+j)
                enddo
c
c               Add branch (k,m) to last colum in Y-matrix row.
c
                ikmu(ls) = yred(1, 1)
                gkmu(ls) = yred(2, 1)
                bkmu(ls) = yred(3, 1)

C               ADD BRANCH (K,M) TO "TERRA" NODES

                l = net(1, i3)
                ls = km(m) + l
                ikmu(ls) = k
                gkmu(ls) = yred(2, 1)
                bkmu(ls) = yred(3, 1)
                gkku(m) = gkku(m) - yred(2, 1)
                bkku(m) = bkku(m) - yred(3, 1)
                net(1, i3) = net(1, i3) + 1
              endif
            endif
          endif
        enddo

C       connect TERRA nodes to EQUIV nodes and vice versa

        do i = 1, knt
          l = net(1, i)
          if (l .ne. 0) then
            k = ntot + i
            m = ntot + knt + i

C           K = "TERRA" NODE, M = "EQUIV" NODE

            aik = anet(1, i)
            bik = anet(2, i)
            ploss = anet(3, i)
            qloss = anet(4, i)

C           COMPUTE "YEO"

            aim = aik**2 + bik**2
            yred(1, 1) = m
            ysq = ploss**2 + qloss**2
            yred(2, 1) = aim*ploss/ysq
            yred(3, 1) =  - aim*qloss/ysq

C           ADD BRANCH (K,M) TO "TERRA" NODES

            ls = km(k) - 1 + kmlen(k)
            ikmu(ls) = m
            gkmu(ls) = yred(2, 1)
            bkmu(ls) = yred(3, 1)
            gkku(k) = gkku(k) - yred(2, 1)
            bkku(k) = bkku(k) - yred(3, 1)

C           ADD BRANCH (M,K) TO "EQUIV" NODES

            ls = km(m)
            ikmu(ls) = k
            gkmu(ls) = yred(2, 1)
            bkmu(ls) = yred(3, 1)
            gkku(m) =  - yred(2, 1)
            bkku(m) =  - yred(3, 1)
            ntypu(m) = 2

C           COMPUTE EQUIVALENT GEN AND VOLTAGES

            aik = anet(1, i)
            bik = anet(2, i)
            ysq = yred(2, 1)**2 + yred(3, 1)**2
            ek =  - (aik*yred(2, 1)+bik*yred(3, 1))/ysq
            fk =  - (bik*yred(2, 1)-aik*yred(3, 1))/ysq
            e(m) = ek
            f(m) = fk
            pnetu(m) = aik*ek + bik*fk
            qnetu(m) = aik*fk - bik*ek
            inetr(m) = 0.0
            ineti(m) = 0.0
            vk = sqrt(ek**2+fk**2)
            vlimn(m) = amin1(vk-0.01, 0.950)
            vlimx(m) = amax1(vk+0.01, 1.052)
            nspar(m) = 0

C           Eliminate small REI generators

            if (dsqrt(pnetu(m)**2+qnetu(m)**2) .lt. 2.00d0) then
              net(2, i) = 0
              ikk(1, m) = 0
            endif
          endif
        enddo

C       PRINT OUT GENERATOR EQUIVALENCING SUMMARY

        if (idebug .gt. 0) then
          write (dbug, 10110)
          write (dbug, 10120)
10110     format (
     &     '1 NETWORK    ELIMINATED              EQUIVALENT             
     & injections      q limits (mvar)  voltage '
     &     )
10120     format (
     &     '             GENERATORS              GENERATORS             
     &MW    MVAR       MAX       MIN    P.U.'
     &     )

          do i = 1, knt
            if (net(2, i) .ne. 0) then
              l = 0
              mt = ntot + knt + i
              aik = (pnetu(mt)+ploadu(mt))*bmva
              bik = (qnetu(mt)+qloadu(mt))*bmva
              do kt = 1, ntot
                if (ikk(3, kt) .eq. i) then
                  if (.not. (ikk(1, kt) .eq. 1 .or. ikk(5, kt) .eq. 0)) 
     &             then
                    l = l + 1
                    if (l .le. 1) then
                      vk = dsqrt(e(mt)**2+f(mt)**2)
                      write (dbug, 10130) i, intbus(kt), intbas(kt), 
     &                 intbus(mt), intbas(mt), aik, bik, vk
10130                 format ('0', i5, 4x, a8, f7.1, 10x, a8, f7.1, 5x, 
     &                 2f8.1, 4x, 16x, f9.3)
                    else
                      write (dbug, 10140) intbus(kt), intbas(kt)
10140                 format (10x, a8, f7.1)
                    endif
                    if (l .eq. net(2, i)) goto 270
                  endif
                endif
              enddo
            endif
  270       continue
          enddo
        endif
        write (outbuf, 10150) lstot, knt
10150   format ('  REI SCHEME HAS REPLACED ', i4, ' GENERATORS WITH ', 
     &   i4, ' EQUIVALENT CLUSTER GENERATORS ')
        call prtout(1)
        ntotx = ntot + 1
        ntot = ntot + 2*knt
        ntot_alf = ntot

        do k = ntotx, ntot
          ek = e(k)
          fk = f(k)
          vmag(k) = sqrt(ek*ek+fk*fk)
          if (vmag(k) .lt. 0.001) then
            vangle(k) = 0
          else
            vangle(k) = atan2(fk, ek)
          endif
        enddo
      endif

  280 continue

C     CHECK Y-MATRIX SYMMETRY

      if (idebug .gt. 0) then
        do kt = 1, ntot
          if (ikk(1, kt) .ne. 0) then
            lf = km(kt)
            ls = km(kt) - 1 + kmlen(kt)
            do l = lf, ls
              mt = ikmu(l)

C             Processing branch (kt-mt), find transpose (mt-kt)

              jf = km(mt)
              js = km(mt) - 1 + kmlen(mt)
              j = jf
              do while (j .le. js .and. ikmu(j) .ne. kt)
                j = j + 1
              enddo
              if (j .le. js .and. ikmu(j) .eq. kt) then
                dg = gkmu(l) - gkmu(j)
                db = bkmu(l) - bkmu(j)
                if (abs(db)+abs(dg) .gt. 1.0e-3) then
                  write (dbug, 10160) kt, mt, dg, db
10160             format (' Electrically asymmetric Y-element located '
     &             , 2i6, 2e12.5)
                endif
              else
                write (dbug, 10170) kt, mt
10170           format (' Topologically asymmetric Y-element located ', 
     &           2i6)
              endif
            enddo
          endif
        enddo
      endif

  290 if (kerrsw .gt. 0) orei = 1

      return
      end
