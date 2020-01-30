C    @(#)rei.f	20.6 11/11/97
      integer function rei(ntoto) 

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qsreic.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red6.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/reic.inc'

      common /scratch/angmx(2, MAXREI), bsname(2, MAXREI), bsbase(2, 
     &       MAXREI), anet(8, MAXREI), ztemp(MAXREI), kolum(MAXBUS), 
     &       net(2, MAXREI), mtrx(MAXBUS)
      character bsname*8

      common /is_batch / is_batch

      integer find_bus, compare, count, status, add_bus, rename_bus
      external find_bus
      logical finished, found
      character reinam*8, chge*6, ztemp*2, zn*2, bus_name*8

      rei = 0	! Set default return status "success"
      kerrsw = 0
      ngensw = kase1(2)
      nlodsw = kase1(4)
      nadmsw = kase1(6)
      idebug = kase1(33)

      ntoto = ntot
      if (jtie .eq. 0 .or. ntotc .eq. 0) then
        do i = 1, ntot
          jarzn(i) = 0
        enddo
      endif

C     Initialize current vector

      do i = 1, ntot
        do j = 1, 6
          ai(j, i) = 0.0
        enddo
      enddo

C     Move constant current constraints back to generation or load

      do kt = 1, ntot
        if (inetr(kt) .ne. 0.0 .or. ineti(kt) .ne. 0.0) then
          vk = dsqrt(e(kt)**2+f(kt)**2)
          pk = inetr(kt)*vk
          qk =  - ineti(kt)*vk
          if (pk .gt. 0.0) then
            ploadu(kt) = ploadu(kt) + pk
            qloadu(kt) = qloadu(kt) + qk
          endif
          pnetu(kt) = pnetu(kt) - pk
          qnetu(kt) = qnetu(kt) - qk
          inetr(kt) = 0.0
          ineti(kt) = 0.0
        endif
      enddo

C     Flag rei-disposed nodes

      iqsrei = 0
      do k = 1, ntot
        kt = inp2opt(k)
        if (ikk(1, kt) .eq. 0 .and. kmlen(kt) .gt. 0) then
          call qsrei(k, kt, ix, ngensw, nlodsw, nadmsw)
          if (ix .ne. 0) ikk(5, kt) = ix
        endif
      enddo

C     Determine subnetwork number of eliminated system

      do k = 1, ntot
        if (ikk(1, k) .eq. 0 .and. kmlen(k) .ne. 0) then
          kolum(k) = 0
        else
          kolum(k) =  - 1
        endif
        ikk(3, k) = 0
      enddo

C     Proceed only if "REI" option enabled

      if (max0(ngensw, nlodsw, nadmsw) .eq. 2) then

C       Expand outwards in eliminated network from a kernel node

        lstot = 0
        ngmax = 0

        knt = 1
        amin = 100.0
        amax =  - 100.0
        ngen = 0

C       Begin REI cluster formulation loop

        finished = .false.
        do while (.not. finished)

C         Search for first kernal node

          do k = 1, ntot
            if (kolum(k) .eq. 0 .and. kmlen(k) .gt. 0) goto 100
          enddo
          finished = .true.
          goto 110

  100     if (knt .gt. MAXREI) goto 160
          knext = 1
          klast = 1
          mtrx(klast) = k
          kolum(k) = knt
          do while (knext .le. klast)
            k = mtrx(knext)
            if (ikk(5, k) .eq. 1) then
              ngen = ngen + 1
              ang = 0.0
              if (pnetu(k) .ne. 0.0 .and. qnetu(k) .ne. 0.0) 
     &           ang = datan2(-qnetu(k), pnetu(k))
              amin = amin1(amin, ang)
              amax = amax1(amax, ang)
            endif

            do l = km(k), km(k) - 1 + kmlen(k)
              m = ikmu(l)
              if (kolum(m) .eq. 0) then
                klast = klast + 1
                if (klast .gt. MAXBUS) goto 170
                mtrx(klast) = m
                kolum(m) = knt
              endif
            enddo
            knext = knext + 1
          enddo

          net(1, knt) = klast
          net(2, knt) = ngen
          angmx(1, knt) = amin
          angmx(2, knt) = amax
          lstot = lstot + ngen
          ngmax = max0(ngmax, ngen)
          ksw = 0
          if (ngen .eq. 0) then
            ksw = 1

C           NO REI candidate was found in the last cluster.
C           nullify that cluster id.

            do k = 1, ntot
              if (kolum(k) .eq. knt) kolum(k) =  - 1000 - knt
            enddo
          else
            ngen = 0
            knt = knt + 1
            amin = 100.0
            amax =  - 100.0
          endif

C         End of REI cluster formulation loop

  110     continue
        enddo

        if (ksw .eq. 0) knt = knt - 1
        do k = 1, ntot
          i = kolum(k)
          if (i .ge. 0) ikk(3, k) = i
        enddo

C       Consolidate solitary generator clusters

        ang = case1(15)
        if (ang .eq. 0.0) ang = 20.0
        angmin = 0.017453*ang
        write (outbuf, 10000) ang
10000   format ('0 MAXIMUM PERMISSIBLE ANGLE DIFFERENCE BETWEEN ', 
     &   'COALESED CLUSTERS: ', f6.1, ' DEGREES.')
        call prtout(1)

        do i = 1, knt
          if (net(2, i) .eq. 1) then
            it = 0
            kt = 0
            lt = 0

C           Find the border node of cluster "i"

            do j = 1, ntot
              if (ikk(3, j) .eq. i) then
                if (ikk(1, j) .ne. 1) then
                  if (ikk(5, j) .eq. 1) kt = j
                  ksw = 0
                  mb = opt2inp(j)
                  if (it .le. 0) then
                    do l = km(j), km(j) - 1 + kmlen(j)
                      mt = ikmu(l)
                      if (ikk(1, mt) .ne. 0) then
                        ksw = 1
                        mb = opt2inp(mt)

C                       Node "MT" is a retained envelope node to cluster "I".
C                       Examine other adjoint nodes to "mt" for possible
C                       adjoint clusters.

                        do m = km(mt), km(mt) - 1 + kmlen(mt)
                          nt = ikmu(m)
                          mb = opt2inp(nt)
                          ksw = 2
                          if (ikk(1, nt) .ne. 1) then
                            i3 = ikk(3, nt)
                            if (i3 .ne. 0) then
                              if (i3 .ne. i) then
                                if (net(2, i3) .gt. 0) goto 120
                              endif
                            endif
                          endif
                        enddo
                      endif
                    enddo
                    goto 130
  120               it = i3
                    lt = nt
                  endif
  130             if (it .gt. 0 .and. kt .gt. 0) goto 140
                endif
              endif
            enddo
            goto 150

C           A candidate for cluster consolidation has been found.
C           However, the solitary generator cannot be more than 20
C           degrees from other generators in the cluster

  140       kb = opt2inp(kt)
            lb = opt2inp(lt)
            jarkb = jarzn(kb)
            jarlb = jarzn(lb)

C           A coalesced REI subsystem cannot integrate subsystems from
C           separate area interchange systems.

            if (jtie .gt. 0 .and. jarkb .eq. jarlb) then
              amin = angmx(1, it)
              amax = angmx(2, it)
              ang = angmx(1, i)
              dang = dim(ang, amax) - dim(amin, ang)
              if (abs(dang) .le. angmin) then

C               Transfer solitary generator "KT" from cluster "I" into
C               cluster "IT

                net(1, i) = net(1, i) - 1
                net(2, i) = net(2, i) - 1
                net(1, it) = net(1, it) + 1
                net(2, it) = net(2, it) + 1
                ikk(3, kt) = it
                kb = opt2inp(kt)
                if (idebug .gt. 0) then
                  write (dbug, 10010) bus(kb), base(kb), i, it
10010             format (' Solitary generator ', a8, f7.1, 
     &             ' tranferred from cluster ', i3, ' into cluster ', 
     &             i3)
                endif
              endif
            endif
          endif
  150     continue
        enddo

C       Generate unique upper case rei name EQ[A-Z]

        finished = .false.
        i = ichar('A')
        do while (i .le. ichar('Z') .and. .not. finished)
          reinam = 'EQ'//char(i)//' '
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
            write (bsname(1, i), 10020) reinam, i
10020       format (a3, ' G', i3)
            bsbase(1, i) = 100.0
            write (bsname(2, i), 10030) reinam, i
10030       format (a3, ' L', i3)
            bsbase(2, i) = 100.0
          enddo
          goto 180
        else
          write (errbuf(1)(1:120), 10040)
10040     format (' Cannot generate a unique REI name EQ[A-Z]')
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
          kerrsw = 1
          goto 360
        endif
  160   write (errbuf(1)(1:120), 10050) MAXREI
10050   format (' MORE THAN ', i4, 
     &   ' ELIMINATED SUBNETWORKS IN REI SYSTEM')
        call prterx('W', 1)
        kerrsw = 1
        goto 360
  170   write (errbuf(1)(1:120), 10060) MAXBUS
10060   format (' More than ', i4, ' buss in rei system')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        kerrsw = 1
        goto 360
      endif

  180 continue
      do while (inptsw .eq. 5)

C       Examine for "coherency clustering"

        write (outbuf, 10070) busdt(1), basedt(1), idat
10070   format ('0 Coherency cluster ', a8, f7.1, ' Population ', i4,
     &          ' has the following compostion:')
        call prtout(1)
        outbuf = ' '
        call prtout(1)
        outbuf = ' '
        do kst = 2, idat - 1, 5
          kend = min0(kst+4, idat)
          i1 = 1
          do i = kst, kend
            nb = find_bus (busdt(i), basedt(i))
            write (outbuf(19*i1-16:), 10090) bus(nb), base(nb), zone(nb)
10090       format (a8, f6.1, 1x, a2)
            i1 = i1 + 1
          enddo
          call prtout(1)
        enddo

        write (outbuf, 10080) ' '
10080   format ('0', 37x, 10('  -----'), a)
        call prtout(1)

        sang = 1.0
        nb = find_bus(busdt(1), basedt(1))
        if (nb .gt. 0) then
          write (errbuf(1), 10100) busdt(1), basedt(1)
10100     format (' Duplicate coherency generator ', a8, f7.1)
          call prterx('W', 1)
          kerrsw = 1
        else
          knt = knt + 1
          if (knt .gt. MAXREI) then
            write (errbuf(1), 10050) MAXREI
            call prterx('W', 1)
            kerrsw = 1
            knt = 1
          else
            bsname(1, knt) = busdt(1)
            bsbase(1, knt) = basedt(1)
            bsname(2, knt) = busdt(1)(1:7)//'*'
            bsbase(2, knt) = basedt(1)
            net(1, knt) = 0
            net(2, knt) = 0
          endif
        endif
        do i = 2, idat
          nb = find_bus(busdt(i), basedt(i))
          if (nb .le. 0) then
            write (errbuf(1), 10110) busdt(i), basedt(i)
10110       format (' Eliminated generator ', a8, f7.1, 
     &       ' is not in system.')
            call prterx('W', 1)
            kerrsw = 1
          else if (ikk(1,inp2opt(nb)) .eq. 1) then
            write (errbuf(1), 10112) busdt(i), basedt(i)
10112       format (' Illegal REI coherency candidate ', a8, f7.1, 
     &         ' is in retained system.')
            call prterx('W', 1)
          else
            kt = inp2opt(nb)
            i3 = ikk(3, kt)
            i5 = ikk(5, kt)
            j1 = 2
            j2 = 2
            j3 = 2
            if (zdata(i)(1:1) .ne. ' ') j1 = rval(zdata(i)(1:1))
            if (zdata(i)(2:2) .ne. ' ') j2 = rval(zdata(i)(2:2))
            if (i3 .gt. 0) then
              net(1, i3) = net(1, i3) - 1
              ikk(3, kt) = 0
              ikk(5, kt) = 0
              if (i5 .gt. 0) then
                net(2, i3) = net(2, i3) - 1
                lstot = lstot - 1
              endif
            endif
            call qsrei(nb, kt, ix, j1, j2, j3)
            if (ix .eq. 0) then
              write (errbuf(1), 10120) busdt(i), basedt(i)
10120         format (' REI coherency candidate ', a8, f7.1, 
     &         ' has passive attributes and is ineligible for inclusion.
     &')
              call prterx('W', 1)
              ikk(5, kt) = 0
            else
              ikk(1, kt) = 0
              ikk(3, kt) = knt
              ikk(5, kt) = 1
              net(1, knt) = net(1, knt) + 1
              net(2, knt) = net(2, knt) + 1
              lstot = lstot + 1
              ngmax = max0(ngmax, net(2, knt))
            endif
          endif
        enddo
        call readcr()
      enddo

      if (kerrsw .ne. 1) then
        if (knt .ne. 0) then

          if (idebug .gt. 0) then
            write (dbug, 10130)
10130       format ('0', 9x, ' SUB NETWORK POPULATIONS ')
            do kst = 1, knt, 4
              kend = min0(kst+3, knt)
              write (dbug, 10140) (i, net(1, i), net(2, i), i = kst, 
     &         kend)
10140         format (4(i8, 2i5))
            enddo
          endif

          if (ngmax .ge. 1000) then
            write (errbuf(1), 10150)
10150       format ('More than 1000 rei candidates ', 
     &       'coalesced into a single cluster equivalent.')
            call prterx('W', 1)

C           Split all excessively populated clusters into smaller clusters

            finished = .false.
            do while (.not. finished)
              do i = 1, knt
                if (net(2, i) .gt. 1000) goto 190
              enddo
              finished = .true.
              goto 220

  190         do while (.true.)
                do j = 1, knt
                  if (net(2, j) .eq. 0) goto 200
                enddo
                knt = knt + 1
                j = knt
                if (knt .gt. MAXREI) goto 230
  200           net(1, j) = 0
                net(2, j) = 0
                do kt = 1, ntot
                  if (ikk(3, kt) .eq. i) then
                    if (ikk(5, kt) .eq. 1) then
                      if (net(2, j) .ge. 1000) goto 210
                      net(1, j) = net(1, j) + 1
                      net(2, j) = net(2, j) + 1
                      net(1, i) = net(1, i) - 1
                      net(2, i) = net(2, i) - 1
                      ikk(3, kt) = j
                      kb = opt2inp(kt)
                      if (idebug .gt. 0) then
                        write (dbug, 10160) bus(kb), base(kb), i, j, 
     &                   net(2, i), net(2, j)
10160                   format (' Overflow candidate ', a8, f7.1, 
     &                   ' transferred from cluster ', i3, 
     &                   ' into cluster ', i3, ' -- populations:', 2i4)
                      endif
                      if (net(2, i) .le. 1000) goto 220
                    endif
                  endif
                enddo
                goto 220
  210           continue
              enddo
  220         continue
            enddo
            goto 240
  230       write (errbuf(1), 10050) MAXREI
            call prterx('W', 1)
            kerrsw = 1
            goto 360
          endif

  240     ntotx = ntot + 3*knt
          if (ntotx .gt. MAXBUS) then
            write (errbuf(1), 10170) ntotx, knt, MAXBUS
10170       format (' Too many buses (', i4, ') forming (', i3, 
     &       ') REI system. Limit (', i4, ')')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw = 1
            goto 360
          else

C           Initialize remaining current array "INET"

            do i = ntot + 1, ntotx
              do j = 1, 5
                ikk(j, i) = 0
              enddo
              do j = 1, 6
                ai(j, i) = 0.0
              enddo
            enddo

C           Initialize bus and branch data for REI nodes, added in the
C           following order:

C           TERRA<n> (temporary, eliminated)
C           ...
C           EQA G<n> Equivalent generator
C           EQA L<n> Equivalent load
C           ...

            i1 = yptr + 1	    ! Store branches for TERRA nodes
            i2 = i1 + lstot + 2*knt ! Store branches for EQA G and EQA L
C                                   ! nodes
            yptr = i2 + 2*knt	    ! Store linked branches from
C                                   ! REI-reduced generators/loads to
C                                   ! TERRA nodes

C           Initialize "TERRA<n>" nodes

            do i = 1, knt
              j = min0(1, net(2, i))
              k = ntot + i
              inp2opt(k) = k
              opt2inp(k) = k
              km(k) = i1
              kmlen(k) = min0(1000, net(2, i)+2)
              pnetu(k) = 0.0
              qnetu(k) = 0.0
              inetr(k) = 0.0
              ineti(k) = 0.0
              ploadu(k) = 0.0
              qloadu(k) = 0.0
              vlimn(k) = 0.0
              vlimx(k) = 0.0
              gkku(k) = 0.0
              bkku(k) = 0.0
              ntypu(k) = 0
              nspar(k) = 0
              do m = km(k), km(k) - 1 + kmlen(k)
                ikmu(m) = 0
                gkmu(m) = 0.0
                bkmu(m) = 0.0
              enddo
              write (bus_name, 10180) i
10180         format ('TERRA', i3)
              bus_base = 100.0
              if (j .eq. 0) then
                write (bus_name, 10190) i
10190           format ('~~~T', i4)
                kmlen(k) = 0
              endif
              ix = add_bus(bus_name, bus_base, k)
              if (ix .ne. k) goto 330
              intbus(k) = bus(k)
              intbas(k) = base(k)
              e(k) = 0
              f(k) = 0
              ikk(1, k) = 0
              ikk(2, k) = 0
              ikk(3, k) = i
              ikk(4, k) = 0
              ikk(5, k) = 0
              i1 = i1 + kmlen(k)
              net(1, i) = 0

C             Initialize "EQA G<n>", "EQA L<n>" nodes

              do jsw = 1, 2
                k = ntot + knt + 2*i + jsw - 2
                inp2opt(k) = k
                opt2inp(k) = k
                km(k) = i2
                kmlen(k) = 1
                pnetu(k) = 0.0
                qnetu(k) = 0.0
                inetr(k) = 0.0
                ineti(k) = 0.0
                ploadu(k) = 0.0
                qloadu(k) = 0.0
                vlimn(k) = 0.0
                vlimx(k) = 0.0
                gkku(k) = 0.0
                bkku(k) = 0.0
                ntypu(k) = 0
                nspar(k) = 0
                do m = km(k), km(k) - 1 + kmlen(k)
                  ikmu(m) = 0
                  gkmu(m) = 0.0
                  bkmu(m) = 0.0
                enddo
                bus_name = bsname(jsw, i)
                bus_base = bsbase(jsw, i)
                if (j .eq. 0) then
                  write (bus_name, 10200) k
10200             format ('~~~G', i4)
                  kmlen(k) = 0
                endif
                ix = add_bus(bus_name, bus_base, k)
                if (ix .ne. k) goto 340
                intbus(k) = bus(k)
                intbas(k) = base(k)
                e(k) = 0
                f(k) = 0
                ikk(1, k) = min0(j, 1)
                ikk(2, k) = 0
                ikk(3, k) = i
                ikk(4, k) = 0
                ikk(5, k) = 0
                i2 = i2 + kmlen(k)
              enddo
            enddo
            do i = 1, knt
              do j = 1, 8
                anet(j, i) = 0.0
              enddo
            enddo

C           Add branches (K,M) from all "TERRA" nodes (K) to generator
C           nodes (M)

            do k = 1, ntot
              if (ikk(5, k) .ne. 0) then
                if (ikk(1, k) .ne. 1) then
                  i3 = ikk(3, k)
                  if (i3 .ne. 0) then
                    pgen = pnetu(k) + ploadu(k)
                    qgen = qnetu(k) + qloadu(k)
                    ek = e(k)
                    fk = f(k)
                    vk = ek**2 + fk**2
                    jqsrei = nspar(k)
                    if (jqsrei .eq. 0) goto 290
                    qmin = qsdta(1, jqsrei)
                    qmax = qsdta(2, jqsrei)
                    gshunt = qsdta(3, jqsrei)
                    bshunt = qsdta(4, jqsrei)
                    badj = qsdta(5, jqsrei)
                    nadmsw = mod(ifix(sngl(qsdta(6, jqsrei))), 10)
                    nlodsw = mod(ifix(sngl(qsdta(6, jqsrei)))/10, 10)
                    ngensw = mod(ifix(sngl(qsdta(6, jqsrei)))/100, 10)

C                   Remove any shunt which is transferred to REI node

                    gkku(k) = gkku(k) - gshunt/vk
                    bkku(k) = bkku(k) + bshunt/vk

C                   Remove any generation which is transferred to REI node

                    if (ngensw .eq. 2) then
                      pgenx = pgen
                      qgenx = qgen
                      pgen = 0
                      qgen = 0
                    else
                      pgenx = 0
                      qgenx = 0
                    endif

C                   Remove any load which is transferred to REI node

                    if (nlodsw .eq. 2) then
                      ploadx = ploadu(k)
                      qloadx = qloadu(k)
                      ploadu(k) = 0.0
                      qloadu(k) = 0.0
                    else
                      ploadx = 0
                      qloadx = 0
                    endif
                    pnetu(k) = pgen - ploadu(k)
                    qnetu(k) = qgen - qloadu(k)
                    c1 = sqrt(pgenx**2+qgenx**2)
                    c2 = sqrt(ploadx**2+qloadx**2)
                    if (c1+c2 .eq. 0.0) then
                      c1 = 1.0
                      c2 = 1.0
                    endif
                    cg = c1/(c1+c2)
                    cl = c2/(c1+c2)
                    pgenx = pgenx - cg*gshunt
                    qgenx = qgenx - cg*bshunt
                    ploadx = ploadx + cl*gshunt
                    qloadx = qloadx + cl*bshunt
                    do i = 1, 6
                      qsdta(i, jqsrei) = 0
                    enddo

C                   Append generator data to equivalent generator node (ii)

                    ii = ntot + knt + 2*i3 - 1
                    jqsrei = nspar(ii)
                    if (jqsrei .eq. 0) then
                      iqsrei = iqsrei + 1
                      if (iqsrei .gt. 2000) goto 300
                      jqsrei = iqsrei
                      nspar(ii) = jqsrei
                      do i = 1, 6
                        qsdta(i, iqsrei) = 0.0d0
                      enddo
                    endif
                    qsdta(1, jqsrei) = qsdta(1, jqsrei) + qmin
                    qsdta(2, jqsrei) = qsdta(2, jqsrei) + qmax
                    qsdta(3, jqsrei) = qsdta(3, jqsrei) + cg*gshunt
                    qsdta(4, jqsrei) = qsdta(4, jqsrei) + cg*bshunt
                    qsdta(5, jqsrei) = qsdta(5, jqsrei) + cg*badj

C                   Append load data to equivalent load node (ii)

                    ii = ii + 1
                    jqsrei = nspar(ii)
                    if (jqsrei .eq. 0) then
                      iqsrei = iqsrei + 1
                      if (iqsrei .gt. 2000) goto 310
                      jqsrei = iqsrei
                      nspar(ii) = jqsrei
                      do i = 1, 6
                        qsdta(i, iqsrei) = 0.0d0
                      enddo
                    endif
                    qsdta(3, jqsrei) = qsdta(3, jqsrei) + cl*gshunt
                    qsdta(4, jqsrei) = qsdta(4, jqsrei) + cl*bshunt
                    qsdta(5, jqsrei) = qsdta(5, jqsrei) + cl*badj
                    anet(1, i3) = anet(1, i3) + (pgenx*ek+qgenx*fk)/vk
                    anet(2, i3) = anet(2, i3) + (pgenx*fk-qgenx*ek)/vk
                    anet(3, i3) = anet(3, i3) + pgenx
                    anet(4, i3) = anet(4, i3) + qgenx
                    anet(5, i3) = anet(5, i3) 
     &                          - (ploadx*ek+qloadx*fk)/vk
                    anet(6, i3) = anet(6, i3) 
     &                          - (ploadx*fk-qloadx*ek)/vk
                    anet(7, i3) = anet(7, i3) - ploadx
                    anet(8, i3) = anet(8, i3) - qloadx

C                   Connect generator node k to TERRA node (m) via 
c                   new branch (k,m)

                    m = i3 + ntot

                    if (yptr .ge. MAXYE) goto 320

                    yptr = yptr + 1
                    ikmu(yptr) = m
                    gkmu(yptr) = (pgenx-ploadx)/vk
                    bkmu(yptr) =  - (qgenx-qloadx)/vk

                    gkku(k) = gkku(k) - gkmu(yptr)
                    bkku(k) = bkku(k) - bkmu(yptr)

C                   Link new branch (k,m) to last branch in contiguous 
c                   set iykm

                    ls = kmlen(k)
                    ln = km(k) - 1 + ls
                    ikmu(ln) = ipack_2 (yptr, ikmu(ln))

C                   Connect TERRA node (m) to generator node k via new 
c                   branch (m,k)

                    l = net(1, i3) + 1
                    ln = km(m) - 1
                    ikmu(l+ln) = k
                    gkmu(l+ln) = gkmu(yptr)
                    bkmu(l+ln) = bkmu(yptr)
                    gkku(m) = gkku(m) - gkmu(yptr)
                    bkku(m) = bkku(m) - bkmu(yptr)
                    net(1, i3) = net(1, i3) + 1
                  endif
                endif
              endif
            enddo

C           Consolidate generator and load nodes if voltages are 
c           sufficiently close.

            vtol = case1(13)

            if (vtol .eq. 0.0) vtol = 0.250
            write (outbuf, 10210) vtol
10210       format (
     &       '0 REI equivalent generator and load nodes minimum voltage 
     &separation for distinct entities '
     &       , f6.3)
            call prtout(1)

            do i = 1, knt
              if (net(1, i) .ne. 0) then
                aik = anet(1, i)**2 + anet(2, i)**2
                bik = anet(5, i)**2 + anet(6, i)**2
                if (aik .ne. 0.0 .and. bik .ne. 0.0) then
                  ek1 = (anet(3, i)*anet(1, i)
     &                - anet(4, i)*anet(2, i))/aik
                  fk1 = (anet(3, i)*anet(2, i)
     &                + anet(4, i)*anet(1, i))/aik
                  ek2 = (anet(7, i)*anet(5, i)
     &                - anet(8, i)*anet(6, i))/bik
                  fk2 = (anet(7, i)*anet(6, i)
     &                + anet(8, i)*anet(5, i))/bik
                  if (abs(ek1-ek2)+abs(fk1-fk2) .le. vtol) then
                    do j = 1, 4
                      anet(j, i) = anet(j, i) + anet(j+4, i)
                      anet(j+4, i) = 0
                    enddo
                    ii = ntot + knt + 2*i - 1
                    j1 = nspar(ii)
                    j2 = nspar(ii+1)
                    do j = 1, 5
                      qsdta(j, j1) = qsdta(j, j1) + qsdta(j, j2)
                      qsdta(j, j2) = 0.0d0
                    enddo
                  endif
                else 
c
c                  A passive REI module slipped through.  Add a small
c                  residual value.
c
                   if (aik .eq. 0.0) then
                      anet(1,i) = 0.1
                      anet(3,i) = 0.1
                   endif
                   if (bik .eq. 0.0) then
                      anet(5,i) = 0.1
                      anet(7,i) = 0.1
                   endif
                endif
              endif
            enddo

C           Connect "TERRA" nodes with "EQA G", EQA L" nodes

            do i = 1, knt
              l = net(1, i)
              if (l .ne. 0) then
                k = ntot + i

C               K = "TERRA" node, 
c               M = "EQA G" (jsw = 1), 
c                   "EQA L" (jsw = 5) node

                do jsw = 1, 5, 4
                  if (jsw .eq. 1) then
                    m = ntot + knt + 2*i - 1
                    ii = 2*i - 1
                    lp = km(k) - 2 + kmlen(k)
                  else
                    m = m + 1
                    ii = ii + 1
                    lp = lp + 1
                  endif
                  do j = 1, 16
                    kbsdta(j, m) = 0
                  enddo
                  aik = anet(jsw, i)
                  bik = anet(jsw+1, i)
                  ploss = anet(jsw+2, i)
                  qloss = anet(jsw+3, i)

C                 compute "YEO"

                  aim = aik**2 + bik**2
                  yred(1, 1) = m
                  ysq = ploss**2 + qloss**2
                  if (ysq .eq. 0.0) goto 250

C                 Add branch (K,M) to "TERRA" nodes

                  ikmu(lp) = m
                  gkmu(lp) =  - aim*ploss/ysq
                  bkmu(lp) = aim*qloss/ysq
                  gkku(k) = gkku(k) - gkmu(lp)
                  bkku(k) = bkku(k) - bkmu(lp)

C                 Add branch (M,K) to "EQA G", "EQA L" nodes

                  lq = km(m) - 1 + kmlen(m)
                  ikmu(lq) = k
                  gkmu(lq) = gkmu(lp)
                  bkmu(lq) = bkmu(lp)
                  gkku(m) = gkku(m) - gkmu(lp)
                  bkku(m) = bkku(m) - bkmu(lp)

C                 Compute equivalent gen and voltages

                  e(m) = (ploss*aik-qloss*bik)/aim
                  f(m) = (ploss*bik+qloss*aik)/aim
                  ek = e(m)
                  fk = f(m)
                  vk = ek**2 + fk**2
                  jj = ntot + knt + ii
                  j1 = nspar(jj)
                  pnetu(m) = ploss + qsdta(3, j1)
                  qnetu(m) = qloss + qsdta(4, j1)

C                 Convert all fixed shunt into equivalent shunt current

                  bkku(m) = bkku(m) - qsdta(5, j1)/vk
                  aik = qsdta(3, j1)
                  bik = qsdta(4, j1) - qsdta(5, j1)
                  qsdta(5, j1) =  - (aik*ek+bik*fk)/vk
                  qsdta(6, j1) =  - (aik*fk-bik*ek)/vk
                  if (jsw .eq. 5) then
                    ploadu(m) =  - anet(jsw+2, i) - qsdta(3, j1)
                    qloadu(m) =  - anet(jsw+3, i) - qsdta(4, j1)
                  endif
                  pgen = pnetu(m) + ploadu(m)
                  qgen = qnetu(m) + qloadu(m)

C                 Determine appropriate bus type: "BC", "BQ", or "BE".

                  jj = jj + 1
                  if (jsw .ne. 1) jj = jj - 2
                  j2 = nspar(jj)
                  if (dmin1(qsdta(1, j1), qsdta(1, j2)) .le. -1.0d5 
     &             .and. dmax1(qsdta(2, j1), qsdta(2, j2)) .ge. 1.0d5) 
     &             then
                    jtyp = 2
                    qsdta(1, j1) = dmod(qsdta(1, j1), 1.0d5)
                    qsdta(2, j1) = dmod(qsdta(2, j1), 1.0d5)
                  elseif (qsdta(1, j1) .ge. qsdta(2, j1) .and. qsdta(5, 
     &             j1) .eq. 0.0) then
                    jtyp = 2
                    qsdta(1, j1) = 0
                    qsdta(2, j1) = qgen*bmva
                  else
                    jtyp = 2
                    qsdta(1, j1) = dmin1(qsdta(1, j1), dble(qgen*bmva))
                    qsdta(2, j1) = dmax1(qsdta(2, j1), dble(qgen*bmva))
                  endif

                  kbsdta(1, m) = jtyp
                  owner(m) = '***'
                  busdta(3, m) = ploadu(m)*bmva
                  busdta(4, m) = qloadu(m)*bmva
                  busdta(6, m) =  - qsdta(5, j1)*bmva/vk
                  busdta(8, m) = pgen*bmva
                  busdta(9, m) = qsdta(2, j1)
                  busdta(10, m) = qsdta(1, j1)
                  busdta(11, m) = sqrt(ek**2+fk**2)
                  if (.not. (jtyp .eq. 15 .or. jtyp .eq. 16)) then
                    busdta(12, m) = busdta(11, m)
                  elseif (busdta(11, m) .eq. 0.0) then
                    busdta(12, m) = 0.0
                  else
                    busdta(12, m) = atan2(fk, ek)
                  endif
                enddo
                goto 260

C               Delete passive rei node.

  250           ikk(1, m) = 0
                ikk(2, m) = 0
                ikk(3, m) = i
                ikk(4, m) = 0
                ikk(5, m) = 0
                nt = opt2inp(m)
                bus_name = '~~~'//bus(nt)(5:8)
                bus_base = base(nt)
                status = rename_bus(nt, bus_name, bus_base)
                intbus(nt) = bus(nt)
                km(nt) = 0

C               Reduce branch count of "TERRA" node

                kmlen(k) = kmlen(k) - 1
                if (kmlen(k) .eq. 0) km(k) = 0
              endif
  260         continue
            enddo

C           Print out generator equivalencing summary

            call forbtm()
            call fortop()
            write (outbuf, 10220)
10220       format (
     &       'NETWORK ELIMINATE         EQUIVALENT          INJECTIONS  
     &        q limits (mvar)   shunt admittance     voltage'
     &       )
            call prtout(1)
            write (outbuf, 10230)
10230       format (
     &       '        GENERATORS        GENERATORS          MW    MVAR  
     &          max     min      mw      mvar      p.u.  angle'
     &       )
            call prtout(1)

            do i = 1, knt
              if (net(2, i) .ne. 0) then
                count = 0
                mt = ntot + knt + 2*i - 1
                if (bus(mt)(1:3) .ne. '~~~') then
                  aik = pnetu(mt)*bmva
                  bik = qnetu(mt)*bmva
                  j1 = nspar(mt)
                  vk1 = e(mt)**2 + f(mt)**2
                  ak1 = 57.2957795*datan2(f(mt), e(mt))
                  g1 = (gkku(mt)+gkmu(mt))*vk1*bmva
                  b1 =  - (bkmu(mt)+bkmu(mt))*vk1*bmva
                  qmn1 = qsdta(1, j1)
                  qmx1 = qsdta(2, j1)
                  do j = 1, 5
                    qsdta(j, j1) = 0.0
                  enddo
                endif
                nt = mt + 1
                if (bus(nt)(1:3) .ne. '~~~') then
                  cik = pnetu(nt)*bmva
                  dik = qnetu(nt)*bmva
                  j2 = nspar(nt)
                  vk2 = e(nt)**2 + f(nt)**2
                  ak2 = 57.2957795*datan2(f(nt), e(nt))
                  g2 = (gkku(nt)+gkmu(km(nt)))*vk2*bmva
                  b2 =  - (bkku(nt)+bkmu(km(nt)))*vk2*bmva
                  qmn2 = qsdta(1, j2)
                  qmx2 = qsdta(2, j2)
                  do j = 1, 5
                    qsdta(j, j2) = 0.0
                  enddo
                endif
                ksw = 0	! First line is EQA G
                if (bus(mt)(1:3) .eq. '~~~') ksw = 1

c               First line is EQA L

                do kt = 1, ntot
                  if (ikk(3, kt) .eq. i) then
                    if (.not. (ikk(1, kt) .eq. 1 .or. 
     &                         ikk(5, kt) .eq. 0)) then
                      k = opt2inp(kt)
                      count = count + 1
                      if (ksw .eq. 0) then
                        chge = '      '
                        if (count .eq. 1) write (chge, 10250) i
                        vk = sqrt(vk1)
                        write (outbuf, 10240) i, bus(k), base(k), bus
     &                   (mt), base(mt), aik, bik, qmx1, qmn1, g1, b1, 
     &                   vk, ak1
10240                   format ('0', i5, 4x, a8, f7.1, 4x, a8, f7.1, 
     &                   1x, 2f8.1, 4x, 2f8.1, 2x, 2f8.1, f11.3, f6.1)
                        call prtout(1)
                        ksw = 1
                        if (bus(nt)(1:3) .eq. '~~~') ksw = 2
                      elseif (ksw .eq. 1) then
                        chge = '      '
                        if (count .eq. 1) write (chge, 10250) i
10250                   format ('0', i5)
                        vk = sqrt(vk2)
                        write (outbuf, 10260) chge, bus(k), base(k), 
     &                   bus(nt), base(nt), cik, dik, qmx2, qmn2, g2, 
     &                   b2, vk, ak2
10260                   format (a6, 4x, a8, f7.1, 4x, a8, f7.1, 1x, 
     &                   2f8.1, 4x, 2f8.1, 2x, 2f8.1, f11.3, f6.1)
                        call prtout(1)
                        ksw = 2
                      else
                        write (outbuf, 10270) bus(k), base(k)
10270                   format (10x, a8, f7.1)
                        call prtout(1)
                      endif
                      if (count .eq. net(2, i)) goto 270
                    endif
                  endif
                enddo

C               Print out EQA L if missed in above loop (only one 
c               generator)

  270           if (ksw .eq. 1) then
                  if (bus(nt)(1:3) .ne. '~~~') then
                    vk = sqrt(vk2)
                    write (outbuf, 10280) bus(nt), base(nt), cik, dik, 
     &               qmx2, qmn2, g2, b2, vk, ak2
10280               format (29x, a8, f7.1, 1x, 2f8.1, 4x, 2f8.1, 2x, 
     &               2f8.1, f11.3, f6.1)
                    call prtout(1)
                  endif
                endif

C               Debug dump of "TERRA" subsystem

                if (idebug .gt. 0) then
                  aik = 0
                  bik = 0
                  kt = ntot + i
                  ls = km(kt)
                  lf = ls - 1 + kmlen(kt)
                  do l = ls, lf
                    mt = ikmu(l)
                    gkm = gkmu(l)
                    bkm = bkmu(l)
                    em = e(mt)
                    fm = f(mt)
                    aik = aik + em*gkm - fm*bkm
                    bik = bik + em*bkm + fm*gkm
                    m = opt2inp(mt)
                    write (dbug, 10290) l, bus(m), base(m), gkm, bkm, 
     &               em, fm
10290               format (i6, 4x, a8, f7.1, 4e10.3)
                  enddo

                  k = opt2inp(kt)
                  write (dbug, 10300) bus(k), base(k), net(1, i), net
     &             (2, i), aik, bik, gkku(kt), bkku(kt), e(kt), f(kt), 
     &             km(kt), kmlen(kt)
10300             format (10x, a8, f7.1, i4, i10, 6x, 6e10.3, 2i6)
                endif
              endif
            enddo

C           Extend "ZONE" ARRAY to include REI nodes

            do i = 1, knt
              ztemp(i) = '$$'
              zone(i+ntot) = '$$'
              zone(2*i-1+ntot) = '$$'
              zone(2*i+ntot) = '$$'
            enddo
            do ksw = 0, 1
              i = 0
              do kt = 1, ntot

C               Not all REI subsystems were assigned zones. Try again, 
c               but bus be less particular.

                if (ikk(1, kt) .ne. 1) then
                  if (.not. (ksw .eq. 0 .and. ikk(5, kt) .ne. 1)) then
                    i3 = ikk(3, kt)
                    if (i3 .ne. 0) then
                      if (ztemp(i3) .eq. '$$') then
                        i = i + 1
                        k = opt2inp(kt)
                        ztemp(i3) = zone(k)
                        if (i .eq. knt) goto 280
                      endif
                    endif
                  endif
                endif
              enddo
            enddo

  280       continue
            do i = 1, knt
              zn = ztemp(i)
              zone(ntot+i) = zn
              k = ntot + knt + 2*i - 1
              zone(k) = zn
              zone(k+1) = zn
            enddo
            goto 350
  290       write (errbuf(1), 10310) bus(opt2inp(k)), base(opt2inp(k))
10310       format (
     &       'Inviable Terra Equivalent node cannot receive REI subsyste
     &m from node '
     &       , a8, f6.1)
            call prterx('W', 1)
            kerrsw = 1
            goto 360
  300       write (errbuf(1), 10320)
10320       format (
     &       'More than 2000 generator candidates coalesced into rei sub
     &systems.'
     &       )
            call prterx('W', 1)
            kerrsw = 1
            goto 360
  310       write (errbuf(1), 10320)
            call prterx('W', 1)
            kerrsw = 1
            goto 360
  320       write (errbuf(1), 10330) yptr
10330       format ('0 REI system overflowed, allocated:', i5)
            call prterx('W', 1)
            kerrsw = 1
            goto 360
  330       write (errbuf(1), 10340) bus_name, bus_base, ix
10340       format (' Error adding REI bus ', a8, f6.1, 1x, i5, 
     &       ' to system ')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerrsw = 1
            goto 360
  340       write (errbuf(1), 10340) bus_name, bus_base, ix
            kerrsw = 1
            goto 360
          endif
        endif

  350   continue
      endif

  360 if (kerrsw .gt. 0) rei = 1

      return
      end
