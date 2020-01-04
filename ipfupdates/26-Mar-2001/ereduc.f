C    %W% %G%
      subroutine ereduc

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/zbdata.inc'
      include 'ipfinc/elim2.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/red2.inc'
      include 'ipfinc/reliab.inc'

      integer mtrx(MAXBUS)
      logical finished

C     THIS SUBROUTINE DETERMINES THE OPTIMAL SIZE OF A RETAINED NETWORK
C     WHICH INITIALLY IS DEFINED BY AN ARBITRARY SET OF KERNEL NODES.

      next = 0
      ksw = 0
      nsize = 2*ltot + ltot/4
      nsize = min0(nsize, MAXYE)
      last = nsize

C     INITIALIZE IKK ARRAY

      if (kase1(21) .eq. 0) then
        do i = 1, ntot
          ikk(1, i) = 1
          do j = 2, 5
            ikk(j, i) = 0
          enddo
        enddo

C       EXPLICITY ELIMINATE DELETED BUSES

        do i = ntot_alf+1, ntot
          kt = inp2opt(alf2inp(i))
          ikk(1, kt) = 0
          kmlen(kt) = 0
        enddo

C       EXPLICITILY ELIMINATE PASSIVE DC BUSES

        if (mtdcbs .ne. 0) then
          do kt = nbslck + 1, ntot
            if (kmlen(kt) .eq. 0) ikk(1, kt) = 0
          enddo
        endif

      else
        do i = 1, ntot
          do j = 1, 5
            ikk(j, i) = 0
          enddo
        enddo

C       UNCONDITIONALLY SAVE SYSTEM SLACK BUSES

        do i = 1, nbslck
          ikk(1, i) = 1
        enddo

C       EXPLICITY ELIMINATE DELETED BUSES

        do i = ntot_alf+1, ntot
          j = inp2opt(alf2inp(i))
          ikk(1, j) = 0
          kmlen(j) = 0
        enddo

C       FLAG PASSIVE DC BUS-FLAG FOR ELIMINATION

        if (mtdcbs .ne. 0) then
          do kt = nbslck + 1, ntot
            if (kmlen(kt) .eq. 0) ksw = 1
          enddo
        endif

        if (.not. (iequiv .eq. 0 .and. ksw .eq. 0)) then
          if (iequiv .ne. 0) then

C           DETERMINE RETAINED SYSTEM FROM OVERLOAD AND OUTAGE LISTS

            do i = 1, nout
              k1 = klnc(1, i)
              ikk(1, k1) = 1
              k2 = klnc(2, i)
              ikk(1, k2) = 1
            enddo
            do i = 1, novl
              k1 = klno(1, i)
              ikk(1, k1) = 1
              k2 = klno(2, i)
              ikk(1, k2) = 1
            enddo
            nret = kase1(10)

C           Optionally expand per > NETWORK, ZONES = ..., BASES - ...

C           OPTIONALLY EXPAND RETAINED SYSTEM "NRET" NODES ADJACENT ORIGINAL B

            if (num_network .gt. 0) then

              do nbx = 1, ntot_alf
                nb = alf2inp(nbx)
                kt = inp2opt(nb)
                if (ikk(1,kt) .eq. 0) then
                  finished = .false.
                  i = 1
                  do while (.not. finished)
                    if (zn_network(i) .eq. zone(nb)) then
                       if (vl_network .le. base(nb) .and.
     &                     vh_network .ge. base(nb)) ikk(1,kt) = 1
                       finished = .true.
                    else
                       i = i + 1
                       finished = (i .gt. num_network)
                    endif
                  enddo
                endif
              enddo
            endif

            if (irect .eq. 0 .and. chase1(22) .eq. ' ') 
     &       nret = max0(2, nret)
            if (nret .ne. 0) then
              n1 = 0
              n2 = 0
              do i = 1, ntot
                if (ikk(1, i) .ne. 0) then
                  n2 = n2 + 1
                  mtrx(n2) = i
                endif
              enddo
              n3 = n2
              do i = 1, nret
                do while (n1 .lt. n2)
                  n1 = n1 + 1
                  kt = mtrx(n1)
                  do l = km(kt), km(kt) - 1 + kmlen(kt)
                    mt = ikmu(l)
                    if (ikk(1, mt) .ne. 1) then
                      n3 = n3 + 1
                      mtrx(n3) = mt
                      ikk(1, mt) = 1
                    endif
                  enddo
                enddo

                if (n2 .eq. n3) goto 100
                n1 = n2
                n2 = n3
              enddo
            endif
  100       if (.not. (kase1(26) .eq. 0 .or. nrelib .eq. 0)) then

C             RETAIN NODES FOR RESCHEDULING.

              do i = 1, nrelib
                k1 = nbsout(1, i)
                k1 = inp2opt(k1)
                ikk(1, k1) = 1
              enddo
            endif

C           OPTIONALLY IMPLEMENT REI EQUIVALENT SYSTEM

            if (kase1(22) .ne. 0) then
              if (jtie .ne. 0) then
                do i = 1, jtie
                  if (tie(2, i) .lt. tie(8, i)) then
                    k1 = tie(1, i)
                  else
                    k1 = tie(7, i)
                  endif
                  k1 = inp2opt(k1)
                  ikk(1, k1) = 1
                enddo
                do j = 1, ntotc
                  k1 = karea(1, j)
                  k1 = inp2opt(k1)
                  ikk(1, k1) = 1
                enddo
              endif
            endif
          endif
          iequiv = 1
          next = 0
          do kt = 1, ntot
            if (kmlen(kt) .ne. 0) then
              loc(kt) = next + 1
              lf = km(kt) - 1
              do l = 1, kmlen(kt)
                mt = ikmu(l+lf)
                next = next + 1
                kolum(next) = mt
                korder(next) = next + 1
              enddo
              korder(next) = 0
            else
              loc(kt) = 0
            endif
            is = 1
            if (ikk(1, kt) .eq. 0) is =  - 1
            kownt(1, kt) = isign(kmlen(kt), is)
          enddo
          next = next + 1
          do i = next, last
            kolum(i) = 0
            korder(i) = i + 1
          enddo
          korder(last) = 0
          call reord2(ntot, kase1(30))
          do i = 1, ntot
            if (ikk(1, i) .eq. 1) then
              ikk(4, kt) = 0
            elseif (ikk(4, kt) .ne. 0 .or. ikk(5, kt) .gt. nopt) then
              ikk(1, kt) = 1
              ikk(4, kt) = 1

            endif
          enddo
        endif
      endif
      return
      end
