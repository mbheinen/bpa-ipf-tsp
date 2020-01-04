C    @(#)reord2.f	20.4 11/11/97
      subroutine reord2(ktot, kdebug)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/elim2.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red2.inc'

c     Note: This variable can be changed with a symbolic debugger
      common /term_debug/ iterm

      do i = 1, 800
        indexx(i) = 0
      enddo
      korder(0) = 0
      ich2(0) = 0
      ich1(0) = 0
      nopt = 0
      ncmax = 0
      ncmin = 0
      nzd = 0
      ix0 = 200
      do i = 1, ktot
        j =  -kownt(1, i)
        if (j .lt. 0) then
          kownt(1, i) = -j
          kownt(2, i) = 0
        else if (j .gt. 0) then
          nzd = nzd + 1
          iv = ivaln(i)
          iv = max0(iv, -ix0)
          iv = min0(iv, 800-ix0)
          kownt(1, i) =  -j
          kownt(2, i) = iv
          ncmin = min0(ncmin, iv)
          ncmax = max0(ncmax, iv)
          k = indexx(iv+ix0)
          ich1(i) = k
          if (k .gt. 0) ich2(k) = i
          indexx(iv+ix0) = i
          ich2(i) = 0
          ikk(4, i) = 1
        endif
      enddo

c     Define KBSKNT and KBRKNT to pertain to eliminated and envelope
c     network only.

      kbrknt = 0
      kbsknt = 0
      do i = 1, ktot
        if (kownt(1, i) .lt. 0) then

          kbsknt = kbsknt + 1
          l1 = loc(i)
          do while (l1 .ne. 0)
            kbrknt = kbrknt + 1
            l1 = korder(l1)
          enddo
        elseif (kownt(1, i) .ne. 0) then
          ic = 0
          l1 = loc(i)
          do while (l1 .ne. 0)
            k = kolum(l1)
            if (kownt(1, k) .lt. 0) ic = ic + 1
            l1 = korder(l1)
          enddo

          if (ic .gt. 0) ikk(3, i) = 1
        endif
      enddo
      do i = 1, ktot
        if (kownt(1, i) .gt. 0) then

          if (ikk(3, i) .ne. 0) then
            l1 = loc(i)
            kbsknt = kbsknt + 1
            do while (l1 .ne. 0)
              k = kolum(l1)
              if (kownt(1, k) .ge. 0) then
                if (ikk(3, k) .eq. 0) goto 100
              endif
              kbrknt = kbrknt + 1
  100         l1 = korder(l1)
            enddo
          endif
        endif
      enddo
      nmin = kbsknt + kbrknt
      if (kdebug .ne. 0) write (dbug, 10000)
10000 format ('1 NODE ELIMINATED  STEP  VALENCY  NODES REMAINING  BRAN'
     & , 'ches remaining  ratio  sparsity factor equivalent network ', 
     & 'size '//)

c     NZD is the count of nodes in the eliminated system. All or part
c     may be eliminated. Only these nodes are candidates for elimination

      ksw = 0
      nelim = 0
      if (kdebug .eq. 3) call debug(ktot)
  110 continue
      do ncn = ncmin, ncmax
        i = indexx(ncn+ix0)
        if (i .ne. 0) goto 120
      enddo
      goto 140

  120 if (kdebug .eq. 3) call debug(ktot)
      ich2(0) = 0
      ich1(0) = 0

      ncmin = ncn

c     check validity of ICH1 <--> ICH2

      if (kdebug .ne. 0) then
        call rcheck(ncmin, ncmax, inum)
        j = nzd - nelim
        if (inum+nelim .ne. nzd) then
          write (dbug, 10010) nelim, inum, j, ncmin, ncmax
10010     format (' PARTIAL LOSS OF SUBSYSTEM AT ELIMINATION STEP ', 
     &     5i6)
        endif
      endif
      inext = ich1(i)
      iv = ivaln(i)
      indexx(ncn+ix0) = inext
      ich2(inext) = 0
      ich1(i) = 0
      kownt(2, i) = 20000
      nelim = nelim + 1
      ikk(4, i) = 0
      ikk(5, i) = nelim
      if (nelim .ne. nzd) then
        if (ksw .ne. 1) then
          if (iv .ne. -ix0) then
            if (iv .lt. 800-ix0) goto 130
          endif
          iv = ivaln(i)
  130     continue
          kbsknt = kbsknt - 1
          kbrknt = kbrknt + iv
          ic = kbsknt + kbrknt
          if (kdebug .ne. 0) then
            write (dbug, 10020) i, nelim, iv, kbsknt, kbrknt, ic
10020       format (i13, i10, i7, i16, i18, 25x, i20)
          endif
          if (ic .le. nmin) then
            nmin = ic
            nopt = nelim
          endif

c         Build IX array of adjacent nodes

          itot = 0
          l1 = loc(i)
          lx = l1
          do while (l1 .ne. 0)
            itot = itot + 1
            ix(itot) = kolum(l1)
            kolum(l1) = 100000
            lx = l1
            l1 = korder(l1)
          enddo
          korder(last) = loc(i)
          loc(i) = 0
          last = lx
          if (kdebug .gt. 1) then
            write (dbug, 10030) i, itot, (ix(j), j = 1, itot)
10030       format (' ELIMINATING NODE ', i4, ' BRANCHES ', 11i6/(38x, 
     &       10i6))
          endif
          if (last .ne. 0) then
            if (itot .eq. 0) goto 110

            call elimin(i, kerr)
c
c           Debug toplolgical symmetry check
c
            if (iterm .ne. 0) then
              write (dbug, 10032) i, nelim
              write (*, 10032) i, nelim
10032         format (' Eliminating node ', i4, ' at step ', i4)
              call ck_topol (ktot, kerr)
            endif

            if (kerr .eq. 0) then
              do j = 1, itot
                jbs = ix(j)
                j2 = kownt(1, jbs)
                iv = kownt(2, jbs)
                if (j2 .le. 0) then
                  ich2(0) = 0
                  ich1(0) = 0
                  j1 = ivaln(jbs)
                  j1 = max0(j1, -ix0)
                  j1 = min0(j1, 800-ix0)
                  if (j1 .ne. iv) then
                    if (ich2(jbs) .eq. 0) then
                      k = ich1(jbs)
                      indexx(iv+ix0) = k
                      ich2(k) = 0
                    else
                      l = ich2(jbs)
                      k = ich1(jbs)
                      if (l .gt. 0) ich1(l) = k
                      if (k .gt. 0) ich2(k) = l
                    endif
                    ich1(0) = 0
                    ich2(0) = 0
                    k = indexx(j1+ix0)
                    indexx(j1+ix0) = jbs
                    ich1(jbs) = k
                    if (k .gt. 0) ich2(k) = jbs
                    ich2(jbs) = 0
                    ncmin = min0(ncmin, j1)
                    ncmax = max0(ncmax, j1)
                  endif
                  kownt(1, jbs) = min0(0, j2-icon(j))
                  kownt(2, jbs) = j1
                endif
              enddo
              goto 110
            endif
          endif

c         Curtail futher elimination -- storage limit reached.

          write (errbuf(1), 10040) nelim
10040     format (
     &     ' AN EXTREMELY DENSE NETWORK WAS DEVELOPED AFTER ELIMINATINg 
     &'
     &     , i4, ' nodes from system. ')
          call prterx('W', 1)
          ksw = 1
        endif
        goto 110
      endif
      if (kbsknt+kbrknt-1 .le. nmin) nopt = nelim
  140 if (nelim .gt. nopt) then
        write (outbuf, 10050) nopt, nelim
10050   format ('0 AN OPTIMALLY DETERMINED NETWORK HAS ELIMINATED ', 
     &   i4, ' NOdes from a total of ', i4, ' candidates.')
        call prtout(1)
      endif
      return
      end
