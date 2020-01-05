C    @(#)number.f	20.3 2/13/96
      subroutine number
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/elim.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/renum.inc'

      xindex(0) = 0
      ich1(0) = 0
      ich2(0) = 0
      ntotx = ntot
      do i = 1,500
         xindex(i) = 0
      enddo
      do i = 1,ntot
         inp2opt(i) = 0
         j = kownt(2,i)
         if (j .eq. 0) then
            kownt(2,i) = 1
            inp2opt(i) = ntotx
            ntotx = ntotx - 1
         else
            if (j .gt. 0) then
               kownt(1,i) = j
               kownt(2,i) = 1
            else
               j = -j
               kownt(1,i) = j
               kownt(2,i) = 0
            endif
            k = xindex(j+1)
            ich1(i) = k
            ich2(i) = 0
            ich2(k) = i
            xindex(j+1) = i
         endif
      enddo
      xindex(0) = 0
      ich1(0) = 0
      ich2(0) = 0
      nelim = 1
      ncmin = 1
      ncn = ncmin - 1
c
c     Beginning of nodal elimination loop: Get first node in lowest
c     ordered chain
c
  180 do while (ncn .lt. 500 .and. (xindex(ncn+1) .eq. 0 ))
         ncn = ncn + 1
      enddo
      if (ncn .ge. 500) then
         write (errbuf(1),190) nelim,ntotx
  190    format(' Ill-conditioned diagonals between steps ', i5,
     &    ' and ', i5, ' has forced scheme 2 to revert to scheme 1')
         call prterx ('W',1)
         do i = 1, ntot
            j = inp2opt(i)
            if (j .eq. 0) then
               inp2opt(i) = nelim
               write (outbuf,200) i,bus(i),base(i),nelim
  200          format(' node ',i4,' (bus ',a8,f6.1,
     &                ') is forced to order ',i4)
               call prtout (1)
               nelim = nelim + 1
               if (nelim .ge. ntotx) go to 230
            endif
         enddo
  230    continue
         go to 900
      endif
c
c     "ibus" is the first node in the chain having the lowest valency
c
      ibus = xindex(ncn+1)
      xindex(0) = 0
      ich1(0) = 0
      ich2(0) = 0
      ilast = 0
c
c     Search for ELIGIBLE candidates (non-zero diagonals).
c
      do while (kownt(2,ibus) .le. 0)
         ilast = ibus
         ibus = ich1(ibus)
         if (ibus .eq. 0) then
            ncn = ncn + 1
            go to 180
         endif
      enddo
c
c     "ibus" is the eligible node, "inext" is the next candidate.
c
      inext = ich1(ibus)
      if (kase1(40) .gt. 1) call rdump
      if (ilast .gt. 0) then
         ich2(inext) = ilast
         ich1(ilast) = inext
      else
         xindex(ncn+1) = inext
         ich2(inext) = 0
         ncmin = ncn
      endif
      inp2opt(ibus) = nelim
      if (kase1(40) .ne. 0) then
         write (dbug,280) ibus,bus(ibus),base(ibus),nelim
  280    format(' node ',i4,' bus (',a8,f6.1,') is eliminated in ',i4,
     &          ' step  ')
      endif
      if (nelim .ge. ntotx) go to 900
      nelim = nelim + 1
      l1 = loc2(ibus)
      l2 = loc1(ibus)
      itot = 0
      jtot = 0
c
c     Build list "ix()" and "jx()" of positively and negatively
c     incident branches.
c
      if (l1 .gt. 0) then
         lx = l1
         do while (l1 .gt. 0)
            if (kolum2(l1) .ne. ibus) then
               itot = itot + 1
               ix(itot) = kolum2(l1)
            endif
            lx = l1
            l1 = kordr2(l1)
         enddo
         kordr2(last2) = loc2(ibus)
         loc2(ibus) = 0
         kordr2(lx) = 0
         last2 = lx
      endif
      if (l2 .gt. 0) then
         lx = l2
         do while (l2 .ne. 0)
            if (kolum1(l2) .ne. ibus) then
               jtot = jtot + 1
               jx(jtot) = kolum1(l2)
            endif
            lx = l2
            l2 = kordr1(l2)
         enddo
         kordr1(last1) = loc1(ibus)
         kordr1(lx) = 0
         loc1(ibus) = 0
         last1 = lx
      endif
      if (kase1(40) .ne. 0) then
         write (dbug,350) last2,itot,(ix(j),j = 1,itot)
  350    format(' last2 ',i5,' itot ',i4,' ix ',20i5/(26x,20i5))
         write (dbug,360) last1,jtot,(jx(j),j = 1,jtot)
  360    format(' last1 ',i5,' jtot ',i4,' jx ',20i5/(26x,20i5))
      endif
      call elimbs (ibus)
      do 460 j = 1,jtot
         jbus = jx(j)
         j1 = kownt(1,jbus)
         j2 = kownt(2,jbus)
         jadd = jcon(j)
         if (jadd+j1.lt.0) jadd = -j1
         xindex(0) = 0
         ich1(0) = 0
         ich2(0) = 0
         if (jadd .eq. 0) then
         else
            if (jadd .lt. 0) then
                ncmin = min0(ncmin,j1+jadd)
            endif
            k = ich1(jbus)
            l = ich2(jbus)
            if (l .gt. 0) then
               ich1(l) = k
               ich2(k) = l
            else
               ich2(k) = 0
               xindex(j1+1) = k
            endif
            j1 = j1 + jadd
            k = xindex(j1+1)
            xindex(j1+1) = jbus
            ich1(jbus) = k
            ich2(jbus) = 0
            ich2(k) = jbus
         endif
         if (j2 .eq. 0) then
c
c           Determine if ill-conditioned node "jbus" has now become
c           well-conditioned as a result of eliminating node "ibus".
c
            do k = 1,itot
               if (ix(k) .eq. jbus) then
                  j2 = 1
                  ncmin = min0(ncmin,j1)
                  go to 450
               endif
            enddo
         endif
  450    kownt(1,jbus) = j1
         kownt(2,jbus) = j2
  460 continue
      ncn = ncmin - 1
      go to 180
  900 continue
      return
      end
