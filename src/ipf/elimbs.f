C    @(#)elimbs.f	20.3 2/13/96
      subroutine elimbs (ib)
      include 'ipfinc/parametr.inc'
      include 'ipfinc/elim.inc'
      include 'ipfinc/renum.inc'
c
c     This subroutnie eliminates node ib from the digraph.
c
c     The diagraph is defined with two topologically equivalent sets
c     -- the positively incident set and the negatively incident set.
c
c     The positively incident set is defined with "loc1", "kolum1",
c     and "kordr1".  The negatively incident set is defined with
c     "loc2", "kolum2", and "kordr2".
c
c     The orientation is as follows:
c
c                   i <--- j
c
c     "i" is the positive-incident branch with respect to "j"
c     "j" is the negative-incident branch with respect to "i".
c
c     Pass 1 - merge branches jx() <-- ib <-- ix(), and delete the
c     eliminated branches jx() <-- ib.
c
c     Obtain ibs, the current node in jx()
c
      i = 0
  100 i = i + 1
      if (i.gt.jtot) go to 300   ! This is the only exit from pass 1.
      ibs = jx(i)
      jcon(i) = -1
      assign 162 to isw
      ksw = 1
c
c     Obtain the first negatively incident branch ibs <-- k2 from
c     node ibs.
c
  120 l2 = loc2(ibs)
      ilast = 0
      go to 130
c
c     Obtain the next negatively incident branch ibs <-- k2 from
c     node ibs.
c
  140 ilast = l2
      l2 = kordr2(l2)
  130 do while (l2 .ne. 0)
         k2 = kolum2(l2)
         if (k2 .ne. ib) go to 160
         inext = kordr2(l2)         ! Delete the eliminated branch
         kordr2(last2) = l2         ! ibs <-- ib from node ibs.
         last2 = l2
         kordr2(last2) = 0
         if (ilast .ne. 0) then
            kordr2(ilast) = inext
         else
            loc2(ibs) = inext
         endif
         l2 = inext
      enddo
      if (ksw .eq. 1 .or. ksw .eq. 3) ksw = ksw + 1
  160 go to isw (162,170,200,362,370,500)
c
c     Obtain k1 from ix(). This defines the first equivalent
c     branch ibs <-- k1 from node ibs.
c
  162 j = 0
  170 j = j + 1
      if (j.gt.itot) go to 180
      k1 = ix(j)
      if (k1.eq.ibs) go to 170
      go to 190
  180 if (ksw .lt. 3) ksw = ksw + 2
  190 continue
c
c     Determine whether the equivalent negatively incident branch
c     ibs <-- k1 is a new branch from node ibs, or if it already
c     exist.
c
c     ksw is assigned the following attributes
c
c        1 -- normal
c        2 -- end-of-branches for node k2
c        3 -- end-of-branches for node k1
c        4 -- end-of-branches for nodes k1 and k2
c
  200 go to (210,220,250,100) ksw
  210 if (k1 - k2) 220,242,260
c
c     Equivalent branch ibs <-- k1 does not exist.  Add this branch to
c     ibs.
c
  220 inow = next2
      if (inow.eq.0) call erexit
      next2 = kordr2(next2)
      kolum2(inow) = k1
      kordr2(inow) = l2
      if (ilast .ne. 0) then
         kordr2(ilast) = inow
      else
         loc2(ibs) = inow
      endif
      l2 = inow
      jcon(i) = jcon(i) + 1
c
c     Obtain the next k2 from ibs and the next k1 from ix(), and
c     repeat the examination for the next equivalent branch ibs <-- k1
c     from node ibs.
c
  242 assign 170 to isw
      go to 140
c
c     Obtain the next k2 from ibs.  There are no more equivalent
c     branches to check for or add to bus ibs.  However, looping
c     is continued to delete the eliminated branch ibs <-- ib.
c
  250 if (k2.gt.ib) go to 100
      assign 200 to isw
      go to 140
c
c     Advance the next k2 from ibs and continue checking for the
c     presence of equivalent branch ibs <-- k1.
c
  260 assign 200 to isw
      go to 140
c
c     Pass 2 - merge branches ix() --> ib --> jx(), and delete the
c     eliminated branches ix() --> ib.
c
c     Obtain ibs, the current node in ix()
c
  300 i = 0
  310 i = i + 1
      if (i.gt.itot) go to 570   ! This is the only exit from pass 2.
      ibs = ix(i)
      assign 362 to isw
      ksw = 1
c
c     Obtain the first positively incident branch ibs --> k2 from
c     node ibs.
c
  320 l2 = loc1(ibs)
      ilast = 0
      go to 330
c
c     Obtain the next positively incident branch ibs --> k2 from
c     node ibs.
c
  340 ilast = l2
      l2 = kordr1(l2)
  330 do while (l2 .gt. 0)
         k2 = kolum1(l2)
         if (k2.ne.ib) go to 360
         inext = kordr1(l2)         ! Delete the eliminated branch
         kordr1(last1) = l2         ! ibs --> ib from node ibs.
         last1 = l2
         kordr1(last1) = 0
         if (ilast .gt.0) then
            kordr1(ilast) = inext
         else
            loc1(ibs)= inext
         endif
         l2 = inext
      enddo
  350 if (ksw.eq.1.or.ksw.eq.3) ksw = ksw + 1
  360 go to isw(162,170,200,362,370,500)
c
c     Obtain k1 from jx(). This defines the first equivalent
c     branch ibs --> k1 from node ibs.
c
  362 j = 0
  370 j = j + 1
      if (j.gt.jtot) go to 380
      k1 = jx(j)
      if (k1.eq.ibs) go to 370
      go to 390
  380 if (ksw.lt.3) ksw = ksw + 2
  390 continue
c
c     Determine whether the equivalent positively incident branch
c     ibs --> k1 is a new branch from node ibs, or if it already
c     exist.
c
c     ksw is assigned the following attributes
c
c        1 -- normal
c        2 -- end-of-branches for node k2
c        3 -- end-of-branches for node k1
c        4 -- end-of-branches for nodes k1 and k2
c
  500 go to (510,520,550,310) ksw
  510 if (k1 - k2) 520,542,560
c
c     Equivalent branch ibs --> k1 does not exist.  Add this branch to
c     ibs.
c
  520 inow = next1
      if (inow.eq.0) call erexit
      next1 = kordr1(next1)
      kolum1(inow) = k1
      kordr1(inow) = l2
      if (ilast .gt. 0) then
         kordr1(ilast) = inow
      else
         loc1(ibs) = inow
      endif
      l2 = inow
c
c     Obtain the next k2 from ibs and the next k1 from ix(), and
c     repeat the examination for the next equivalent branch ibs --> k1
c     from node ibs.
c
  542 assign 370 to isw
      go to 340
c
c     Obtain the next k2 from ibs.  There are no more equivalent
c     branches to check for or add to bus ibs.  However, looping
c     is continued to delete the eliminated branch ibs --> ib.
c
  550 if (k2.gt.ib) go to 310
      assign 500 to isw
      go to 340
c
c     Advance the next k2 from ibs and continue checking for the
c     presence of equivalent branch ibs --> k1.
c
  560 assign 500 to isw
      go to 340
  570 return
      end
