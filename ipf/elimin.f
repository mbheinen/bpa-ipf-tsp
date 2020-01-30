C    @(#)elimin.f	20.3 2/13/96
      subroutine elimin(ibs,kerr)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/elim2.inc'
      include 'ipfinc/red2.inc'
 
C     PROCESS BUS "JBS"
C
      kerr=0
      do 340 i = 1,itot
      jbs  = ix(i)
      icon(i) = -1
      ksw = 1
      assign 190 to isw
C
C     LOGIC TO ADVANCE INDICES CORRESPONDING TO NATURAL BRANCHES OF JBS
C
  110 l2 = loc(jbs)
      l3 = 0
      ilast = 0
      go to 130

  120 ilast = l2
      l2 = korder(l2)
  130 if (l2.eq.0) go to 150
  140 k2 = kolum(l2)
      if (k2.eq.ibs) then
        if (ilast.eq.0) then
          loc(jbs) = korder(l2)
        else
          korder(ilast) = korder(l2)
        endif
        korder(last) = l2
        last = l2
        l2 = korder(l2)
        kolum(last) = 100000
        korder(last) = 0
        go to 130
      endif
      go to 160

  150 if (ksw.eq.1.or.ksw.eq.3) ksw = ksw + 1
  160 go to isw (190,230)
C
C     LOGIC TO ADVANCE INDICES CORRESPONDING TO EQUIVALENT BRANCHES OF
C     JBS
C
  190 l3 = l3 + 1
  200 if (l3.gt.itot) go to 210
      k3 = ix(l3)
      if (k3.eq.jbs) go to 190
      go to 220

  210 if (ksw.lt.3) ksw = ksw + 2
  220 continue
C
C     THE FOLLOWING LOGIC PERFORMS A MERGE-PASS BETWEEN THE ORDERED
C     SETS DESIGNATED "K2" AND "K3"
C
C     "KSW" IS ASSIGNED THE FOLLOWING ATTRIBUTES
C
C       1 -- NORMAL
C       2 -- END-OF-DATA BRANCHES FOR NODE K2
C       3 -- END-OF-DATA EQUIVALENT BRANCHES FOR NODE K3
C       4 -- END-OF-DATA (2 AND 3)
C
  230 go to (240,310,250,340) ksw
  240 if (k2 - k3) 250,300,310
C
C     INCREMENT BRANCHES FOR K2
C
  250 assign 230 to isw
      go to 120
C
C     INCREMENT BRANCHES FOR K2 AND K3
C
  300 assign 190 to isw
      go to 120
C
C     ADD EQUIVALENT BRANCH (K2 <--> K3) AND INCREMENT BRANCHES FOR K3
C
  310 icon(i) = icon(i) + 1
      if (next.eq.0) then
        kerr = 1
        go to 360
      endif
      lx = next
      next = korder(next)
      if (ilast.eq.0) go to 320
      korder(ilast) = lx
      go to 330

  320 loc(jbs) = lx
  330 kolum(lx) = k3
      korder(lx) = l2
      ilast = lx
      go to 190

  340 continue
  360 return
      end
