C    @(#)komptx.f	20.6 11/11/97
      function komptx (dummy)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/data.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/sort2.inc'
 
      integer shift

      komptx = 0
      return 
 
      entry kpzone(m,n)
 
C     SORT "SAVE ZONES"
 
      kpzone=kompr(savzns(m),savzns(n),junk)
      return
 
      entry kparea(m,n)
 
C     SORT "SAVE AREAS"
 
      kparea=kompr(savare(m),savare(n),junk)
      return
 
      entry kpbase(m,n)
 
C     SORT "SAVE BASES"
 
      ixbase=100.0*(savbas(m)-savbas(n))
      kpbase=ixbase
      return
 
      entry kpbus(m,n)
 
C     SORT NEW SYSTEM "BUS" AND "BASE" ARRAYS
 
      if (kompr (bus(m), bus(n), kpbus) .eq. 0) then
         ixbase = 100.0*(base(m) - base(n))
         kpbus = ixbase
      endif
      return
 
      entry kpface(m,n)
 
C     SORT "SAVE INTERFACE" ARRAY
 
      if (key .eq. 1) then
         kpface = face(4,m) - face(4,n)
         if (kpface .eq. 0) then
            k1 = shift (face(1,m), -16)
            k2 = shift (shift (face(1,m), 16), -16)
            if (k1 .gt. 0) k1 = inp2alf(k1)
            if (k1 .eq. 0) k1 = ntot + 1
            if (k2 .gt. 0) k2 = inp2alf(k2)
            if (k2 .eq. 0) k2 = ntot + 1
            m1 = shift (face(1,n), -16)
            m2 = shift (shift (face(1,n), 16), -16)
            if (m1 .gt. 0) m1 = inp2alf(m1)
            if (m1 .eq. 0) m1 = ntot + 1
            if (m2 .gt. 0) m2 = inp2alf(m2)
            if (m2 .eq. 0) m2 = ntot + 1
            kpface = k1 - m1
            if (kpface .eq. 0) kpface = k2 - m2
         endif
         if (kpface .eq. 0) kpface = face(2,m) - face(2,n)
      else
         k1 = shift (face(1,m), -16)
         k2 = shift (shift (face(1,m), 16), -16)
         if (k1 .gt. 0) k1 = inp2alf(k1)
         if (k1 .eq. 0) k1 = ntot + 1
         if (k2 .gt. 0) k2 = inp2alf(k2)
         if (k2 .eq. 0) k2 = ntot + 1
         m1 = shift (face(1,n), -16)
         m2 = shift (shift (face(1,n), 16), -16)
         if (m1 .gt. 0) m1 = inp2alf(m1)
         if (m1 .eq. 0) m1 = ntot + 1
         if (m2 .gt. 0) m2 = inp2alf(m2)
         if (m2 .eq. 0) m2 = ntot + 1
         kpface = k1 - m1
         if (kpface .eq. 0) kpface = k2 - m2
         if (kpface .eq. 0) kpface = face(2,m) - face(2,n)
      endif
      return
 
      entry kpintr(m,n)
 
C     SORT INTERFACE ARRAY "IFSORT"
 
      k1 = shift (ifsort(1,m), -16)
      k2 = shift (shift (ifsort(1,m), 16), -16)
      if (k1 .gt. 0 .and. k1 .lt. 19999) k1 = inp2alf(k1)
      if (k1 .eq. 0 .or. k1 .eq. 19999) k1 = ntot + 1
      if (k2 .gt. 0 .and. k2 .lt. 19999) k2 = inp2alf(k2)
      if (k2 .eq. 0 .or. k2 .eq. 19999) k2 = ntot + 1
      m1 = shift (ifsort(1,n), -16)
      m2 = shift (shift (ifsort(1,n), 16), -16)
      if (m1 .gt. 0 .and. m1 .lt. 19999) m1 = inp2alf(m1)
      if (m1 .eq. 0 .or. m1 .eq. 19999) m1 = ntot + 1
      if (m2 .gt. 0 .and. m2 .lt. 19999) m2 = inp2alf(m2)
      if (m2 .eq. 0 .or. m2 .eq. 19999) m2 = ntot + 1
      kpintr = k1 - m1
      if (kpintr .eq. 0) kpintr = k2 - m2
      if (kpintr .eq. 0) kpintr = ifsort(2,m) - ifsort(2,n)
      return
 
      entry kpbrch(m,n)
 
C     SORT BRANCH DELETE ARRAY "NDELBR"
 
      kpbrch = ndelbr(1,m) - ndelbr(1,n)
      if (kpbrch.eq.0) kpbrch = ndelbr(2,m) - ndelbr(2,n)
      return
 
      end
