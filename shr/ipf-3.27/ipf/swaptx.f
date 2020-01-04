C    @(#)swaptx.f	20.3 2/13/96
      subroutine swaptx
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/data.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/sort2.inc'
C
      character tempc*10, fctxtx*130
      save
 
C
C     SORT "SAVE ZONES"
C
      entry spzone (m,n)
 
      tempc = savzns(m)
      savzns(m) = savzns(n)
      savzns(n) = tempc
      return
C
C     SORT "SAVE AREAS"
C
      entry sparea(m,n)
 
      tempc = savare(m)
      savare(m) = savare(n)
      savare(n) = tempc
      return
C
C     SORT "SAVE BASES"
C
      entry spbase(m,n)
 
      temp = savbas(m)
      savbas(m) = savbas(n)
      savbas(n) = temp
      return
C
C     SORT NEW SYSTEM "BUS" AND "BASE" ARRAYS
C
      entry spbus(m,n)
 
      tempc = bus(m)
      bus(m) = bus(n)
      bus(n) = tempc
      temp = base(m)
      base(m) = base(n)
      base(n) = temp
      return
C
C     SORT "SAVE INTERFACE" ARRAY
C
      entry spface(m,n)
 
      do i = 1,4
         itemp = face(i,m)
         face(i,m) = face(i,n)
         face(i,n) = itemp
      enddo
      tempc = facec(m)
      facec(m) = facec(n)
      facec(n) = tempc
      return
C
C     SORT INTERFACE ARRAY "IFSORT"
C
      entry spintr(m,n)
 
      do i = 1,2
         itemp = ifsort(i,m)
         ifsort(i,m) = ifsort(i,n)
         ifsort(i,n) = itemp
      enddo
      fctxtx =fcetxt(m)
      fcetxt(m)=fcetxt(n)
      fcetxt(n)=fctxtx
      return
C
C     SORT BRANCH DELETE ARRAY "NDELBR"
C
      entry spbrch(m,n)
 
      do i = 1,2
         itemp = ndelbr(i,m)
         ndelbr(i,m) = ndelbr(i,n)
         ndelbr(i,n) = itemp
      enddo
      return
      end
