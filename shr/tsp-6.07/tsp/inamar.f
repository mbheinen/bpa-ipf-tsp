C    %W% %G%
      function inamar(tstbus, kb)

C     !DEM

      character*8 tstbus
 
C     This function finds compares bus name (TSTBUS) and bus kva rating 
C     code (KB) for a particular bus to a matching bus from the 
C     powerflow.  It is used after INITL1 has executed instead of INAM.
C
C     The powerflow data is found in arrays EXNAMC for names IXNAMN 
C     for kv codes.  However, they are assumed to be reordered for 
C     fault, DC links and other conditions by INITL1.
C
C     By indirectly addressing these two data arrays via INDX2N,
C     they will appear to still be alphabetically sorted, so that
C     a binary search algorithm can be used.
C
C     INAMAR returns zero if the bus is not found.
C     For now, INAMAR does not produce an error message.
 
      include 'tspinc/params.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/busnum.inc'

C     Begin     Begin     Begin     Begin     Begin     Begin
 
C     No such bus as blank

      if (tstbus .eq. ' ') then
        errflag = 1
        inamar = 0
      else
        ix1 = 1
        ix2 = ntot
        in1 = indx2n(ix1)
        in2 = indx2n(ix2)
        do while (.true.)
          ixmid = (ix1+ix2)/2
          inmid = indx2n(ixmid)
          if (tstbus .le. exnamc(inmid)) then
            if (tstbus .ge. exnamc(inmid)) then

C             Bus name match
C             < tie >

              if (kb .ge. ixnamn(inmid)) then
                if (kb .eq. ixnamn(inmid)) goto 120
                goto 100
              endif
            endif
            ix2 = ixmid - 1
            goto 110
          endif
  100     ix1 = ixmid + 1
  110     if (ix2 .lt. ix1) goto 130
        enddo

C       Here if a kv also matched

  120   inamar = inmid
        goto 140

C       Here if bus name & kv combo not found

  130   errflag = 1
        inamar = 0
      endif
  140 return
      end
