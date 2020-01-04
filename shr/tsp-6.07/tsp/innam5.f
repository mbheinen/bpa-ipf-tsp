C    %W% %G%
      function innam5(busnam, kvcode)

c     This function is used to compare a bus name and bus kva rating 
c     code for a particular bus to similar identification obtained 
c     from the power flow file.  This identification is contained
c     in EXNAMC and IXBASE. A binary search is made on the two tables.
c     If an agreement is made, an integer indicating the position in 
c     EXNAMC is returned.  Otherwise, the integer returned is zero.  
c     An abort flag (ERRFLAG) is set depending upon the type of card 
c     the base name and code were read from.
 
      include 'tspinc/params.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      character*8 busnam

C     CHECK FOR BLANK

      if (kvcode .le. 0) then
        base = 0.0
      elseif (busnam .eq. ' ') then
        goto 130
      else
        i1 = 1
        i2 = ntot
        do while (.true.)
          innam5 = (i1+i2)/2
          if (kompr(exnamc(innam5), busnam, kdum) .ge. 0) then
            if (kompr(exnamc(innam5), busnam, kdum) .le. 0) then
              if (ixnamn(innam5) .eq. kvcode) goto 140
              if (ixnamn(innam5) .le. kvcode) goto 100
            endif
            i2 = innam5 - 1
            goto 110
          endif
  100     i1 = innam5 + 1
  110     if (i2 .lt. i1) goto 120
        enddo
  120   base = basekv(kvcode)
      endif

      write (errbuf(1), 10000) buffer
10000 format ('0', a)
      write (errbuf(2), 10010) busnam, base
      call prterr('W', 2)
10010 format (' THE CARD WAS IGNORED BECAUSE THE BUS (', a8, f7.1,
     & ') WAS NOT USED IN POWER FLOW PROGRAM.')
      if (errflag .ne. 0) iabort = 1
  130 errflag = 1
      innam5 = 0
  140 return
      end
