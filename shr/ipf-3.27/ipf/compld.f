C    @(#)compld.f	20.3 2/13/96
      integer function compld (last, ix, jx, find)
C
C     This subroutine searches for the concatenated pattern
C
C         {BUSCOD[1..IX] // BUSCOD[1..IX]}
C
C     for matching within all existing patterns.
C
C     LAST = maximum entities in BUSCOD() and its derivatives which must
C            be searched.
C     IX   = First entity in BUSCOD() of pattern No. 1.
C     JX   = First entity in BUSCOD() of pattern No. 2.
C
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/ikk.inc'
 
      common /amtrx/ buspct(2,3,MAXBUS), ownlis(100), buscod(2*MAXBUS),
     1               nxtbus(2*MAXBUS), businx(2*MAXBUS),
     2               lstbus(2*MAXBUS)
      character ownlis * 3, buscod * 9
      integer businx
 
      logical match1, match2
      integer find
 
      find = 0
C
C     Match starting point of pattern No. 1.
C
      do 150 ip1s = 1, last
         if (lstbus(ip1s) .eq. 0) then
            ip2s = ix
            match1 = .false.
            match2 = .false.
            if (ip1s .gt. 0 .and. ip2s .gt. 0) then
               if (buscod(ip1s) .eq. buscod(ip2s)) then
                  i = businx(ip1s)
                  j = businx(ip2s)
                  do 110 k = 1, 3
                     if (buspct(1,k,i) .ne. buspct(1,k,j)) then
                        go to 150
                     endif
  110             continue
                  match1 = .true.
C
C                 Now match remaining items in pattern No. 1 and
C                 eventually pattern No. 2.
C
                  ip1 = nxtbus(ip1s)
                  ip2 = nxtbus(ip2s)
  130             if (ip1 .gt. 0 .and. ip2 .gt. 0) then
                     if (buscod(ip1) .eq. buscod(ip2)) then
                        i = businx(ip1)
                        j = businx(ip2)
                        do 140 k = 1, 3
                           if (buspct(1,k,i) .ne. buspct(1,k,j)) then
                              go to 150
                           endif
  140                   continue
                        if (match1) match2 = .true.
                        ip1 = nxtbus(ip1)
                        ip2 = nxtbus(ip2)
                        go to 130
                     else
                        go to 150
                     endif
                  else if (ip1 .eq. 0 .and. ip2 .eq. 0) then
                     if (match1 .and. match2) then
                        find = ip1s
                        go to 160
                     else if (match1) then
                        ip1 = nxtbus(ip1)
                        ip2 = jx
                        go to 130
                     endif
                  endif
               else
                  go to 150
               endif
 
            endif
         endif
  150 continue
  160 compld = find
      return
      end
