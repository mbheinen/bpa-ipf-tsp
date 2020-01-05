C    %W% %G%
      subroutine lserch
 
C     This subroutine checks to insure that all LZ cards entered
C     are used in the line switching sequence.  It then searchs through
C     the line switching table and finds the pi admittances for each
C     line switched.  Admittances come from the branch data table
C     if the line existed in the POWERFLOW or from the IJSLN (LZ) tables
C     if the line was entered on LZ cards.  It is called by INPUT2.
 
      include 'tspinc/params.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/ibrnch.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
 
      character*8 i1c, j1c
      character*1 kpc
  
C     Sort through the line switching tables and add any faulted
c     line to the default distance relay bypass tables if a
c     default distance relay is present
 
      if (ifcd .ne. 0 .and. iswbps .ne. 0) then
        do i = 1, ifcd
          if (mflt(i) .le. 2) then
            if (mflt(i) .ge. -2) then
              nbypas = nbypas + 1
              kbpass(1, nbypas) = iabs(iftabn(i))
              kbpass(2, nbypas) = iabs(jftabn(i))
            endif
          endif
        enddo
      endif
 
C     Eliminate the entries in the branch immittance (LZ) tables that
c     are not referred to with a line switching card (LS).
C     IJSLN(1,J),(2,J), and (3,J) are the bus numbers and section no.
c     for the jth line in the branch immittance table.
C     IBRNHN(1,4,I),(2,4,I), and (3,4,I) are the bus numbers and
c     section no. for the ith line in the switching table.
C     IJSLC(J) is the parallel id for the branch immittance tables
C     and IBRNHC(I) is the parallel for the LS tables.
 
      if (ibr .ne. 0) then
        if (lst .le. iline) then
          do j = lst, iline
            iswt = 0
            do while (.true.)
              do i = 1, ibr
                do ij = 1, 3
                  if (ijsln(ij, j) .ne. ibrnhn(ij, 4, i)) goto 100
                enddo
                if (kompr(ijslc(j), ibrnhc(4, i), kdum) .eq. 0) goto
     &           120
  100           continue
              enddo
 
C             If the line cannot be found, reverse the order of the
c             bus numbers and search again.
 
              if (iswt .eq. 1) goto 110
              j1 = ijsln(1, j)
              ijsln(1, j) = ijsln(2, j)
              ijsln(2, j) = j1
              if (iswt .eq. 1) goto 110
              iswt = 1
            enddo
 
C           THE JTH ENTRY IN THE LZ TABLES (IJSLN) CANNOT BE FOUND IN TH
C           LS TABLES (IBRNHN).  REPLACE IJSLN(J) WITH IJSLN(LST), AND
C           ZERO OUT IJSLN(LST) AND THEN DECREASE THE LENGTH OF THE IJSL
C           TABLE BY 1 (LST = LST-1).
 
  110       lst = lst + 1
            ijsln(1, j) = ijsln(1, lst-1)
            ijsln(2, j) = ijsln(2, lst-1)
            ijsln(3, j) = ijsln(3, lst-1)
            ijslc(j) = ijslc(lst-1)
            gijs(j) = gijs(lst-1)
            bijs(j) = bijs(lst-1)
            jj = 2*j
            ii = 2*lst - 2
            gsl(jj-1) = gsl(ii-1)
            bsl(jj-1) = bsl(ii-1)
            gsl(jj) = gsl(ii)
            bsl(jj) = bsl(ii)
            ijsln(1, lst-1) = 0
            ijsln(2, lst-1) = 0
            ijsln(3, lst-1) = 0
            ijslc(lst-1) = ' '
            gijs(lst-1) = 0.0
            bijs(lst-1) = 0.0
            gsl(ii-1) = 0.0
            bsl(ii-1) = 0.0
            gsl(ii) = 0.0
            bsl(ii) = 0.0
            goto 130
  120       if (iswt .ne. 0) then
 
C             LINE HAS BEEN FOUND, BUT BUS NUMBERS IN OPPOSITE ORDER
C             SO SWITCH LINE SHUNTS FROM ONE END TO THE OTHER.
 
              jj = 2*j
              gtemp = gsl(jj)
              gsl(jj) = gsl(jj-1)
              gsl(jj-1) = gtemp
              btemp = bsl(jj)
              bsl(jj) = bsl(jj-1)
              bsl(jj-1) = btemp
            endif
  130       continue
          enddo
        endif
      endif
      do itrr = 1, ibr
 
C       IF THIS LINE HAS ALREADY BEEN PROCESSED, SKIP IT
 
        if (ibrnhn(1, 4, itrr) .ne. 0 .or. ibrnhn(2, 4, itrr) .ne. 0)
     &     then
          ii = ibrnhn(1, 4, itrr)
          jj = ibrnhn(2, 4, itrr)
          ksect = ibrnhn(3, 4, itrr)
          kpc = ibrnhc(4, itrr)
          call brnchy(ii, jj, kpc, ierr, gkm, bkm, gmk, bmk, gk1, bk1, 
     &                gk2, bk2)
 
C         IF IERR = -3, THIS LINE CANNOT BE SWITCHED, SO PROCEDE TO
C         TO THE NEXT LINE.  AN ERROR MESSAGE WAS WRITTEN IN
C         BRNCHY FOR THIS CONDITION.
 
          if (ierr .ne. -3) then
 
C           IF IERR = -1, THEN THE LINE COULD NOT BE FOUND IN THE BRANCH
C           DATA TABLES FROM THE POWER FLOW.  NOW SEARCH THE LZ TABLE
C           (IJSLN) TO SEE IF THE LINE WAS ENTERED ON AN LZ CARD.
 
            if (ierr .eq. -1) then
              do jtrr = lst, iline
                if (ii .eq. ijsln(1, jtrr)) then
                  if (jj .eq. ijsln(2, jtrr)) then
                    if (ksect .eq. ijsln(3, jtrr)) then
                      if (kpc .eq. ijslc(jtrr)) goto 140
                    endif
                  endif
                endif
              enddo
 
C             LINE CANNOT BE FOUND IN THE LZ TABLES EITHER, SO WRITE
C             WRITE ERROR MESSAGE AND GO TO NEXT LS CARD
 
              iabort = 1
              i1c = exnamc(ii)
              kbase = ixnamn(ii)
              base1 = basekv(kbase)
              j1c = exnamc(jj)
              kbase = ixnamn(jj)
              base2 = basekv(kbase)
              write (errbuf(1), 10000) i1c, base1, j1c, base2, kpc,
     &         ksect
              call prterr('E', 1)
10000         format (
     &         ' NO BRANCH DATA CAN BE FOUND FOR LINE SWITCHING CARD ',
     &         a8, f5.1, 2x, a8, f5.1, 1x, a1, 1x, i1)
              goto 150
 
C             LINE HAS BEEN FOUND IN THE LZ TABLE, SO SET IBRNHN(1,5,I)
C             EQUAL TO THE TIME IN CYCLES WHEN THE LINE IS SWITCHED.
 
  140         k = ibrnhn(1, 5, itrr)
              bradd(jtrr) = cyc(k)
 
C             SET ALL ENTRIES IN THE BRNHN TABLE = 0 FOR LINE II ,JJ, KP
C             TO SIGNIFY THAT THIS LINE HAS BEEN PROCESSED.
 
              do indx = itrr, ibr
                if (ii .eq. ibrnhn(1, 4, indx)) then
                  if (jj .eq. ibrnhn(2, 4, indx)) then
                    if (kpc .eq. ibrnhc(4, indx)) then
                      ibrnhn(1, 4, indx) = 0
                      ibrnhn(2, 4, indx) = 0
                      ibrnhn(3, 4, indx) = 0
                      ibrnhc(4, indx) = ' '
                    endif
                  endif
                endif
              enddo
            else
 
C             LINE HAS BEEN FOUND IN THE BRANCH DATA TABLE, SO ADD THE
C             LINE TO THE LZ (BRANCH IMMITTANCE TABLE)
 
              lst = lst - 1
              j = lst
              ijsln(1, j) = ii
              ijsln(2, j) = jj
              ijsln(3, j) = ksect
              ijslc(j) = kpc
              jj = 2*j
              bradd(j) =  - 1000.0
              gijs(j) = gkm
              bijs(j) = bkm
              gsl(jj-1) = gk1
              bsl(jj-1) = bk1
              gsl(jj) = gk2
              bsl(jj) = bk2
            endif
          endif
        endif
  150   continue
      enddo
      return
      end
