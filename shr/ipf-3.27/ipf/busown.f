C    @(#)busown.f	20.3 2/13/96
      integer function busown (nb, kntown, ownlis)
 
      character * 3 ownlis(*)
 
C     This function determines if bus NB belongs to OWNLIS(*).
C
C     Input parameters:
C
C           NB        - external bus number,
C           KNTOWN    - counter of entities in OWNLIS(*).
C           OWNLIS(*) - character array of ownerships.
C
C     Output results:
C
C           BUSOWN - 0 = bus NB is not in OWNLIS(*).
C                    1 = bus NB is in OWNLIS(*).
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		kbsdta, owner, 
      include 'ipfinc/cbus.inc'
c	Global variables used:
c		bctbl_nxt, kbctbl
c
      character * 3 cbown
 
      if (kntown .eq. 0) then
         busown = 1
      else
         busown = 0
         i = 1
         do while (i .le. kntown .and. busown .eq. 0)
            if (owner(nb) .eq. ownlis(i)) then
               busown = 1
            else
               i = i + 1
            endif
         enddo
         icb = kbsdta(15,nb)
         do while (icb .gt. 0 .and. busown .eq. 0)
            call getchr(3, cbown, kbctbl(10,icb))
            i = 1
            do while (i .le. kntown .and. busown .eq. 0)
               if (cbown .eq. ownlis(i)) then
                  busown = 1
               else
                  i = i + 1
               endif
            enddo
            icb = bctbl_nxt(icb)
         enddo
      endif
      return
      end
