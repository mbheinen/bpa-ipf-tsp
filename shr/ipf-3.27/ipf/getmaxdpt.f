C    @(#)getmaxdpt.f	20.1 2/13/96
C****************************************************************
C
C   File: getmaxdtp.f
C
C   Purpose: Routine to retrieve the MAXNUM entities from DPT(KEY,*).
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: get_orpt22.f
C
C****************************************************************
C
	subroutine getmaxdpt (dpt, key, maxnum, nt, maxsen, senmax)
        double precision dpt(2,*)
        integer key, maxnum, nt, maxsen(*)
        real senmax(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/ecvar.inc'

        nt = 0
        dplo = 1.0e+20
c
c       Pass 1 - check LTC constraints
c
        do k = 1, ntota
          dp = dpt(key,k)
          if (dp .ne. 0.0) then
            ityp = mod (ltran(10,k),100)
            if (key .eq. 1 .and. ityp .ne. 3) then
            else if (key .ne. 1 .and. ityp .eq. 3) then
            else 
              if (nt .lt. maxnum) then
                nt = nt + 1
                if ((nt .eq. 1) .or. (abs(dp) .le. dplo)) then
                  dplo = abs(dp)
                  ntlo = nt
                endif
                maxsen(nt) = k
                senmax(nt) = dp
              else
                if (abs(dp) .gt. dplo) then
                  maxsen(ntlo) = k
                  senmax(ntlo) = dp
                  dplo = abs(dp)
                  do j = 1,maxnum
                    if (abs(senmax(j)) .le. dplo) then
                      dplo = abs(senmax(j))
                      ntlo = j
                    endif
                  enddo
                endif
              endif
            endif
          endif
        enddo
c
c       Pass 2 - check bus constraints
c
        do k = ntota+1,ntotx-1
          dp = dpt(key,k)
          if (dp .ne. 0.0) then
            if (nt .lt. maxnum) then
              nt = nt + 1
              if ((nt .eq. 1) .or. (abs(dp) .le. dplo) ) then
                dplo = abs(dp)
                ntlo = nt
              endif
              maxsen(nt) = k
              senmax(nt) = dp
            else
              if (abs(dp) .gt. dplo) then
                maxsen(ntlo) = k
                senmax(ntlo) = dp
                dplo = abs(dp)
                do j = 1,maxnum
                  if (abs(senmax(j)) .le. dplo) then
                    dplo = abs(senmax(j))
                    ntlo = j
                  endif
                enddo
              endif
            endif
          endif
        enddo
        return
        end
