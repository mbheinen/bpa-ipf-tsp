C    @(#)ldptizon.f	20.9 3/29/99
C****************************************************************  
C  
C     File: ldptizon.f  
C  
C     Purpose: Routine to load PTI LTC Zone data from raw data file   
C  
c     Return code:  n = 0 : Success  
c                   n = 1 : Error  
c  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: load_pti.f  
C  
C****************************************************************  
      integer function ldptizon (xbuf, options, numver, error)  
      integer error, options(*), numver  
      character *(*) xbuf  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/arcntl.inc'  
  
      character word(20)*10, zonename*2  
      logical first, finished  
      integer ftn_atoi, ptihashz, fnd_ptiy, status  
  
      ldptizon = 0  
      error = 0  
      last = index (xbuf, '/') - 1  
      if (last .le. 0) last = lastch(xbuf)  
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')  
  
      do i = nwrd+1, 2  
        word(i) = ' '  
      enddo  
      npti1 = ftn_atoi(word(1))  
      if (npti1 .eq. 0) then  
        ldptizon = 1  
      else  
c 
c       Generate unique zonename if necessary 
c 
        zonename = word(2) 
        status = fnd_ptiy (zonename) 
        finished = (status .eq. 0) 
        next = 48 
        do while (next .le. 90 .and. .not. finished) 
           zonename = zonename(1:1) // char(next) 
           status = fnd_ptiy (zonename)  
           if (status .eq. 0) then 
              finished = .true. 
           else if (next .eq. 57) then 
              next = 35 
           else if (next .ge. 35 .and. next .le. 37) then 
              next = next + 1 
           else if (next .eq. 38) then 
              next = 42 
           else if (next .eq. 42) then 
              next = 43 
           else if (next .eq. 43) then 
              next = 45 
           else if (next .eq. 45) then 
              next = 46 
           else if (next .eq. 46) then 
              next = 65 
           else 
              next = next + 1 
           endif 
        enddo 
        if (zonename .ne. word(2)(1:2)) then  
          write (errbuf(1), 10000) npti1, word(2), zonename  
10000     format (' Duplicate zone name (1:2) on PTI Zone record (',   
     &       i6, 1x, a, ') renamed to (', a, ')')  
          call prterx ('W',1)                  
        endif  
  
        num = ptihashz (npti1, zonename)  
        if (num .le. 0) then  
          write (errbuf(1), 10010) npti1, zonename  
10010     format (' Zone on PTI Zone record (',   
     &       i6, 1x, a, ') could not be hashed.')  
          call prterx ('W',1)                  
          error = 1                             
          go to 900  
        endif  
      endif  
  900 continue  
      return  
      end  
