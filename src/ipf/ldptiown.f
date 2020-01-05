C    @(#)ldptiown.f	20.2 3/29/99
C****************************************************************  
C  
C     File: ldptiown.f  
C  
C     Purpose: Routine to load PTI Owner data from raw data file   
C  
c     Return code:  n = 0 : Success  
c                   n = 1 : Error  
c  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: load_pti.f  
C  
C****************************************************************  
      integer function ldptiown (xbuf, options, numver, error)  
      integer error, options(*), numver  
      character *(*) xbuf  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/arcntl.inc'  
  
      character word(20)*10  
      logical first  
      integer ftn_atoi, add_ptio  
  
      ldptiown = 0  
      error = 0  
      last = index (xbuf, '/') - 1  
      if (last .le. 0) last = lastch(xbuf)  
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')  
  
      do i = nwrd+1, 2  
        word(i) = ' '  
      enddo  
      npti1 = ftn_atoi(word(1))  
      if (npti1 .eq. 0) then  
        ldptiown = 1  
      else  
        num = add_ptio (npti1, word(2))  
        if (num .le. 0) then  
          write (errbuf(1), 100) npti1, word(2)  
  100     format (' Owner on PTI Owner record (',   
     &       i6, 1x, a, ') could not be hashed.')  
          call prterx ('W',1)                  
          error = 1                             
          go to 900  
        endif  
      endif  
  900 continue  
      return  
      end  
