C    @(#)ldptiint.f	20.7 1/7/99
C**************************************************************** 
C 
C     File: ldptiint.f 
C 
C     Purpose: Routine to load PTI area transaction data from raw  
C              data file  
C 
c     Return code:  n = 0 : Success 
c                   n = 1 : Error 
c 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: load_pti.f 
C 
C**************************************************************** 
      integer function ldptiint (xbuf, options, numver, error) 
      integer error, options(*), numver 
      character *(*) xbuf 
 
      include 'ipfinc/parametr.inc' 
 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/pti_data.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/alt_case.inc' 
      include 'ipfinc/alpha.inc' 
      include 'ipfinc/area.inc' 
      include 'ipfinc/arcntl.inc' 
 
      character word(30)*10 
      logical first 
      integer fnd_ptic, ftn_atoi 
 
      ldptiint = 0 
      error = 0 
      last = index (xbuf, '/') - 1 
      if (last .le. 0) last = lastch(xbuf) 
      call ptiscan (xbuf(1:last), word, nwrd, ',',  ' ')    

      do i = nwrd+1, 4 
        word(i) = ' ' 
      enddo 
c 
c     Add defaults 
c 
      if (word(3) .eq. ' ') word(3) = '1' 
 
      npti1 = ftn_atoi(word(1)) 
      npti2 = ftn_atoi(word(2)) 
      if (npti1 .eq. 0) then 
        ldptiint = 1 
      else 
        if (ntotic .ge. 5*MAXCAR-1) then 
           write (errbuf(1), 110) 5*MAXCAR 
  110      format ('More than ',i5, 
     &       ' Area Transaction records. Overflow occurred at branch:') 
           write (errbuf(2), 120) xbuf(1:80)    
  120      format(11x, '(', a, ')')          
           call prterx ('E',2)                 
           ntotic = 1                            
           error = 1                            
           go to 900 
        endif 
        kp1 = fnd_ptic (npti1) 
        kp2 = fnd_ptic (npti2) 
        if (kp1 .le. 0) then 
           write (errbuf(1), 122) npti1, npti2 
  122      format (' PTI area1 on area transaction record (', i6, 1x,  
     &       i6, ') is not in system.') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        endif 
        if (kp2 .le. 0) then 
           write (errbuf(1), 124) npti1, npti2 
  124      format (' PTI area2 on area transaction record (', i6, 1x,  
     &       i6, ') is not in system.') 
           call prterx ('W',1)                 
           error = 1                            
           go to 900 
        endif 
        ntotic = ntotic + 1 
        arcint(1,ntotic) = pti_anam(kp1) 
        arcint(2,ntotic) = pti_anam(kp2) 
        arcinp(ntotic) = ftn_atof (word(4)) 
        ntotic = ntotic+1 
        arcint(1,ntotic) = pti_anam(kp2) 
        arcint(2,ntotic) = pti_anam(kp1) 
        arcinp(ntotic) = -ftn_atof (word(4)) 
 
      endif 
  900 continue 
      return 
      end 
