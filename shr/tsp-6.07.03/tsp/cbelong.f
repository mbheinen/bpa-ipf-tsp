C    %W% %G%
      logical function cbelong (ch1,charset) 
        implicit none
        character ch1*1
        character charset*(*)
C
C     -  Determines if c1 is in charset.  
c     -  Returns true if so, false if not  
c
      if (index (charset,ch1) .gt. 0)  then 
        cbelong = .true.
      else 
        cbelong = .false.
      endif 
      return
      end
