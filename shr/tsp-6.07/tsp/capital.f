C    %W% %G%
      character*(*) function capital (string)
      character*(*) string
c     
c     PURPOSE:                                              
c       "CAPITAL" Converts any lower-case alphabetic    
c       characters in a string to UPPER case.           
c                                                               
c     PARAMETER DESCRIPTIONS:                               
c       string =  The incoming character string which   
c                 may need conversion.                  
c                                                               
c       capital = the outgoing character string         
c                 converted from string.                
c                                                               
c     DEVELOPPED BY:                                        
c       Section EOHB                                    
c       Methods Development Section                     
c       Bonneville Power Administration                 
c                                                               
      integer i, chr, nchr, offset, lca, lcz

      capital = string
 
c     Process each character in the string

      offset = ichar ('a') - ichar('A')
      lca = ichar ('a')
      lcz = ichar ('z')

c     Find out how long the STRING is

      nchr = len(string)
      do i = 1, nchr
         chr = ichar(string(i:i))
         if (chr .ge. lca .and. chr .le. lcz) then
            capital(i:i) = char(chr-offset)
         endif
      enddo

      return
      end
