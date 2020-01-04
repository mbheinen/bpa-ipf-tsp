C    %W% %G%
       subroutine readid                                                
C * * *                                                                 
C * * * THIS SUBROUTINE USES THE INQUIRE COMMAND TO OBTAIN THE FILE NAME
C * * * TO THE INPUT FILE, FOR005.  THIS NAME IS THEN USED AS THE JOB   
C * * * IDENTIFIER WHICH IS CALLED SCASE. IT IS CALLED BY SWINGM.       
C * * *                                                                 
      include 'tspinc/blkcom1.inc' 
      character*50 table                                                
C     -                                                                 
C     -                    Begin                                        
C     -    Set swing case name length and fetch full switch deck file   
C     -    name.                                                        
      lcasfl = 10                                                       
       inquire(unit=5,name =table)                                      
C     -    The next character after the directory's closing bracket is  
C     -    the start of the case name.                                  
      do 100 i=1,50                                                     
       if(table(i:i) .eq. ']') go to 150                                
 100   continue                                                         
       i=0                                                              
 150         ind1 = i+1                                                 
       ind2 = ind1 + lcasfl - 1                                         
C      -   Swing case name SCASE is LCASFL characters long at most.  If 
C      -   the case name is shorter, the dot before the file extension  
C      -   indicates the end of the case name.                          
       scase = table(ind1:ind2)                                         
       lpdot = index(scase,'.')                                         
       if (lpdot .gt. 1) scase = scase(1:lpdot-1)                       
       return                                                           
       end                                                              
