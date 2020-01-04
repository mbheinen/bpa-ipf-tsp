C    %W% %G%
      subroutine upcase (instr)                                         
C     -    Utility subroutine to force all letters (if no more than 133 
C     -      characters) of an input string to upper case.              
C     -    In the SWING program, this is called by TAPEWK just before   
C     -      comparing PF names from the .SWI and .BSE files.           
      character*(*) instr                                               
      integer strlen                                                    
      character*1 thisch                                                
C     -                                                                 
      strlen = min (len (instr),133)                                    
      do 123  la = 1,strlen                                             
         thisch = instr(la:la)                                          
         if ( 'a' .le. thisch .and. thisch .le. 'z') then               
            thisch = char (ichar (thisch) - 32)                         
            instr (la:la) = thisch                                      
         endif                                                          
 123  continue                                                          
      return                                                            
      end                                                               
