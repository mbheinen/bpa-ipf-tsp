C    %W% %G%
        function kompr(word1,word2,fvalue)                              
C                                                                       
C       FUNCTION TO CHECK THE ASCII SORT ORDER OF WORD1                 
C       AND WORD2. MAY ALSO BE CALLED AS A SUBROUTINE.                  
C                                                                       
C       0 RETURNED  WHEN EQUAL                                          
C       -1 RETURNED WHEN THEY ARE IN ASCENDING ORDER                    
C       +1 RETURNED WHEN THEY ARE IN DESCENDING ORDER                   
C                                                                       
C       EXAMPLE: (BOTH FUNCTION CALL AND SUBROUTINE CALL)               
C                                                                       
C          NSRT=KOMPR(WORD1,WORD2,DUMMY)                                
C          CALL KOMPR(WORD1,WORD2,NSRT)                                 
C                                                                       
        integer fvalue                                                  
        character word1*(*), word2*(*)                                  
C                                                                       
        if  (word1.lt.word2) then                                       
           fvalue = -1                                                  
        else if (word1.gt.word2) then                                   
           fvalue = +1                                                  
        else                                                            
           fvalue = 0                                                   
        endif                                                           
C                                                                       
        kompr=fvalue                                                    
C                                                                       
        return                                                          
        end                                                             
