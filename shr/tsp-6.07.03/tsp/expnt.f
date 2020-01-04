C    %W% %G%
        subroutine expnt (number,field,len)                             
C * * *                                                                 
C * * * THIS SUBROUTINE WRITES A NUMBER USING A VARIABLY FORMATED       
C * * * E FIELD DESCRIPTOR.  IT IS CALLED BY VARFMT.                    
C * * *                                                                 
        real number                                                     
        character*(*) field                                             
        character*8 f900                                                
        integer len, decml                                              
        lexp = 7                                                        
C        THIS NUMBER INCLUDES A REQUIRED LEADING BLANK (1),             
C        THE ONES PLACE (1),                                            
C        THE DECIMAL POINT (1),                                         
C        AT LEAST ONE DECIMAL (1),                                      
C        THE EXPONENT (4),                                              
C        AND THE MINUS SIGN (IF NEGATIVE)                               
                                                                        
        if (number.lt.0) lexp = 8                                       
        write(f900,900) len, len-lexp+1                                 
 900    format('(E', i2,'.',i2,')')                                     
        write(field,f900) number                                        
        return                                                          
        end                                                             
                                                                        
