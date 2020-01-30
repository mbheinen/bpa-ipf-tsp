C    %W% %G%
        subroutine fxdec (number,field,len)                             
C * * *                                                                 
C * * * THIS SUBROUTINE WRITES A NUMBER USING A VARIABLY FORMATED       
C * * * F FIELD DESCRIPTOR.  IT IS CALLED BY VARFMT.                    
C * * *                                                                 
        real number                                                     
        character*(*) field                                             
        integer len, decml                                              
        character*8 f950                                                
        pnum = abs(number)                                              
        decml = len - 3                                                 
        if (pnum.ge.1.0) decml = decml - log10(pnum)                    
        if (number.lt.0.0) decml = decml - 1                            
        write(f950,950) len,decml                                       
 950    format('(F',i2,'.',i2,')')                                      
        write(field,f950) number                                        
        return                                                          
        end                                                             
