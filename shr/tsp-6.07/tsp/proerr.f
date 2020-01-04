C    %W% %G%
        subroutine proerr                                               
                                                                        
      include 'tspinc/prt.inc' 
      include 'tspinc/errmsg.inc' 
                                                                        
        lprtsv = lprtsw                                                 
        lcrtsw = crtsw                                                  
        lfchsv = fichsw                                                 
        lprtsw = 1                                                      
        crtsw = 0                                                       
       fichsw = 0                                                       
C                                                                       
        call forbtm                                                     
        write (outbuf,5)                                                
    5   format('**** CASE PROCESSING SUMMARY ****')                     
        call rpnlod                                                     
        call fortop                                                     
C                                                                       
        call skipln (2)                                                 
        write (outbuf,6) errcnt(1)                                      
    6   format(' THERE WERE',i4,' INFORMATION ERRORS')                  
        call prtout(1)                                                  
C                                                                       
        write (outbuf,7) errcnt(2)                                      
    7   format(11x,i4,' WARNING ERROR(S)')                              
        call prtout(1)                                                  
C                                                                       
        write (outbuf,8) errcnt(3)                                      
    8   format(11x,i4,' FATAL ERROR(S)')                                
        call prtout(1)                                                  
C                                                                       
        write (outbuf,9) errcnt(4)                                      
    9   format(8x,'AND',i4,' ABORT ERROR(S)')                           
        call prtout(1)                                                  
        call skipln (2)                                                 
C                                                                       
        crtsw=0                                                         
C                                                                       
        if (numerr .ne. 0) then                                         
           errcnt(1) = 0                                                
           errcnt(2) = 0                                                
           errcnt(3) = 0                                                
           errcnt(4) = 0                                                
C                                                                       
           write (outbuf,110)                                           
110        format('0  * * * ERROR MESSAGES ENCOUNTERED * * *')          
           call prtout(1)                                               
           call skipln (2)                                              
C                                                                       
           do 150 i23 = 1,numerr                                        
              outbuf = errm(i23)                                        
              call prtout(1)                                            
              errm(i23) = ' '                                           
150        continue                                                     
C                                                                       
           call forbtm                                                  
           call fortop                                                  
           numerr = 0                                                   
C                                                                       
        else                                                            
           write (outbuf,200)                                           
200        format('0  * * * NO ERROR MESSAGES ENCOUNTERED * * *')       
           call prtout(1)                                               
           call skipln (2)                                              
        endif                                                           
C                                                                       
        lprtsw = lprtsv                                                 
        crtsw = lcrtsw                                                  
        fichsw = lfchsv                                                 
                                                                        
        return                                                          
        end                                                             
