C    %W% %G%
        subroutine mpost(rname)                                         
        character*(*) rname                                             
      include 'tspinc/prt.inc' 
        write (outbuf, 100) rname                                       
100     format(' ENTERING ROUTINE ',a)                                  
        iptemp = lprtsw                                                 
        iftemp = fichsw                                                 
        ictemp = crtsw                                                  
        lprtsw = 0                                                      
        fichsw = 0                                                      
        crtsw = 1                                                       
        call prtout (1)                                                 
        lprtsw = iptemp                                                 
        fichsw = iftemp                                                 
        crtsw = ictemp                                                  
        call prtout(1)                                                  
        return                                                          
        end                                                             
