C    %W% %G%
        subroutine getfx1 (fex,efd,ve,ifld,kc)                          
C                                                                       
C       This subroutine calculates the  VE using                        
C       values EFD, IFLD, and KC. IT IS CALLED BY SOLFH                 
C                                                                       
        include 'tspinc/params.inc' 
        include 'tspinc/prt.inc' 
        include 'tspinc/igentn.inc' 
        include 'tspinc/bname.inc' 
        include 'tspinc/buskv.inc' 
        include 'tspinc/lnk1a.inc' 

        character*8 name                                                  
        character*1 id                                                    
        real ifld,kc,in                                                 

        l6 = 6                                                          
C                                                                       
C       If KC is zero, the derivation is trival                         
C                                                                       
        if (kc .eq. 0.0) then                                           
           ve = amax1(efd,0.0)                                          
           fex = 1.0                                                    
           efdx = fex*ve                                                
           return                                                       
        endif                                                           
C                                                                       
C       Find initial balance (start with IN in midpoint range)          
C                                                                       
        in = 0.5*(0.51 + 0.715)                                         
        ve = abs(kc*ifld/in)                                            
C                                                                       
C       Begin iteration                                                 
C                                                                       
        do 100 itr = 1,10                                               
        if (ve .gt. 0.0) then                                           
           in = kc*ifld/ve                                              
        else                                                            
           in = 0.0                                                     
        endif                                                           
C                                                                       
C       Determine range of operation                                    
C                                                                       
        if (abs(in) .le. 0.51) then                                     
           c1 = 0.0                                                     
           c2 = -0.58*kc                                                
           c3 = 1.0                                                     
           fex = 1.0 - 0.58*abs(in)                                     
        else if (abs(in) .lt. 0.715) then                               
           c1 = -0.865*kc**2                                            
           c2 = -0.01429*kc                                             
           c3 = 0.93227                                                 
           fex = -0.865*(abs(in) + 0.00826)**2 + 0.93233                
        else if (abs(in) .lt. 0.9802) then                              
           c1 = 0.0                                                     
           c2 = -1.714*kc                                               
           c3 = 1.68                                                    
           fex = 1.68 - 1.714*abs(in)                                   
        else                                                            
           c1 = 0.0                                                     
           c2 = 0.0                                                     
           c3 = 0.0                                                     
           fex = 0.0                                                    
           ve = amax1 (efd,0.0)                                         
C                                                                       
C          ERROR - ILLEGAL OPERATING RANGE                              
C                                                                       
           call mpost('GETFX1')                                         
           i1 = igentn(1,ispf)                                          
           id = igentc(ispf)                                            
           name = bname(i1)                                             
           bkv = buskv(i1)                                              
           write (errbuf(1),30) name,bkv,id,in,kc,ifld,ve               
   30      format('0 ILLEGAL EXCITER OPERAING RANGE ',a8,2x,f5.1,2x,a1, 
     1       ' IN ',f9.6,' KC ',f9.6,' IFLD ',f9.6,' VE ',f9.6)         
           call prterr('W',1)                                           
           return                                                       
        endif                                                           
        efdx = ve*fex                                                   
C                                                                       
C       Check convergence                                               
C                                                                       
        if (abs (efd-efdx) .lt. 1.0e-05) return                         
C                                                                       
C       Solve for new VE                                                
C                                                                       
        c4 = c3 - c1*(ifld/ve)**2                                       
        c5 = 2.0*c1*ifld/ve*2 + c2                                      
        c6 = efdx - c4*ve - c5*ifld                                     
        ve = (efd - c5*ifld - c6)/c4                                    
        if (ve .lt. 0.0) then                                           
C                                                                       
C          Error - illegal operating range                              
C                                                                       
           call mpost('GETFX1')                                         
           i1 = igentn(1,ispf)                                          
           id = igentc(ispf)                                            
           name = bname(i1)                                             
           bkv = buskv(i1)                                              
           write (errbuf(1),90) name,bkv,id,in,kc,ifld,ve               
   90      format('0 CAUTION - NEGATIVE EXCITER VOLTAGE ',a8,2x,f5.1,   
     1       2x,a1,' IN ',f9.6,' KC ',f9.6,' IFLD ',f9.6,' VE ',f9.6)   
           call prterr('W',1)                                           
        endif                                                           
                                                                        
  100   continue                                                        
C                                                                       
C       Error - cannot be properly initialized                          
C                                                                       
        call mpost('GETFX1')                                            
        i1 = igentn(1,ispf)                                             
        id = igentc(ispf)                                               
        name = bname(i1)                                                
        bkv = buskv(i1)                                                 
        write (errbuf(1),102) name,bkv,id,kc,ifld                       
 102    format('0', 2x,a8,2x,f5.1,2x,a1,5x,'VE cannot be properly ',     
     1  'solved. Check coefficient KC ',f9.6,' and field current ', 
     2   f9.6)                                                          
        call prterr('E',1)                                              
        call erexit                                                     
        return                                                          
        end                                                             
