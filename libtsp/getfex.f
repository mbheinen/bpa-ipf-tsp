C    %W% %G%
        subroutine getfex (fex, efd, ve, ifld, kc)                          
        real fex, efd, ve, ifld, kc
C                                                                       
C       This subroutine calculates the initial VE using initial         
C       values EFD, IFLD, and KC.                                       
C                                                                       
        include 'tspinc/prt.inc' 
        include 'tspinc/int3.inc' 

        real in

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
        do itr = 1,10                                               
          if (ve .gt. 0.0) then                                           
            in = kc*ifld/ve                                              
          else                                                            
            in = 0.0                                                     
          endif                                                           
C                                                                       
C         Determine range of operation                                    
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
C            ERROR - ILLEGAL OPERATING RANGE                              
C                                                                       
             write (errbuf(1),30) name,base,id,in,kc,ifld,ve              
   30        format('0 ILLEGAL EXCITER OPERATING RANGE ',a8,2x,f5.1,2x,
     &          a1, ' IN ',f9.6,' KC ',f9.6,' IFLD ',f9.6,' VE ',f9.6)         
             call prterr('E',1)                                           
             imchn = 3                                                    
             return                                                       
          endif                                                           
          efdx = ve*fex                                                   
C                                                                       
C         CHECK CONVERGENCE                                               
C                                                                       
          if (abs (efd-efdx) .lt. 1.0e-05) return                         
C                                                                       
C         SOLVE FOR NEW VE                                                
C                                                                       
          c4 = c3 - c1*(ifld/ve)**2                                       
          c5 = 2.0*c1*ifld/ve*2 + c2                                      
          c6 = efdx - c4*ve - c5*ifld                                     
          ve = (efd - c5*ifld - c6)/c4                                    
          if (ve .lt. 0.0) then                                           
C                                                                       
C           ERROR - ILLEGAL OPERATING RANGE                              
C                                                                       
            write (errbuf(1),90) name,base,id,in,kc,ifld,ve              
   90       format('0 CAUTION - NEGATIVE EXCITER VOLTAGE ',a8,2x,f5.1,   
     1        2x,a1,' IN ',f9.6,' KC ',f9.6,' IFLD ',f9.6,' VE ',f9.6)   
            call prterr('E',1)                                           
         endif                                                           
                                                                        
        enddo
C                                                                       
C       ERROR - CANNOT BE PROPERLY INITIALIZED                          
C                                                                       
        write (errbuf(1),102) name,base,id,kc,ifld                      
 102    format('0',2x,a8,2x,f5.1,2x,a1,5x,'VE cannot be properly initial
     1ized. Check coefficient KC ',f9.6,' and field current ',f9.6)     
        call prterr('E',1)                                              
        imchn = 3                                                       
        return                                                          
        end                                                             
