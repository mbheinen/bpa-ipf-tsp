C    %W% %G%
      subroutine int3fk(a)                                              
C                                                                       
C     This subroutine calculates initial state vectors and past values     
c     for exciter type FK                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 
      include 'tspinc/spare1.inc' 

      dimension array(40), a(40)                                         
      equivalence           (array( 1), vrmax ), (array( 2), vrmin ),          
     1  (array( 3), vref ), (array( 4), efdmax), (array( 5), efdmin),
     2  (array( 6), hbr  ), (array( 7), hbf   ), (array( 8), cka   ),          
     3  (array( 9), dr   ), (array(10), ckc   ), (array(11), vimax ),         
     4  (array(12), vimin), (array(13), x7pst ), (array(14), vcpst ), 
     5  (array(15), hcb  ), (array(24), ckf   ),                          
     6  (array(16), ckj  ), (array(17), db    ), (array(18), dc    ),        
     7  (array(19), df   ), (array(20), dk    ), (array(21), rcex  ),        
     8  (array(22), vro  ), (array(23), xcex  ), (array(25), da    ),        
     9  (array(26), hba  ), (array(27), vco   ), (array(28), theta ),        
     a  (array(29), efd  ), (array(30), a1    ), (array(31), a2    ),       
     b  (array(32), ckpi ), (array(33), slpo  ), (array(34), bnf   )

      l6 = 6                                                            
C                                                                       
C     Transfer CITER data to ARRAY                                          
C                                                                       
      do 100 itr=1,40                                                   
100   array(itr) = a(itr)                                               
C                                                                       
C     Calculate voltage transducer ckt                                   
C                                                                       
 2031 if (xt .le. 0.0) then
          vtr = vhd                                                     
          vti = vhq                                                     
      else
          vtr = vhd + rt*agend - xt*agenq                               
          vti = vhq + rt*agenq + xt*agend                               
      endif
      crt = agend                                                   
      cit = agenq                                                   
      zcr = rcex*crt - xcex*cit                                     
      zci = rcex*cit + xcex*crt                                     
      vcr = zcr + vtr                                               
      vci = zci + vti                                               
      vc = sqrt(vcr**2 + vci**2)                                    
      vco = vc                                                      

c *** csw addition 10/93
      vcpst = vco
c *** csw end addition

      dtsk = 0.1                                                    
      dr = 2.0*dr/dtsk + 1.0                                        
      hbr = (dr - 2.0)*vc + vc                                      
C                                                                       
C     Initial field current, cfd, equals initial field volts, EFDO.           
C                                                                       
      cfd = efdo                                                    
      vt = sqrt(vtr*vtr + vti*vti)                                  
      emax = vrmax*vt -ckc*cfd                                      
      emin = vrmin*vt - ckc*cfd                                     
C                                                                       
C     Ck for limit violation                                              
C                                                                       
      if (efdo .gt. emax) then                                       
         write(errbuf,2000) name,base,id,efdo                          
 2000    format(1h0,'  Initial field volts violates limits for ',      
     1     a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
         call prterr('E',1)                                            
         iabort = 1                                                    
         go to 2300                                                    
      endif                                                         
      efd = efdo                                                    
      vr = efd                                                     
      x3 = vr                                                       
      va = vr/cka                                                   
      da = 2.0*da/dtsk + 1.0                                        
      hba = vr*(da - 1.0)                                           
      db = 2.0*db/dtsk + 1.0                                        
      dc = 2.0*dc/dtsk + 1.0                                        
      vai = va                                                      
      if (vai .gt. vimax) then                                       
         write(errbuf,2000) name,base,id,vai                           
 2100    format(1h0,'  INITIAL INTERNAL VOLTS VIOLATES LIMITS FOR ',   
     1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
         call prterr('E',1)                                            
         iabort = 1                                                    
         go to 2300                                                    
      endif                                                         
      hcb = va*(db - 2.0) - vai*(dc - 2.0)                          
      df = 2.0*df/dtsk +1.0                                         
      dk = 2.0*ckf/dtsk                                             
      hbf = -vr*dk                                                  
      vref = vai + vc                                               
      vro = vr                                                      
C                                                                       
C     Transfer data to CITER and return to INITAL3                          
C                                                                       
 2300 do 2400 itr = 1,40                                            
 2400 a(itr) = array(itr)                                           

      return                                                        
      end                                                           
