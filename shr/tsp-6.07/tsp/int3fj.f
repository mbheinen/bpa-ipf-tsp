C    %W% %G%
      subroutine int3fj(a)                                              
C                                                                       
C  THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C  FOR EXCITER TYPE FJ                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 

      dimension array(40),a(40)                                         

      equivalence           (array( 1), vrmax ), (array( 2), vrmin ),          
     1  (array( 3), vref ), (array( 4), efdmax), (array( 5), efdmin),
     2  (array( 6), hbr  ), (array( 7), hbf   ), (array( 8), cka   ),          
     3  (array( 9), dr   ), (array(10), ckc   ), (array(11), x7pst ),         
     4  (array(12), vcpst), (array(13), va    ), (array(14), vao   ), 
     5  (array(15), hcb  ), (array(24), ckf   ),                          
     6  (array(16), ckj  ), (array(17), db    ), (array(18), dc    ),        
     7  (array(19), df   ), (array(20), dk    ), (array(21), rcex  ),        
     8  (array(22), vro  ), (array(23), xcex  ), (array(25), da    ),        
     9  (array(26), hba  ), (array(27), vco   ), (array(28), theta ),        
     a  (array(29), efd  ), (array(30), a1    ), (array(31), a2    ),       
     b  (array(32), ckpi ), (array(33), slpo  ), (array(34), bnf   )

C                                                                       
C TRANSFER CITER DATA TO ARRAY                                          
C                                                                       
      do 100 itr=1,40                                                   
100   array(itr) = a(itr)                                               
                                                                        
C                                                                       
CCCC CALCULATE VOLTAGE TRANSDUCER CKT                                   
C                                                                       
 2031     if(xt .gt. 0.) go to 2033                                     
          vtr = vhd                                                     
          vti = vhq                                                     
          go to 2034                                                    
 2033     vtr = vhd + rt*agend - xt*agenq                               
          vti = vhq + rt*agenq + xt*agend                               
 2034     crt = agend                                                   
          cit = agenq                                                   
          zcr = rcex*crt - xcex*cit                                     
          zci = rcex*cit + xcex*crt                                     
          vcr = zcr + vtr                                               
          vci = zci + vti                                               
          vc = sqrt(vcr**2 + vci**2)                                    
          vco = vc                                                      
          vcpst = vco
          dtsj = 0.1                                                    
          dr = 2.0*dr/dtsj + 1.0                                        
          hbr = (dr - 2.0)*vc + vc                                      
C                                                                       
C INITIAL FIELD CURRENT,CFD, EQUALS INITIAL FIELD VOLTS,EFDO.           
C                                                                       
          cfd = efdo                                                    
          vt = sqrt(vtr*vtr + vti*vti)                                  
          emax = efdmax*vt -ckc*cfd                                     
          emin = efdmin*vt - ckc*cfd                                    
C                                                                       
CCC CK FOR LIMIT VIOLATION                                              
C                                                                       
          if(efdo .gt. emax) then                                       
          write(errbuf,2000) name,base,id,efdo                          
 2000     format(1h0,'  INITIAL FIELD VOLTS VIOLATES LIMITS FOR ',      
     1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
          call prterr('E',1)                                            
          iabort = 1                                                    
          go to 2300                                                    
          endif                                                         
          efd = efdo                                                    
           vr = efd                                                     
          if(vr .gt. vrmax) then                                        
          write(errbuf,2010) name,base,id,vr                            
 2010     format(1h0,'  INITIAL REGUL VOLTS VIOLATES LIMITS FOR ',      
     1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
          call prterr('E',1)                                            
          iabort = 1                                                    
          go to 2300                                                    
          endif                                                         
          x3 = vr                                                       
          va = vr/cka                                                   
          da = 2.0*da/dtsj + 1.0                                        
          hba = vr*(da - 1.0)                                           
          db = 2.0*db/dtsj + 1.0                                        
          dc = 2.0*dc/dtsj + 1.0                                        
          vai = va                                                      
          hcb = va*(db - 2.0) - vai*(dc - 2.0)                          
          df = 2.0*df/dtsj +1.0                                         
          dk = 2.0*ckf/dtsj                                             
          hbf = -vr*dk                                                  
          vref = vai + vc                                               
          vro = vr                                                      
          slpo = 0.0                                                    
          vao = va                                                      
C                                                                       
C TRANSFER DATA TO CITER AND RETURN TO INITAL3                          
C                                                                       
 2300     do 2400 itr = 1,40                                            
 2400     a(itr) = array(itr)                                           
          return                                                        
          end                                                           
