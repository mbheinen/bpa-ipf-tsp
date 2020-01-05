C    %W% %G%
      subroutine int3fl(a)                                              
                                                                        
c     This subroutine initializes the FL exciter

      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 

      dimension array(40),a(40)                                         

       equivalence         (array( 1), vrmax), (array(2),  vrmin),          
     1 (array( 3),  vref), (array( 4), vbmax), (array(5),    cki),          
     2 (array( 6),   hbr), (array( 7),  cckp), (array(8),    cka),          
     3 (array( 9),    dr), (array(10),   ckc), (array(11), vimax),         
     4 (array(12), vimin), (array(13),    va), (array(14), vslim),              
     5 (array(15),   hcb), (array(24),   ckg),                           
     6 (array(16),   ckj), (array(17),    db), (array(18),    dc),        
     7 (array(19), vgmax), (array(20),    xl), (array(21),  rcex),        
     8 (array(22),   vro), (array(23),  xcex), (array(25),    da),        
     9 (array(26),   hba), (array(27),   vco), (array(28), theta),        
     * (array(29),   efd), (array(30),   vbo), (array(31),  ckpr),        
     * (array(32),  ckpi), (array(33),   vio), (array(34),   bnf)          
      l6 = 6                                                            
                                                                        
C TRANSFER CITER DATA TO ARRAY                                          
      do itr=1,40                                                   
        array(itr) = a(itr)                                               
      end do
                                                                        

      efd = efdo                                                    
      vgi = efdo*ckg                                                
      if(vgi .gt. vgmax) then
        write (errbuf, 2027) name, base, id, vgi                      
        call prterr ('W',1)                                           
 2027   format(1h0, ' INITIAL VG VIOLATES VGMAX FOR ', a8, 2x, f5.1,  
     1           2x, a1, 5x, ' INITIAL VG = ', f6.3)                    
        vgi = vgmax                                                   
      end if
                                                                        
C CALCULATE VOLTAGE TRANSDUCER CKT                                   
      if(xt .le. 0.) then
        vtr = vhd                                                     
        vti = vhq                                                     
      else
        vtr = vhd + rt*agend - xt*agenq                               
        vti = vhq + rt*agenq + xt*agend                               
      end if
      crt   = agend                                                   
      cit   = agenq                                                   
      zcr   = rcex*crt - xcex*cit                                     
      zci   = rcex*cit + xcex*crt                                     
      vcr   = zcr + vtr                                               
      vci   = zci + vti                                               
      vc    = sqrt(vcr**2 + vci**2)                                    
      vco   = vc                                                      
      dr    = 2.0*dr/dt + 1.0                                          
      hbr   = (dr - 2.0)*vc + vc                                      
                                                                        
c calculate thevenin voltage
      theta = theta*3.141592/180.                                   
      ckp1  = cckp                                                   
      ckpi  = ckp1*sin(theta)                                        
      ckpr  = ckp1*cos(theta)                                        
      oir   = agend                                                   
      oii   = agenq                                                   
      ckxr  = cki + ckpr*xl                                          
      ckxi  = ckpi*xl                                                
      ckiir = ckxr*oir - ckxi*oii                                   
      ckiii = ckxr*oii + ckxi*oir                                   
      ckvr  = ckpr*vtr - ckpi*vti                                    
      ckvi  = ckpr*vti + ckpi*vtr                                    
      verr  = ckvr-ckiii                                             
      veri  = ckvi + ckiir                                           
      ve    = sqrt(verr**2 + veri**2)                                  
                                                                        
c set initial field current equal to initial field voltage
      cfd   = efdo                                                    

      if (ve.gt.0.0) then                        
        cin = abs(ckc*cfd/ve)                   
      else                                        
        cin = 0.0                                
      endif                                     

      if (cin.le.0.51) then                  
         fex = 1.0 - 0.58*cin                 
      else if (cin.lt.0.715) then          
         fex = -0.865*(cin + 0.00826)**2 + 0.93233 
      else if (cin .lt. 0.9802) then              
         fex = 1.68 - 1.714*cin                
      else                                   
         fex = 0.0                          
      endif                                

      vb = fex*ve                                                   
      if(vb .gt. vbmax) then                                     
        write(errbuf,2000) name,base,id,vb                          
 2000   format('  VB violates limits for ',      
     1         a8,2x,f5.1,2x,a1,5x,' VB = ',f6.3)
        call prterr('W',1)                                            
        vb = vbmax
      endif                                                         
      vr = efdo/vb                                                  
      x3 = vr                                                       
      da = 2.0*da/dt + 1.0                                          
      if (vr.gt.vrmax .or. vr.lt.vrmin) then
        write (errbuf, 2036) name, base, id, vr                       
        call prterr ('E',1)                                           
 2036   format(1h0, ' INITIAL VR VIOLATES LIMITS FOR ', a8,           
     1           2x, f5.1, 2x, a1, 5x, 'INIT VR = ', f6.3)              
        iabort = 1                                                    
        go to 2300                                                    
      end if
      vi   = vr/cka                                                   
      vio  = vi                                                      
      hba  = vr*(da - 2.0) + cka*vi                                  
      va   = vi + vgi                                                 
      db   = 2.0*db/dt + 1.0                                          
      dc   = (2.0*dc/dt + 1.0)*ckj                                    
      vai  = va/ckj                                                  
      hcb  = va*(db - 2.0) - vai*(dc - 2.0*ckj)                      
      if(vai.gt.vimax .or. vai.lt.vimin) then
        write (errbuf, 2040) name, base, id, vai                      
        call prterr ('E',1)                                           
 2040   format(1h0, ' INITIAL VAI VIOLATES LIMITS FOR ', a8, 2x, f5.1,
     1       2x, a1, 5x, ' INIT VAI = ', f6.3 )                     
        iabort = 1                                                    
        go to 2300                                                    
      end if
      verr = vai                                                    
      vref = verr + vc                                              
      vro  = vr                                                      
      vbo  = vb                                                      

c line to bomb out to on error condition
2300  continue
                                                                        
c tramsfer data to citer and return to inital3
      do itr = 1,40                                            
        a(itr) = array(itr)                                           
      end do

      return                                                        
      end                                                           
