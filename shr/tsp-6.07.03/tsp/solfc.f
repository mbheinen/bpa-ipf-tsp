C    %W% %G%
      subroutine solfc (a)                                              
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FC            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 

      dimension array(40), a(40)                                         

      equivalence          (array(1),    vrmax), (array(2),  vrmin),      
     1 (array(3),   vref), (array(4),    esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),      cke), (array(8),    cka),      
     3 (array(9),     dr), (array(10),   cfldo), (array(11),   ck5),      
     4 (array(12),    df), (array(13),     akf), (array(14),   hbf),      
     5 (array(15),   hcb), (array(16),     ckc), (array(17),    db),      
     6 (array(18),    dc), (array(19),     hbe), (array(20),    de),      
     7 (array(21),  rcex), (array(22),   vemax), (array(23),  xcex),      
     8 (array(24),   vao), (array(25),      da), (array(26),   hba),      
     9 (array(27),   vco), (array(28), cfldpst), (array(29),   efd),      
     * (array(30),   ckd), (array(31),   x7pst), (array(32), vcpst),      
     * (array(33),  vfeo), (array(34),     veo), (array(35), vemin),
     * (array(36),    a4), (array(37),      a5), (array(38),    a6)      
                                                                        
C TRANSFER CITER DATA TO ARRAY                                      
      do itr = 1,40                                                 
        array(itr) = a(itr)                                               
      end do
                                                                        
C CALCULATE VOLTAGE TRANSDUCER CIRCUIT                              
      zcd = rcex*oid - xcex*oiq                                         
      zcq = rcex*oiq + xcex*oid                                         
      vcd = zcd + vtd                                                   
      vcq = zcq + vtq                                                   
      vc = sqrt(vcd**2 + vcq**2)                                        
                                                                        
C CALCULATE FIELD CURRENT                                           
      cfld = vtq + ra * oiq + ((xd - xp) / (1.0 + satd) + xp) * oid           
      if (mgen .ge. 6) cfld = cfd                                       
                                                                        
C TEST FOR NEW TIMESTEP                                             
      if (lppwr .eq. 0) then                                            
                                                                        
C RECALCULATE STATE VECTORS FOR TIME = T                        

        ck1 = csatx * exp (esatx * abs(efd)) * (1.0 + efd * esatx)    
        ck2 = efd**2 * esatx * csatx * exp (esatx * abs(efd))         
                                                                        
        vc1 = (hbr + vco)/dr                                          
        vfe = (cke + ck1)*veo + ckd*cfldo - ck2                       
        vf = (vfe*akf + hbf)/df                                       
        vai = x7o - vf + vref - vc1                                   
        va = (dc*vai + hcb)/db                                        
        vr = (cka*va + hba)/da                                       
        if (vr.gt.vrmax) vr = vrmax
        if (vr.lt.vrmin) vr = vrmin
        x3 = vr                                                       
                                                                        
C UPDATE PAST VALUE PARAMETERS                                  
        hbr = (dr - 2.0)* vc1 +  vco
        hcb = (db - 2.0)* va  - (dc - 2.0)*vai
        hba = (da - 2.0)* vr  +  cka*va
        hbe =  de*veo + vr - vfe
        hbf = (df - 2.0)*vf - akf*vfe

C Store coefficients for estimation of VE limits                
C   VE(max) = VEMAX + CA5*CFLD                                    
        ca5   = -ckd/(de + cke + ck1)
        vemax = (hbe + ck2 + vrmax)/(de + cke + ck1)
        vemin = (hbe + ck2 + vrmin)/(de + cke + ck1)
                                                                        
C Store coefficients for solution of VE:                        
C   VE(t) = A4*VERR(t) + A4*X7(t) + A5*CFLD(t) + A6               
        a1 = cka*dc/(da*db)                                     
        a2 = 1.0 + a1*akf/df                                         
        a3 = 1.0/(de + a2*(cke+ck1))                                  
        a4 = a1*a3                                                    
        a5 = -a2*a3*ckd                                               
        a6 = (hbe + hba/da + hcb*a1/dc - hbf*a1/df + a2*ck2)*a3

c assign values to be used in sub time loop
        dtsk    = 0.1
        ndiv    = edt/dtsk

        delcfld = (cfldo - cfldpst)*dtsk/edt
        delx7   = (x7o - x7pst)*dtsk/edt
        delvc   = (vco - vcpst)*dtsk/edt

c start sub time loop
        do itr = 1, ndiv-1

	  if (idsw.ge.3.and.idsw.le.5) then
            vcn = vc
            x7n = x7
            cfldn = cfld
          else
            x7n   = x7o + itr*delx7
            vcn   = vco + itr*delvc
            cfldn = cfldo + itr*delcfld
          end if

          vc1 = (vcn + hbr)/dr                                               
          ve = a4*(vref + x7n - vc1) + a5*cfldn + a6                          

          vemaxx = vemax + ca5*cfldn                                         
          veminx = vemin + ca5*cfldn                                        
          if (veminx.lt.0.0) veminx = 0.0

          if (ve.gt.vemaxx) ve = vemaxx
          if (ve.lt.veminx) ve = veminx

          vc1 = (hbr + vcn)/dr                                          
          vfe = (cke + ck1)*ve + ckd*cfldn - ck2                       
          vf = (vfe*akf + hbf)/df                                       
          vai = x7n - vf + vref - vc1                                   
          va = (dc*vai + hcb)/db                                        
          vr = (cka*va + hba)/da                                       
          if (vr.gt.vrmax) vr = vrmax
          if (vr.lt.vrmin) vr = vrmin
            
          hbr = (dr - 2.0)* vc1 +  vcn
          hcb = (db - 2.0)* va  - (dc - 2.0)*vai
          hba = (da - 2.0)* vr  +  cka*va
          hbe =  de*ve + vr - vfe
          hbf = (df - 2.0)*vf - akf*vfe

          ca5   = -ckd/(de + cke + ck1)
          vemax = (hbe + ck2 + vrmax)/(de + cke + ck1)
          vemin = (hbe + ck2 + vrmin)/(de + cke + ck1)
                                                                        
          a6 = (hbe + hba/da + hcb*a1/dc - hbf*a1/df + a2*ck2)*a3

        end do

        cfldpst = cfldo
        x7pst   = x7o
        vcpst   = vco
        efdo    = efd
                                                                        
      endif                                                             
                                                                        
C CALCULATE STATE VECTORS FOR TIME = T + DT                         
      vc1 = (vc + hbr)/dr                                               
      ve = a4*(vref + x7 - vc1) + a5*cfld + a6                          

      vemaxx = vemax + ca5*cfld                                         
      veminx = vemin + ca5*cfld                                         
      if (veminx.lt.0.0) veminx = 0.0

      if (ve.gt.vemaxx) ve = vemaxx
      if (ve.lt.veminx) ve = veminx

      if (ve .gt. 0.0) then                                             
        cin = ckc*cfld/ve                                               
      else                                                              
        cin = 0.0                                                       
      endif                                                             
                                                                        
C Determine range of operation                                      
      if (abs(cin) .le. 0.51) then                                      
         fex = 1.0 - 0.58*cin                                           
      else if (abs(cin) .lt. 0.715) then                                
         fex = -0.865*(cin + 0.00826)**2 + 0.93233                      
      else if (abs(cin) .lt. 0.9802) then                               
         fex = 1.68 - 1.714*cin                                         
      else                                                              
         fex = 0.0                                                      
      endif                                                             

      efd = ve*fex                                                      

      vfeo = vfe                                                    
      vao = va                                                      
      veo = ve                                                          
      cfldo = cfld                                                      
      vco = vc                                                          
                                                                       
C TRANSFER EXCITER VARIABLES BACK TO CITER                          
      do itr = 1,40                                                 
        a(itr) = array(itr)                                               
      end do

      return                                                            
      end                                                               
