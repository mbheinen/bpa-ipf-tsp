C    %W% %G%
      subroutine solfj(a)                                             
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FJ            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/param.inc' 
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

c Transfer CITER data to array                                          
      do itr=1,40                                                   
        array(itr) = a(itr)                                               
      end do

c calculate voltage tranducer circuit
      vcd     =  rcex*oid - xcex*oiq + vtd
      vcq     =  rcex*oiq + xcex*oid + vtq
      vc      =  sqrt(vcd**2 + vcq**2)                                    
      vt      =  sqrt(vtd*vtd + vtq*vtq)                                  

c calculate field current (if mgen >= 6 use field current from cntrl)
      cfld = vtq + ra*oiq + ((xd - xp)/(1.0 + satd) + xp)*oid
      if (mgen.ge.6) cfld = cfd 

      if (lppwr.eq.0) then
c calculate internal states in the closed loop to get next H values
        vc1o  = (hbr + vco)/dr                                              
        vfo   = (vro*dk + hbf)/df
        vai   =  x7o - vfo + vref - vc1o
        vao   = (vai*dc + hcb)/db
        hbr   =  vc1o*(dr-2.) + vco
        hcb   =  vao *(db-2.) - vai *(dc-2.)
        hbf   =  vfo *(df-2.) - vro *dk
        hba   =  vro *(da-2.) + vao *cka
        a1    = (cka*dc*df)/(da*db*df+cka*dc*dk)
        a2    = (cka*df*hcb+hba*db*df-cka*hbf*dc)/(da*db*df+cka*dc*dk)

c assign values to be used in sub-time loop
        dtsk    =  0.1                                                        
        ndiv    =  edt/dtsk                                            
        delx7   =  (x7o - x7pst)*dtsk/edt
        delvc   =  (vco - vcpst)*dtsk/edt

c start sub-timestep loop
        do itr  =  1,ndiv-1                                              

c extrapolate input from previous values
          if (idsw.ge.3.and.idsw.le.5) then
            x7n = x7
            vcn = vc
          else
            x7n   =  x7o + itr*delx7
            vcn   =  vco + itr*delvc
          end if

c calculate output
          vc1o  = (hbr + vcn)/dr                                              
          vro   = a1*(x7n+vref-vc1o) + a2  
          if (vro .gt. vrmax) vro = vrmax  
          if (vro .lt. vrmin) vro = vrmin                                        

c calculate internal states in the closed loop to get next H values
          vfo   = (vro*dk + hbf)/df
          vai   =  x7n - vfo + vref - vc1o
          vao   = (vai*dc + hcb)/db
          hbr   =  vc1o*(dr-2.) + vcn
          hcb   =  vao *(db-2.) - vai *(dc-2.)
          hbf   =  vfo *(df-2.) - vro *dk
          hba   =  vro *(da-2.) + vao *cka
          a2    = (cka*df*hcb+hba*db*df-cka*hbf*dc)/(da*db*df+cka*dc*dk)
        end do
        efdo  =  efd                                                    
        x7pst =  x7o
        vcpst =  vco
        vco   =  vc
      end if

c calculate limits on EFD
      emax    =  efdmax*vt - cfld*ckc                                     
      emin    =  efdmin*vt - cfld*ckc                                     

c calculate present output
      vc1o  = (hbr + vc)/dr                                              
      vro   = a1*(x7+vref-vc1o) + a2  
      if (vro .gt. vrmax) vro = vrmax                                        
      if (vro .lt. vrmin) vro = vrmin                                        

c assign and limit output
      efd     =  vro                                                      
      if(efd .gt. emax) efd = emax                                  
      if(efd .lt. emin) efd = emin                                  

c transfer data back to citer
      do itr=1,40                                                   
        a(itr) = array(itr)                                               
      end do

      return                                                        
      end                                                              
