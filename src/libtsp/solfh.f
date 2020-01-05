C    %W% %G%
      subroutine solfh (a)                                              
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FH            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/prt.inc'

      dimension array(40), a(40)                                         

      equivalence          (array(1),  vamax), (array(2),  vamin),      
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),    cke), (array(8),    cka),      
     3 (array(9),     dr), (array(10),   cfldo), (array(11),   ckf),    
     4 (array(12),    df), (array(13),   vlv), (array(14),   ckn),      
     5 (array(15),   ckr), (array(16),   ckc), (array(17),    db),      
     6 (array(18),    dc), (array(19),   efdn), (array(20),    de),     
     7 (array(21),  rcex), (array(22),    dk), (array(23),  xcex),      
     8 (array(24),    vaio), (array(25),    da), (array(26),   hba),    
     9 (array(27),   vco), (array(28),     hbf), (array(29),   efd),    
     * (array(30),   ckd), (array(31),  dk1), (array(32),efdmax),       
     * (array(33),   vepo), (array(34),   hcb), (array(35),   hbe),     
     * (array(36),    dep), (array(37),bprim), (array(38),   veo),      
     * (array(39),  ckap), (array(40),  ckep)                           
                                                                        
      character gname*16

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
      cfld = vtq + ra*oiq + ((xd - xp)/(1.0 + satd) + xp)*oid           
      if (mgen .ge. 6) cfld = cfd                                       
                                                                        
                                                                        
C TEST FOR NEW TIMESTEP                                             
      if (lppwr .eq. 0) then                                            
                                                                        
C RECALCULATE STATE VECTORS FOR TIME = T                        
        efdo = efd                                                    
                                                                      
        ck1 = csatx * exp (esatx * abs(veo)) * (1.0 + veo * esatx)    
        ck2 = veo**2 * esatx * csatx * exp (esatx * abs(veo))         
        vc1 = (hbr + vco)/dr                                          
        if (dr .eq. 1.0) vc1 = vco                                    
        vf = (efdo*dk + dk1 + hbf)/df                                 
        vaii = x7o - vf + vref - vc1                                  
        vai = (dc*vaii + hcb)/db                                      
        va = (cka*vai + hba)/da                                       
        va = amax1(vamin,amin1(va,vamax))                             
        vr =va*efdo*ckr                                               
        vd = ckd*cfldo                                                
        ve = (vr -vd + hbe)/dep                                        
        slp1 = vai - vaio                                             
        vep = vr -vd -ve*(cke+ck1) +ck2                               
        slp2 = vep - vepo                                             
                                                                        
C CHANGE TIME FACTORS FOR TIME STEP CHANGE                      
        if (al .ne. 0.0) then                                         
          da = (da - 1.0)*tfac + 1.0                                 
          db = (db - 1.0)*tfac + 1.0                                 
          dc = (dc - 1.0)*tfac + 1.0                                 
          de = de*tfac                                               
          df = (df - 1.0)*tfac + 1.0                                 
          dr = (dr - 1.0)*tfac + 1.0                                 
        endif                                                         
                                                                        
C UPDATE PAST VALUE PARAMETERS                                  
        if (dr .eq. 1.0) vc1 = vc                                     
        hbr = (dr - 2.0)*vc1 + vc                                     
        hcb = (db - 2.0)*vai - (dc - 2.0)*vaii                        
        hba = (da - 2.0)*va + cka*vai                                 
        dep = de +ck1 + cke                                           
        hbe = vr -vd +ve*(dep-2.0*(ck1+cke)) +2.*ck2                  
                                                                        
C CALCULATE COEFFIECENTS FOR VN = FCONST + GCONST*EFD                   
        if(efd .lt. efdn)then                                         
          fconst = 0.0                                                  
          gconst = ckf                                                  
        else                                                          
          fconst = efdn*(ckf-ckn)                                       
          gconst = ckn                                                  
        endif                                                         
        dk = 2.0*gconst/edt                                           
        dk1 = 2.0*fconst/edt                                           
        hbf = (df - 2.0)*vf - (dk1 +dk*efd)                           
                                                                        
C SET UP NON-WINDUP LIMITS                                              
        ckap = cka                                                    
        if (va .ge.  vamax .and. slp1 .gt. 0.0) then                  
          ckap = 0.0                                                 
          hba = vamax*da                                             
        else if (va .le. vamin .and. slp1 .lt. 0.0) then              
          ckap = 0.0                                                 
          hba = vamin*da                                             
        endif                                                         
        call getfx1(fex,efd,ve,cfld,ckc)                              
        vemin = vlv/fex                                               
        if (ve .le. vemin .and. slp2 .lt. 0.0) then                   
          hbe = vd - vr                                              
        endif                                                         
        vepo = vep                                                    
        veo = ve                                                      
        vaio = vai                                                    
      endif                                                         
                                                                        
C START OF ITERATIVE PROCEDURE TO CALCULCATE EFD(T)                 
C CALCULATE APPROXIMATION EFD = ECONST*VE -DCONST                   
      cin = abs(ckc*cfld/veo)                                       
      if (cin .gt. 0.715) then                                        
        econst = 1.68                                                 
        dconst = 1.714*ckc*cfld                                       
      else if (cin .lt. 0.51) then                                         
        econst = 1.0                                                  
        dconst = 0.58*ckc*cfld                                        
      else                                                          
        econst = 0.93227 + 0.865*ckc*ckc*cfld*cfld/(veo*veo)          
        dconst = -1.730*ckc*ckc*cfld/veo -0.01429*ckc                 
      endif                                                         
                                                                        
C CALCULATE COEFFIECENTS FOR VN = FCONST + GCONST*EFD                   
      if (efd .lt. efdn)then                                         
        fconst = 0.0                                                  
        gconst = ckf                                                  
      else                                                          
        fconst = efdn*(ckf-ckn)                                       
        gconst = ckn                                                  
      endif                                                         
      dk = 2.* gconst/edt                                           
      dk1 = 2.*fconst/edt                                           
      hbfp = dk1 + hbf                                              
                                                                        
C CALCULATE REDUCED EQUATION COEFFICIENTS                          
      aconst =-(dk*dc*ckap*ckr)/(df*dep*da*db)                      
      bprim = aconst*hbfp/dk + (hcb*ckap*ckr)/(dep*da*db)           
     1          + (hba*ckr)/(dep*da)                                             
      cconst = hbe/dep - dconst/econst - ckd*cfld/dep               
                                                                        
C CALCULATE STATE VECTORS FOR TIME = T + DT                         
      vc1 = (vc + hbr)/dr                                               
      verr = vref - vc1 +x7                                             
      bconst = bprim -1.0/econst- aconst*df*verr/dk                 
      if (ckap .eq. 0.0)then                                             
        efd = -cconst/bconst                                              
      else                                                              
        xconst = bconst*bconst - 4.0*aconst*cconst
        if (xconst .lt. 0.0) then
          xconst = 0.0
          call genname (ispf, gname)                                        !dem
          write (errbuf(1),100) gname
  100     format(' Generator ', a, ' FH exciter driven negative ')
          call prterr('W', 1)
        endif
        efd = (-bconst - sqrt(xconst)) / (2.0*aconst)
      endif                                                             
                                                                        
C CALCULATE MAXIMUM AND MINIMUM VALUES OF EFD                           
      denom = 1.0/(econst-(vamax*ckr)/dep)                              
      xnum = (hbe-ckd*cfld)/dep -dconst/econst                          
      efdmax = xnum*denom                                               
      denom = 1.0/(econst -(vamin*ckr)/dep)                             
      efdmin = xnum*denom                                               
      efdmin = amax1(vlv,efdmin)                                        
      efd = amax1(efdmin,amin1(efd,efdmax))                             
                                                                        
C GET CURRENT FEX AND VE                                                
      call getfx1(fex,efd,ve,cfld,ckc)                                  
      veo = ve                                                          
      cfldo = cfld                                                      
      vco = vc                                                          
      x3 = va                                                       
                                                                        
C TRANSFER EXCITER VARIABLES BACK TO CITER                          
      do itr = 1,40                                                 
        a(itr) = array(itr)                                               
      end do

      return                                                            
      end                                                               
