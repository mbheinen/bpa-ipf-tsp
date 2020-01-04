C    %W% %G%
      subroutine solfe(a)                                              
C                                                                       
C     THIS SUBROUTINE CALCULATES FIELD VOLTAGE FOR IEEE MODEL FE            
C                                                                       
      include 'tspinc/vrgov.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/prt.inc'

      dimension array(40),a(40)                                         

       equivalence         (array(1),  vrmax), (array(2),  vrmin),      
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),    cke), (array(8),    ckv),      
     3 (array(9),     dr), (array(10),   se2),                          
     4                                                                  
     5 (array(15),   hcb),                                              
     6 (array(18),  vrho), (array(19),   hbe), (array(20),    de),      
     7 (array(21),  rcex), (array(22),   vro), (array(23),  xcex),      
     8 (array(24),  vaio), (array(25),    da), (array(26),   hba),      
     9 (array(27),   vco), (array(28),   slp), (array(29),   efd),      
     *                     (array(31),efdmin), (array(32),efdmax),      
     * (array(33), ckap)                                                
                                                                        
      character gname*16                                                !dem

C     TRANSFER CITER DATA TO ARRAY                                          
      do itr = 1,40                                                 
        array(itr) = a(itr)                                               
      end do
                                                                        
C     CALCULATE VOLTAGE TRANSDUCER CIRCUIT                              
      zcd = rcex*oid - xcex*oiq                                         
      zcq = rcex*oiq + xcex*oid                                         
      vcd = zcd + vtd                                                   
      vcq = zcq + vtq                                                   
      vc = sqrt(vcd**2 + vcq**2)                                        
                                                                        
C     TEST FOR NEW TIMESTEP                                             
      if (lppwr .eq. 0) then                                            
                                                                        
C     RECALCULATE STATE VECTORS FOR TIME = T                        
        efdo = efd                                                    
        vc1 = (hbr + vco)/dr                                          
        if (dr .eq. 1.0) vc1 = vco                                    
        verr = vref - vc1                                             
        vai = amin1 (ckv,amax1(-ckv,verr))                            
        vrh = (ckap * vai + hba)/da                                     
        vrh = amin1(vrmax,amax1(vrmin,vrh))                           
        vr = efdo * de  - hbe                                          
        tempexpsatx = esatx * abs(efd)
        if (tempexpsatx .gt. 10.0) then
          tempexpsatx = 10.0
          call genname (ispf, gname)              
          write (errbuf(1),100) gname
  100     format(' Generator ', a, 
     &      ' FE exciter field voltage driven infinite ')
          call prterr('W', 1)
        endif
        ck1 = csatx * exp (tempexpsatx) * (1.0 + efd * esatx)    
        ck2 = efd**2 * esatx * csatx * exp (tempexpsatx)         
                                                                        
C       CHANGE TIME FACTORS FOR TIME STEP CHANGE                      
        if (al .ne. 0.0) then                                         
          da = da*tfac                                               
          de = (de - ck1 - cke)*tfac + ck1 + cke                     
          dr = (dr - 1.0)*tfac + 1.0                                 
        endif                                                         
                                                                        
C       UPDATE PAST VALUE PARAMETERS                                  
        if (dr .eq. 1.0) vc1 = vc                                     
        hbr = (dr - 2.0)*vc1 + vc                                     
        verr = vref - vc1                                             
        hbe = (de - 2.0*cke - 2.0*ck1)*efdo + vr + 2.0*ck2            
        ckap = (vrmax - vrmin)                                        
        hba = da*vrh + ckap*vai                                       
        slp = vai-vaio                                                
        if (vrh .ge. vrmax .and. slp .gt. 0.0) then                   
          ckap = 0.0                                                 
          hba = vrmax*da                                             
        else if (vrh .le. vrmin .and. slp .lt. 0.0) then              
          ckap = 0.0                                                 
          hba = vrmin*da                                             
        endif                                                         
        vaio = vai                                                    
      endif                                                             
                                                                        
C     CALCULATE STATE VECTORS FOR TIME = T + DT                         
      vc1 = (vc + hbr)/dr                                               
      verr = vref - vc1                                                 
      vai = amin1 (ckv,amax1(-ckv,verr))                                
      vrh = (ckap*vai + hba)/da                                         
      vrh = amin1(vrmax,amax1(vrmin,vrh))                               
      if (verr.gt.ckv) then                                             
        vr = vrmax                                                    
      else if (verr .lt. -ckv) then                                     
        vr = vrmin                                                    
      else                                                              
        vr = vrh                                                      
      endif                                                             
      efd = (vr + hbe) / de                                               
      x3 = vrh                                                          
      vco = vc                                                          
                                                                        
C     TRANSFER EXCITER VARIABLES BACK TO CITER                          
      do itr = 1,40                                                 
        a(itr) = array(itr)                                               
      end do

      return                                                            
      end                                                               
