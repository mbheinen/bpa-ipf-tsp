C    %W% %G%
      subroutine int3fd(a)                                              
C                                                                       
C  THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES     
C  FOR EXCITER TYPE FD                                                  
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/int3.inc' 
C     -  Local variables 
      dimension array(40),a(40)                                         
       equivalence        (array(1), vrmax),(array(2), vrmin),          
     1 (array(3),  vref), (array(4),efdmax),(array(5),   cki),          
     2 (array(6),   hbr), (array(7),  cckp),(array(8),   cka),          
     3 (array(9),    dr), (array(10),  ckc),(array(11), hbf),           
     4 (array(12),   df),(array(13),dk),(array(14),    de),             
     5 (array(15),  cke), (array(24),   ckpi),                          
     6 (array(16),   vao),(array(17),   vbo),(array(18),   vco),        
     7 (array(19), vino),(array(20),    xl),(array(21),  rcex),         
     8 (array(22),   xcex),(array(23),  ckpr),(array(25),    da),       
     9 (array(26),   hba),(array(27),   hbe),(array(28), theta),        
     * (array(29),   efd),(array(30),   a1),(array(31),     a2),        
     * (array(32),    a3),(array(33),   vio),(array(34),  bnf)          
      logical debug                                                     !dem
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      debug = .false.                                                   !dem
      l6 = 6                                                            
C                                                                       
C TRANSFER CITER DATA TO ARRAY                                          
C                                                                       
      do 100 itr=1,40                                                   
100   array(itr) = a(itr)                                               
                                                                        
C                                                                       
CCC CK FOR LIMIT VIOLATION                                              
C                                                                       
          if(efdo .gt. efdmax) then                                     
          write(errbuf,2000) name,base,id,efdo                          
 2000     format(1h0,'  INITIAL FIELD VOLTS VIOLATES LIMITS FOR ',      
     1 a8,2x,f5.1,2x,a1,5x,' INITIAL VOLTS = ',f6.3)                    
          call prterr('E',1)                                            
          iabort = 1                                                    
          go to 2300                                                    
          endif                                                         
          efd = efdo                                                    
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
C                                                                       
C R BLOCK                                                               
C                                                                       
          dr = 2.0*dr/dt + 1.0                                          
          hbr = (dr - 2.0)*vc + vc                                      
C                                                                       
CCC CALCULATE THEVENIN VOLTS                                            
C                                                                       
          oir = agend                                                   
          oii = agenq                                                   
          ckiir = cki*oir                                               
          ckiii = cki*oii                                               
          ckvr = cckp*vtr                                               
          ckvi = cckp*vti                                               
          verr = ckvr-ckiii                                             
          veri = ckvi + ckiir                                           
          ve = sqrt(verr**2 + veri**2)                                  
C                                                                       
C INITIAL FIELD CURRENT,CFD, EQUALS INITIAL FIELD VOLTS,EFDO.           
C                                                                       
          cfd = efdo                                                    
          cin = ckc*cfd/ve                                              
          if(cin .le. 0.51) fex = 1.0 - 0.58*cin                        
          if(0.51 .lt. cin .and. cin .lt. 0.715)                        
     1    fex = -0.865*(cin + 0.00826)**2 + 0.93233                     
          if(cin .ge. 0.715)fex = 1.68 - 1.714*cin                      
          if(fex .lt. 0.0) fex = 0.0                                    
          vb = fex*ve                                                   
          vbo = vb                                                      
C                                                                       
C E BLOCK                                                               
C                                                                       
          de = 2.*de/dt                                                 
          hbe = efdo*de                                                 
C                                                                       
C A BLOCK                                                               
C                                                                       
          vr = cke*efdo-vb                                              
          x3 = vr                                                       
          da = 2.0*da/dt + 1.0                                          
          va = vr/cka                                                   
          vao = va                                                      
          hba = vr*(da - 2.0) + cka*va                                  
          if(vr .lt. vrmax .and. vr .gt. vrmin) go to 2038              
          write (errbuf, 2036) name, base, id, vr                       
          call prterr ('E',1)                                           
 2036     format(1h0, ' INITIAL VR VIOLATES LIMITS FOR ', a8,           
     1           2x, f5.1, 2x, a1, 5x, 'INIT VR = ', f6.3)              
          iabort = 1                                                    
          go to 2300                                                    
 2038     vref = va + vc                                                
C                                                                       
C F BLOCK                                                               
C                                                                       
      vf = 0.0                                                          
      dk = 2.*dk/dt                                                     
      df = 2.*df/dt + 1.                                                
      hbf = -dk*efdo                                                    
      vino = vr + vb - cke*efdo                                         
c     -
      if (debug) then                                                   !dem
        call dbgeko ('INT3FD - at end')                                 !dem
        call dbgwrf ('  VC /Vterm xducer/ = ',vc)                       !dem
        call dbgwrf ('  VE /lin drop comp/ = ',ve)                      !dem
        call dbgwrf ('  EFDO /field volts/ = ',efdo)                    !dem
        call dbgwrf ('  CFD /field curr/ = ',cfd)                       !dem
        call dbgwrf ('  FEX /V_e multplr/ = ',fex)                      !dem
        call dbgwrf ('  FEX /V_e multplr/ = ',fex)                      !dem
        call dbgwrf ('  VB /source blk out/ = ',vb)                     !dem
        call dbgwrf ('  VF /regultr feedbk/ = ',vf)                     !dem
        call dbgwrf ('  VA /regultr input/ = ',va)                      !dem
        call dbgwrf ('  VR /regultr output/ = ',vr)                     !dem
        rvef = cke * efdo                                               !dem
        call dbgwrf ('  RVEF /exciter feedbk/ = ',rvef)                 !dem
        call dbgwrf ('  VIN /exc integrtr in/ = ',vino)                 !dem
      endif                                                             !dem
C                                                                       
C TRANSFER DATA TO CITER AND RETURN TO INITAL3                          
C                                                                       
 2300     do 2400 itr = 1,40                                            
 2400     a(itr) = array(itr)                                           
          return                                                        
          end                                                           
