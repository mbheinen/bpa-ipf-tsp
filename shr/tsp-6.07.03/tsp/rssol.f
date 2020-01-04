C    %W% %G%
      subroutine rssol                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONTAINS THE SOLUTION LOGIC FOR THE CF-1        
C * * * UNDERFREQUENCY LOAD SHEDDING LOGIC.  IT IS CALLED BY RELAY.     
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/rscom.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/param.inc' 
      common/relay3/ldbus,pshed1,qshed1,pshed2,qshed2,                  
     1              pshed3,qshed3,pshed4,qshed4,ldmde                   
      dimension dropld(8)                                               
      equivalence (dropld(1),pshed1)                                    
      include 'tspinc/ldropn.inc' 
      do 90 k = 1,nufreq                                                
C * * *                                                                 
C IF RELAY TRIPPED EXIT                                                 
C * * *                                                                 
      if (irstcd(k) .eq. 2 ) go to 90                                   
      ldbus = irsbno(k)                                                 
C * * *                                                                 
C CHECK IF BKR TIMER ACTIVATED TO TRIP                                  
C * * *                                                                 
   10 if(rsbtmr(k) .eq. 0.0) go to 50                                   
      if(rsbtmr(k)-.0001 .gt. to)go to 90                               
C * * *                                                                 
C * * * INITIATE TO TRIP                                                
C * * * SELECT POWER UNIT CODING AND INITIAL  BREAKER TIMER             
C * * *                                                                 
   20 ldmde=1                                                           
      rsbtmr(k) = 0.0                                                   
      irstcd(k) = 2                                                     
      ivpc=2                                                            
      if(ldbus.lt.lfrst)lfrst=ldbus                                     
      name1 = bname(ldbus)                                              
      bkv1 = buskv(ldbus)                                               
      write (outbuf, 30)name1, bkv1, to, (rsldrp(i,k), i = 1,8)         
      call prtout (1)                                                   
   30 format(1h0, a8, 1x, f5.1, 'KV', f6.0, 'CYC CF1 DROP',             
     1           f8.2, 'PZ', f8.2, 'QZ', f8.2, 'PI', f8.2, 'QI',        
     2           f8.2, 'PP', f8.2, 'QP', f8.2, 'PF', f8.2, 'QF')        
      ii=0                                                              
      do 40 i = 1,8                                                     
      ii=ii+1                                                           
   40 dropld(ii) = rsldrp(i,k)                                          
      call loadrp                                                       
      go to 90                                                          
   50 if(idsw.ne.3.and.idsw.ne.5) go to 60                              
C * * *                                                                 
C * * * STORE VOLTAGE AND BYPASS                                        
C * * *                                                                 
      rseyro(k) = eyr(ldbus)                                            
      rseyio(k) = eyi(ldbus)                                            
      go to 90                                                          
C * * *                                                                 
C * * * SOLVE FOR BUS FREQ                                              
C * * *                                                                 
   60 enew = eyr(ldbus)                                                 
      fnew = eyi(ldbus)                                                 
      efsq=enew*enew+fnew*fnew                                          
      if(efsq.ne.0.0)go to 70                                           
      wnew = rsfeqo(k)                                                  
      go to 80                                                          
   70 vm=sqrt(efsq)                                                     
C * * *                                                                 
C * * * PRECESS VARIABLE FIRST BEFORE PROCESSING                        
C * * *                                                                 
      rsx1o(k) = rsx1n(k)                                               
      rsx2o(k) = rsx2n(k)                                               
      wnew = (fnew*rseyro(k) - enew*rseyio(k))/(efsq*edt)               
   80 x1n = (rsfeq1(k) - wnew)*vm/rsvint(k)                             
      x2n = rsckr(k)*edt*0.5*(x1n + rsx1o(k))                           
     1    + rsx2o(k)                                                    
      if (x2n.lt.0.0) x2n=0.0                                           
C * * *                                                                 
C * * * PRECESS VARIABLE                                                
C * * *                                                                 
      rsx1n(k) = x1n                                                    
      rsx2n(k) = x2n                                                    
      rsfeqo(k) = wnew                                                  
      rseyro(k) = eyr(ldbus)                                            
      rseyio(k) = eyi(ldbus)                                            
      if(x2n .lt. rstlev(k))go to 90                                    
C * * *                                                                 
C * * SET BKR TIMER IF BKR TIME NON ZERO OTHERWISE TRIP INSTANTANEOUSLY 
C * * *                                                                 
      if(rsbdly(k) .eq. 0.0) go to 20                                   
      rsbtmr(k) = to + rsbdly(k)                                        
      uftmp = rsbtmr(k)                                                 
      if(uftmp .lt. dnxrly) dnxrly = uftmp                              
   90 continue                                                          
      return                                                            
      end                                                               
