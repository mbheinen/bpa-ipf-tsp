C    %W% %G%
      subroutine freqc                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE UPDATES THE PAST VALUE OF BUS                   
C * * * VOLTAGE ANGLE, BUS FREQUENCY, AND FILTERED BUS                  
C * * * FREQUENCY FOR ALL BUSES IN THE STUDY.                           
C * * * IT IS CALLED BY CNTRL.                                          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/ldndxp.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/lshed1.inc' 
      include 'tspinc/deltfq.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/shdlod.inc' 
C * * *                                                                 
C * * *  CALCULATE BUS FREQUENCY AND FILTERED BUS FREQUENCY             
C * * *  FOR ALL BUSES IN THE STUDY                                     
C * * *                                                                 
      do 100  ibus = 1,nmx                                              
      dfreqo = deltfq(ibus)                                             
      flfeqo = delfqo(ibus)                                             
      eangp = emagrn(2,ibus)                                            
      eyr1 = eyr(ibus)                                                  
      eyi1 = eyi(ibus)                                                  
      if((eyr1 .eq. 0.0) .and.(eyi1 .eq. 0.0)) go to 100                
      eang=atan2(eyi1,eyr1)                                             
      emagrn(2,ibus) = eang                                             
C * * *                                                                 
C   BYPASS ALL BUS FREQ CALCULATIONS WHEN COMING OUT OF DISCONTINUITY   
C * * *                                                                 
      if(i140.eq.0) go to 100                                           
      delan = eang-eangp                                                
      if(delan.lt.-3.14159) delan=delan+6.28318                         
      if(delan.gt. 3.14159) delan=delan-6.28318                         
C                                                                       
C       A NEW ONE DIMENSIONED ARRAY NOW STORES THE BUS FREQUENCY        
C       DIFFERENCE -- DELTFQ                                            
C                                                                       
      dfreq = delan/edt                                                 
      deltfq(ibus) = dfreq                                              
C * * *                                                                 
C * * * CALCULATE FILTERED BUS FREQUENCY DELFQO                         
C * * *                                                                 
      if(tbusf .ne. 0.0)then                                            
         flfeq = (dfreq + dfreqo +flfeqo*(abus-2.))*abusr               
         delfqo(ibus) = flfeq                                           
      endif                                                             
  100 continue                                                          
      return                                                            
      end                                                               
