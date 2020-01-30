C    %W% %G%
      subroutine scal                                                   
c                                                                       
c       THIS SUBROUTINE CALCULATES A SCALE FOR THE PLOT AXIS WHICH      
c       RESULT IN AN EASY TO READ SCALE AND ACCOMODATE ALL DATA         
c       IT IS CALLED BY CALPLT AND MVPLOT.                              
C  Revs:
C    Aug/03/92 - DEM
C      Added multiplier of 10 on VRNGE missing in DO 120 loop
C      Added min,max,range values to error messages
c                                                                       
      include 'tspinc/ovly6.inc' 
      include 'tspinc/prt.inc' 

C     -     Begin     Begin     Begin     Begin     Begin     Begin 

      small=1.0e-5                                                      
      trnge=smax-smin                                                   
      fuldiv=8.0                                                        
      vrnge=0.0001                                                      
      if (trnge.eq.0.0) return                                          
C     -  Determine plot range m * 10 ^ nn,  where m = 1,2,4 and 
C        nn = -4 .. +4 
C     -  Equiv to SM * VRNGE => RNGE,  where VRNGE = 10 ^ nn
      do 120  j = 1, 9                                                  
        sm = 1.0                                                            
C       -  Fuvr = 8 * 10 ^ nn
        fuvr = fuldiv * vrnge                                                 
  100   if (sm .le. 4.000) then 
          rnge = sm * fuvr                                                      
          if (rnge + small .ge. trnge) go to 160                                
          sm = sm * 2.0                                                         
          go to 100                                                         
        endif
        vrnge=vrnge*10.0                                                !dem
  120 call endo
C
      vrnge=vrnge*10.0                                                  !dem
      write (errbuf(1), 137)                                            !dem
  137 format(2x,' SCAL - Automatic scaling problem - range too large.') !dem
      write (errbuf(2), 141) ' Min = ',smin,'  Max = ',smax,            !dem
     +  '  Range = ',trnge                                              !dem
  141 format (2x,a,g11.4,a,g11.4,a,g11.4)                               !dem
      call prterr ('E',2)                                               !dem
      return                                                            
c                                                                       
C     DETERMINE NO OF DEC PLACES                                
c                                                                       
  160 scdiv=rnge/fuldiv                                                 
      remain=amod(scdiv,10.0)                                           
      if (remain.ne.0.) go to 180                                       
      kxp=-1                                                            
      go to 260                                                         
  180 do 200 i=1,4                                                      
      remain=remain*10.0                                                
      if (remain.ge.1.0) go to 240                                      
  200 continue                                                          
C     WRITE (ERRBUF(1),140)                                             
C     CALL PRTERR ('E',1)                                               
      write (errbuf(1), 207)                                            !dem
  207 format(2x,' SCAL - Scaling problem in setting decimal point.')    !dem
      write (errbuf(2), 141) ' Min = ',smin,'  Max = ',smax,            !dem
     +  '  Range = ',trnge                                              !dem
      call prterr ('E',2)                                               !dem
      return                                                            
  240 kxp=i                                                             
c                                                                       
C     ESTABLISH SCALE MAX                                       
c                                                                       
  260 continue                                                          
      if (smax.eq.0.0) then                                             
         py=8.0                                                         
         fulsy=0.0                                                      
         go to 440                                                      
      endif                                                             
      if (smin.eq.0.0) then                                             
         py=0.0                                                         
         fulsy=rnge                                                     
         go to 440                                                      
      endif                                                             
      temp=rnge                                                         
  340 do 360 i=1,9                                                      
      if (abs(temp-smax).le.small) go to 400                            
      if (temp-scdiv+small.lt.smax) go to 400                           
  360 temp=temp-scdiv                                                   
C     WRITE (ERRBUF(1),140)                                             
C     CALL PRTERR ('E',1)                                               
      write (errbuf(1),367)                                             
  367 format(2x,' SCAL - Scaling problem in setting plot max.')         !dem
      write (errbuf(2), 141) ' Min = ',smin,'  Max = ',smax,            !dem
     +  '  Range = ',trnge                                              !dem
      call prterr ('E',2)                                               !dem
  400 fulsy=temp                                                        
  440 valmin=fulsy-(8.0*scdiv)                                          
c                                                                       
C     OFF SCALE TEST                                                    
c                                                                       
      if (valmin-small.le.smin) go to 460                               
c                                                                       
C     INCREASE RANGE                                            
c                                                                       
      rnge=rnge*2.0                                                     
      if (sm+small.gt.4.0) rnge=rnge*1.25                               
      go to 160                                                         
c                                                                       
C     DETERM POS Y=0.0                                          
c                                                                       
  460 pzero=0.0                                                         
      py=(pzero-valmin)/scdiv                                           
      return                                                            
      end                                                               
