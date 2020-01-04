C    %W% %G%
      subroutine relay                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONTAINS THE SOLUTION LOGIC FOR ALL RELAY       
C * * * MODELS.  IT IS CALLED BY CNTRL. IT CALLS GENDRP and LLDROP. 
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc' 

      if(edt .eq. 0.0) return                                           
C * * *                                                                 
C * * * IF THERE ARE INDUCTION MOTORS IN THE STUDY, CALL INDROP         
C * * * TO CHECK FOR LOW VOLTAGE TRIP                                   
C * * *                                                                 
      if(imlv .ne. 0) call indrop                                       
C * * *                                                                 
C * * * CALL RSSOL FOR CF-1 UNDERFREQUENCY LOAD SHEDDING RELAY          
C * * *                                                                 
   10 if(nufreq .ne. 0) call rssol                                      
C * * *                                                                 
C * * * CALL RVSOL FOR VOLTAGE DIFFERENCE LOAD DROPPING RELAY LOGIC     
C * * *                                                                 
      if(kntrv .ne. 0)call rvsol                                        
C * * *                                                                 
C * * * CALL RMSOL FOR GENERATOR UNDERFREQUENCY RELAY LOGIC             
C * * *                                                                 
      if(ngenf .ne. 0)call rmsol                                        
C * * *                                                                 
C * * * CALL RLSOL FOR DEFAULT DISTANCE RELAY LOGIC                     
C * * *                                                                 
      if(ndfltd .ne. 0) call rlsol                                      
C * * *                                                                 
C * * * CALL RGSOL FOR SERIES CAPACITOR LOGIC                           
C * * *                                                                 
      if(kntrg .ne. 0) call rgsol                                       
C * * *                                                                 
C * * * CALL R1SOL FOR POWER RATE RELAY LOGIC                           
C * * *                                                                 
      if(kntr1 .ne. 0) call r1sol                                       
C * * *                                                                 
C * * * CALL RDSOL FOR DISTANCE RELAY LOGIC                             
C * * *                                                                 
      if(kntrd .ne. 0) call rdsol                                       
C * * *                                                                 
C * * * CALL RUSOL FOR UNDERFREQUENCY LINE TRIPPING RELAY LOGIC         
C * * *                                                                 
      if(kntru .ne. 0) call rusol                                       
      return                                                            
      end                                                               
                                                                        
