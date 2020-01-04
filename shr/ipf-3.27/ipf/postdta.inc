
      common /postdtaa/ 
     &  namary
     &, araary, corary, busary, genary, reaary, flgary  
     &, text, nxtcord
     &, buspf, buspf1, buspf2, buscd1, buscd2
     &, bubpf, bubpf1, bubpf2, bubcd1, bubcd2 
     &, prsmsg, msgid, msgid2, msgid2a, msgid3
     &, apqsum     ! used for summing latent branch flo
     &, usropts    ! Plot options supplied with batch comands

      character*103 namary
      character*80 araary, corary, busary, genary, reaary, flgary
      character*120 text, nxtcord
      character*8 buspf, buspf1, buspf2, buscd1, buscd2
      character*10 bubpf, bubpf1, bubpf2, bubcd1, bubcd2
      character*50 prsmsg(40)
      character  msgid*2, msgid2*5, msgid2a*6, msgid3*5
      character*20 apqsum
      character*90 usropts(30)

c****** function declaration ******************************
      character  capital*132
c**********************************************************

      common /postdtai/ 
     &  kbusyes, first, last, kcorflg, kotgflg, kdspflg
     &, dgoptyp, dgopout, vaopdif
     &, bsopnam, bsopvlt, bsopang, bsopgen, bsoprea, bsoplod, bsopsum 
     &, bropflo, broptap, bropcmp, broppar   
     &, broppsn, bropqsn, bropprc, bropqrc
     &, ssophdr, ssoppfc
     &, kntuopt
***  &, bropflo   ********This variable is not used

      common /postdtaf/ 
     &  basepf, basepf1, basepf2 
     &, psumflo, qsumflo     ! used for summing latent branch flo

********** declarations  ********************
      integer first
      integer kntuopt   ! Count of options submitted with batch plot 

********** Option switches  ********************
********** Diagram Options *********************
      integer dgoptyp   ! Diagram type:    1 - P & Q flow  (See "broppxx" and "bropqxx
c                                          2 - MVA/I flow  (Sending end only)
c                                          3 - Loss        
c                                          4 - Interchange
c                                          5 - Coordinates (No flow data)
      integer dgopout   ! Outages          0/1 - No Show (*DEFAULT) / Show
********** Value Options *************************
      integer vaopdif   ! Differences      0/1 - Normal/Difference values
********** Bus Options *************************
      integer bsopnam   ! Bus Name         1 - Print Abbreviation     (*DEFAULT)
c                                          2 - Print Name and KV
      integer bsopvlt   ! Bus Voltage      1 - Voltage in Kv          (*DEFAULT)             
c                                          2 - Voltage in Per Unit
      integer bsopang   ! Bus Angle        0/1 - No Show / Show (*DEFAULT)
      integer bsopgen   ! Bus Generation   0/1/2/3 - No Show / 
c                                           Show (*DEFAULT-no 0 angle gen) / 
c                                           Show More(include 0 angle gen) / Show all
      integer bsoprea   ! Bus Reactance    0/1/2/3 - No Show / 
c                                           Show (*DEFAULT-no 0 angle shunt) / 
c                                           Show More(include 0 angle shunt) / Show all
      integer bsoplod   ! Bus Load         0/1 - No Show (*DEFAULT) / Show
      integer bsopsum   ! Sum of othr flow 0/1 - No Show (*DEFAULT) / Show
********** Branch Options **********************
      integer broppsn   ! Flow Detail:     0/1 - No P/P, Sending end   (P - DEFAULT)  
      integer bropqsn   ! Flow Detail:     0/1 - No Q/Q, Sending end   (Q - DEFAULT)  
      integer bropprc   ! Flow Detail:     0/1 - No P/P, Recv end   (No P - DEFAULT)  
      integer bropqrc   ! Flow Detail:     0/1 - No Q/Q, Recv end   (No Q - DEFAULT)  
      integer broptap   ! Transformer Taps 0/1 - No Show (*DEFAULT) / Show
      integer bropcmp   ! Series Caps      0/1 - No Show / Show (*DEFAULT)
      integer broppar   ! Parallels        0/1 - Combined (DEFAULT) / Separate      
    
********** Site standard Options **********************
      integer ssophdr   ! Site Specific    0   - No Pwrflo Header records (BPA DEFAULT)  
c                                          1/2 - Begin with header record 1 or 2
      integer ssoppfc   ! Site Specific    0   - No Pwrflo Header comments               
c                                          n   - Print 'n' Pwrflo Comment records 
c                                          n   - is set in pfmaster.post with      
c                                                Option SSpecific Comments=n 
********** Option switches  ********************

