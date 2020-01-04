C    %W% %G%
      subroutine input2                                                 
c                                                                  
c     This subroutine reads the branch data table from the power flow 
c     solution history file.  It serves as the main calling program 
c     for subroutines which form data tables for the load, load 
c     shedding, line switching, and relay models.  It is called by 
c     SWINGM.  It calls DCSORT, EXEXIT, INAM, KEYBRD, LODINP, LSERCH, 
c     MPOST, PRTERR, PRTOUT, R1INP, RDINP, RGINP, RMINP, RRINP, RSINP, 
c     RUINP, SHDINT, and ZONINP.                                                     
c                                                                  
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/ecstbd.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/znox2.inc' 
      include 'tspinc/ldrep.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/legspi.inc' 
      include 'tspinc/brnch.inc' 
      include 'tspinc/lnet.inc' 

      equivalence (sheddn, nshedn)                                      

      include 'tspinc/reread.inc' 
      include 'tspinc/workc.inc' 
      include 'tspinc/areanz.inc' 
      include 'tspinc/brakn.inc' 
      include 'tspinc/bypass.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/rddtai_mv.inc' 

c     -  Local variables

      character*1 sbtypc, irrlyc
      equivalence (ldp, dpt), (ldq, dqp)                            
c
c     -     Begin     Begin     Begin     Begin     Begin     Begin
      call mpost('INPUT2')                                              
      ibrk=ibr-1                                                        
      ibr5=5*ibrk                                                       
C                                                                  
C  INITIALIZE RELAY COUNTERS                                       
C                                                                  
      kntrd = 0                                                         
      kntru = 0                                                         
      kntr1 = 0                                                         
      kntrg = 0                                                         
      kntro = 0                                                         
      kntrr = 0                                                         
      kntru = 0                                                         
      lastec = 90000                                                    
      k13=13                                                            
      k14=14                                                            
      k16=16                                                            
      k17=17                                                            
      k19=19                                                            
      k20=20                                                            
      k23=23                                                            
      k29=29                                                            
      k30=30                                                            
      k35=35                                                            
      k36=36                                                            
      k300=300                                                          
      ik3=1                                                             
      ibcap=0                                                           
      ik3cap=1                                                          
      ibzn = 0                                                          
      ik3zn = 1                                                         
      iiznkt = 0                                                        
      iibr=0                                                            
      ilrd=0                                                            
      ilrr=0                                                            
      ilsc=0                                                            
      ilst = 0                                                          
      pi = 3.14159265                                                   
      cvcon = bmva * pi / frqbse                                        
C                                                                  
C  CALL ZNOINP TO INITIALIZE ZNO CAPACITOR TABLES                  
C                                                                  
c     IF (IZNKT .GT. 0) CALL ZNOINP                                     !dem
c     IF(IZNKT .GT. O) CALL ZNOINP                                      
      if (keybrd(16) .ne. 0) then                                        
         do 10139 i = 1,nbypas                                          
         write (outbuf,139) i,kbpass(1,i),kbpass(2,i)                   
         call prtout (1)                                                
10139    continue                                                       
  139    format (1x,'  RELAY BYPASS       ',1x,i10,5x,i10,5x,i10)        
      endif                                                             
  144 continue                                                          
C                                                                  
C  CHECK LOAD NETTING BUSES TO INSURE THAT THEY EXIST              
C  IN THE STUDY                                                    
C                                                                  
      if (ln .ne. 0) then                                                 
         write (outbuf,6330)                                            
         call prtout (1)                                                
 6330    format ('0  CHECKING BUS AND BASE IDENTIFICATION FOR LOAD ',    
     1          'NETTING BUSES.')                                       
         jdsave=jdbus(100)                                              
         if (jdelet.lt.100)jdbus(100)=10000                              
         do 6340 i=1,ln                                                 
         j=inam(lnetc(i),lnetn(i))                                      
         if (j.ne.0)go to 6340                                           
         kb=lnetn(i)                                                    
         base= basekv(kb)                                               
         write (errbuf(1),6335)lnetc(i),base                            
         call prterr ('E',1)                                            
 6335    format(1x,' THE LOAD NETTING BUS (',a8,2x,f6.1,' )DOES NOT ',  
     1          'BELONG TO THIS POWER FLOW.')                           
 6340    continue                                                       
         jdbus(100)=jdsave                                              
      endif                                                             
C                                                                  
C  CALL RSINP TO DECODE UNDERFREQUENCY CF - 1 LOAD SHED RELAY      
C                                                                  
      if (nufreq .gt. 0) call rsinp                                       
C                                                                  
C CALL RMINP TO DECODE UNDERFREQUENCY GENERATOR DROPPING RELAY     
C                                                                  
      if (ngenf .gt. 0) call rminp                                        
C                                                                  
C  CALL DCSORT TO READ DC CARDS AND FORM DATA TABLES               
C                                                                  
      call dcsort                                                       
C                                                                  
C     INITIALIZE ECS POINTERS                                      
C                                                                  
 6460 keyr=1                                                            
      keyi = keyr + 1                                                   
      kdc = keyi + 1                                                    
      kdiscn = kdc + jdc * ( idcl/2 )                                   
C    *   IN THE ABOVE WE ASSUMED THAT ALL CARDS BELONG TO 2 TERMNL DC.  
C    *   ECS SPACE IS WASTED IF THERE ARE MULTI DC CKTS.                
      kswtch=kdiscn + 1                                                 
      kbrnch=kswtch + 1                                                 
      kxname=kbrnch +1                                                  
      iecbrk = kxname + 1                                               
      krelay = iecbrk + 1                                               
      krecs = krelay + 1                                                
      irecs = krecs                                                     
      krrecs = irecs + 1                                                
      irrecs = krrecs                                                   
      kcapec = irrecs + 1                                               
      icapec = kcapec                                                   
      krrrec = krelay                                                   
      irrrec = krrrec                                                   
      keclsc=icapec+1                                                   
      keclrd=keclsc + 1                                                 
      keclrr=keclrd + 1                                                 
      jldar = keclrr + 1                                                
      kecldr = jldar                                                    
      jldz = kecldr + 1                                                 
      kecldz=jldz                                                       
      jlrep=kecldz+1                                                    
      keclrp = jlrep                                                    
      kecnxt=keclrp+1                                                   
      load = lrep + ldz +ldar                                           
C                                                                  
C  CALL LODINP FOR FORM TABLES FOR LOAD REPRESENTATION             
C                                                                  
      if (load .gt. 0)call lodinp                                        
      num=0                                                             
      lst=iline                                                         
      iline = MAXLS                                                     
C                                                                  
C  SKIPPING OVER NON ESSENTIAL RECORDS ON THE POWER FLOW           
C                                                                  
c dlc        IF (KXTOT.GT.0) READ (L3) JUNK                                   
  680 ibr=ibr-1                                                         
      missbr=0                                                          
C                                                                  
C  NOW WE ARE READY TO OBTAIN ADMITTANCES FROM BRANCH DATA RECORD  
C  ON THE POWER FLOW SOLUTION HISTORY FILE                         
C                                                                  
 1040 ibps=0                                                            
c dlc      DO 1045 I1 = 1,LTOT,100                                           
c dlc      I2 = MIN0(I1+99,LTOT)                                             
c dlc      READ (L3) ((BRNCH(J,I),J=1,18), I=I1,I2)                          
c dlc 1045 CONTINUE                                                          
C     -  Convert Ckt ID "0" to " " for consistency with PF              
      izero = intch4('0   ')                                            
      iblnk = intch4('    ')                                            
      do 711 la = 1,ltot                                                
        if (jbrnch(13,la) .eq. izero) jbrnch(13,la) = iblnk             
  711 continue                                                          
C                                                                  
C  CALL LSERCH TO OBTAIN ADMITTANCES FOR THE LINE SWITCHING        
C  (LS) CARDS                                                      
C                                                                  
      call lserch                                                       
C                                                                  
C  IF A DEFAULT DISTANCE RELAY IS IN THE STUDY, SEARCH THE         
C  ENTIRE BRANCH DATA TABLE AND ADD ALL LINES CONTAINING           
C  REGULATING TRANSFORMERS, DC LINES, OR ASSYMMETRIC LINES         
C  TO THE DEFAULT DISTANCE RELAY BYPASS TABLE.                     
C  IBRR = 1 LINE EQUIVALENT PI; = 2 MULTITERMINAL DC LINE          
C  IBRR = 3 LINE SECTION PI     = 4 REGULATING TRANSFORMER         
C  IBRR = 5 TRANSFORMER         = 6 PHASE SHIFTER                  
C  IBRR = 7 2TERM DC LINE       = 8 ASSYMETRIC AC LINE             
C  IBRR = 9 RANI LINE                                              
C                                                                  
      if (iswbps .ne. 0) then                                             
         kend=min0(num,100)+10                                          
         do 1058 i = 1,ltot                                             
         ibrr = jbrnch(1,i)                                             
         if (ibrr .ne. 5 .and. ibrr .ne. 6 .and. ibrr .ne. 1) go to 1058 
         k1 = jbrnch(2,i)                                               
         k2 = jbrnch(12,i)                                              
         nbypas = nbypas+1                                              
         kbpass(1,nbypas) = k1                                          
         kbpass(2,nbypas) = k2                                          
 1058    continue                                                       
      endif                                                             
c     if (iznkt .gt. 0) call znoinp      
C                                                                  
C  CALL RGINP TO READ THE SERIES CAPACITOR DATA CARDS AND FORM DATA
C  TABLES                                                          
C                                                                  
      if (lsc .ne. 0) call rginp                                        
C                                                                  
C   THE IMPEDANCE RELAY, OUT OF STEP BLOCKING RELAY, THE POWER RATE
C   RELAY AND THE REMOTE RELAY ARE ALL STORED IN THE SAME TABLE.   
C   LRD COUNTS THE NUMBER OF THESE RELAY DATA CARDS.               
C                                                                  
  880 if (lrd .ne. 0) then                                              
         call whrec1 ('RD ',msrrl,msrrh,msrrsz)                         !dem
         msrrh = msrrh + 1                                              !dem
         if (lrd .le. 100) then                                         !dem
           read (l1,rec=msrrh) ((work(ll,nn), ll=1,8), nn=1,lrd)         !dem
         else                                                           !dem
           read (l1,rec=msrrh) ((work(ll,nn), ll=1,8), nn=1,100)         !dem
           msrrh = msrrh + 1                                            !dem
           read (l1,rec=msrrh) ((work(ll,nn), ll=1,8), nn=101,lrd)       !dem
         endif                                                          !dem
c        IF((LRD-100).GT.0)read (l1,REC=182)                              
c    1   ((WORK(LL,NN),LL=1,8),NN=101,200)                              
c        IF(LRD .GT. 100) GO TO 885                                     
c        read (l1,REC=181) ((WORK(LL,NN),LL=1,8),NN=1,LRD)                
c        GO TO 890                                                      
c 885    read (l1,REC=181)((WORK(LL,NN),LL=1,8),NN=1,100)                 
c        read (l1,REC=182)((WORK(LL,NN),LL=1,8),NN=101,LRD)               
  890    do 920 i=1,lrd                                                 
           read (work80(i),900) sbtypc                                     
  900        format (1x,a1)                                                  
           buffer = work80(i)                                             
C                                                                  
C  CALL RDINP TO READ THE DISTANCE RELAY CARDS AND FORM            
C  INITIAL TABLES                                                  
C                                                                  
           if (sbtypc .eq. 'D') call rdinp                                  
C                                                                  
C  CALL RUINP TO READ THE UNDERFREQUENCY RELAY CARDS AND FORM      
C  INITIAL TABLES                                                  
C                                                                  
           if (sbtypc .eq. 'U') call ruinp                                  
C                                                                  
C  CALL RDINP TO READ THE OUT OF STEP RELAY CARDS AND FORM         
C  INITIAL TABLES.  THE RD AND RO RELAYS ARE IDENTICAL, SO         
C  USE THE SAME TABLES FOR BOTH.                                   
C                                                                  
           if (sbtypc .eq. 'O') call rdinp                                  
C                                                                  
C  CALL R1INP TO READ THE POWER RATE RELAY CARDS AND FORM          
C  INITIAL TABLES                                                  
C                                                                  
           if (sbtypc.eq.'1' .or. sbtypc.eq.'2' .or. sbtypc.eq.'3')
     +       call r1inp
  920    continue                                                       
      endif                                                             
C                                                                  
C  CALL RRINP TO READ AND FORM INITIAL DATA TABLES FOR THE         
C  REMOTE RELAY.                                                   
C                                                                  
      if (lrr .ne. 0) call rrinp                                        
C                                                                  
C  CALL ZNOINP TO INITIALIZE ZNO CAPACITOR TABLES                  
C                                                                  
      if (iznkt .gt. 0) call znoinp                                     !dem
c     IF(IZNKT .GT. O) CALL ZNOINP                                      
 5000 krrs=irrrec                                                       
      knext=krrs+1                                                      
      kphase = knext                                                    
C     *                                                                 
C     * FORMING TABLES LDIDXF,LDINDXP,AND BUSLD                         
C     *                                                                 
 5020 keclst = kecnxt                                                   
      kldin = keclst                                                    
      kldint = kldin                                                    
      l=lrep+ldz+ldar                                                   
      if (l .eq. 0) then                                                  
         write (errbuf(1),5025)                                         
         call prterr ('W',1)                                            
 5025    format ('0   NO LOAD REPRESENTATION CARDS WERE ENTERED ',       
     1           ' THEREFORE LOAD SWITCHING MAY CAUSE ERRONEOUS ',       
     2           'ANSWERS.')                                             
         klshed = kldin                                                 
         kshed = klshed                                                 
      endif                                                             
C                                                                  
C  CALL SHDINP TO FORM LOAD SHEDDING TABLES                        
C                                                                  
      if (ls .ne. 0) call shdinp                                        
      klshed = kshed                                                    
      keclst=klshed                                                     
C                                                                  
C     START OF PRESET AND CHECK BLOCK                              
C     THIS BLOCK OF LOGIC PRESETS CONSTANTS WHICH WERE NOT         
C     PROVIDED BY THE USER AND CHECKS FOR TABLE OVERFLOW           
C                                                                  
 5650 iswlnk = 0                                                        
      if (missbr.ne.0) iswlnk = 1                                       
      if (ifcd .ne. 0) then                                               
         if (cyc(ifcd).eq.0.0) cyc(ifcd+1)=1.0                          
      endif                                                             
C                                                                  
C              CHECK DATA LIMITS                                        
C                                                                  
      if (ifcd .gt. MAXLS) then                                          
         write (errbuf(1),5720)                                         
         call prterr ('E',1)                                            
 5720    format ('0', 41h cd type 1--switching data table exceeded)      
         iswlnk = 1                                                     
      endif                                                             
      if (lst .le. imon) then                                           
         write (errbuf(1),5760)                                         
         call prterr ('E',1)                                            
 5760    format ('0', 25h line data table exceeded)                      
         iswlnk = 1                                                     
      endif                                                             
      if (isg .gt. mxng) then                                           
         write (errbuf(1),5800)                                         
         call prterr ('E',1)                                            
 5800    format ('0', 30h generator data table exceeded)                 
      endif                                                             
      if (iswlnk .eq. 1) then                                             
         link = 3                                                       
         write (errbuf(1),5840)                                         
         call prterr ('E',1)                                            
 5840    format ('0 LINK 3 SUCCESSFULLY COMPLETED WITH DATA ERRORS')     
         call erexit                                                    
         return                                                         
      endif                                                             
      if (keybrd(8).eq.0) go to 5940                                    
      write (outbuf,5880)                                               
 5880 format('0', 11x,'KEYR',6x,'KEYI',7x,'KDC',4x,'KDISCN',3x,'KSWTCH',
     14x,'KBRNCH')                                                      
      call prtout (1)                                                   
      write (outbuf,5881) keyr,keyi,kdc,kdiscn,kswtch,kbrnch            
 5881 format (5x,6i10)                                                   
      call prtout (1)                                                   
      write (outbuf,5900)                                               
 5900 format('0', 9x,'KRELAY',5x,'KLDIN',5x,'KLDID',5x,'KBUSLD',5x,'KSHE
     1D',4x,'KLSHED',4x,'KPHASE')                                       
      call prtout (1)                                                   
      write (outbuf,5901) krelay,kldin,kldid,kbusld,kshed,klshed,kphase 
 5901 format (5x,7i10)                                                   
      call prtout (1)                                                   
      write (outbuf,5920)                                               
 5920 format('0',11x,'KRRS',4x,'KXNAME',5x,'KECST',4x,'KECSX',4x,'LASTE 
     1C')                                                               
      call prtout (1)                                                   
      write (outbuf,5921) krrs,kxname,kecst,kecsx,lastec                
 5921 format (5x,5i10)                                                   
      call prtout (1)                                                   
 5940 write (outbuf,5960)                                               
 5960 format ('0','SUBROUTINE INPUT2 HAS BEEN PROCESSED.')               
      call prtout (1)                                                   
      return                                                            
      end                                                               
