C    %W% %G%
      subroutine shdsol                                                 
C                                                                       
C     THIS SUBROUTINE PROCESSES THE UNDERFREQUENCY AND                
C     UNDERVOLTAGE LOAD SHEDDING LOGIC                                
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/buslod.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ldrep.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/lshed1.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/shdlod.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/deltfq.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/ldshdn.inc' 
      dimension pd(4),qd(4)                                             
      character*8 name                                                  
      data fconv /9.549297/                                             
      if(idisw .eq. 2) return                                           
      do 1000 ilds = 1,ls                                               
      ibno = lshdno(ilds)                                               
      nlevl =lshdlv(ilds)                                               
      if(nlevl .eq. 0) go to 1000                                       
      do 950 ind1 = 1,nlevl                                             
      if(shdcde(ind1,ilds) .eq. 3.) go to 950                           
      if(shdcde(ind1,ilds) .eq. 2.) go to 480                           
C                                                                       
C     UNDERFREQUENCY LOAD SHED LOGIC                                  
C                                                                       
      if (lshdcd(ilds) .ne. 3) go to 450                                
C                                                                       
C     BYPASS ALL BUS FREQ CALCULATIONS WHEN COMING OUT OF DISCONTINUITY   
C                                                                       
      if(i140.eq.0) go to 950                                           
      freq = shdlev(ind1,ilds)                                          
      emag = emagrn(1,ibno)                                             
      pasta =emagrn(2,ibno)                                             
      eyr1 = eyr(ibno)                                                  
      eyi1 = eyi(ibno)                                                  
      if(eyr1 .eq. 0.0) eyr1 = 0.00001                                  
      presa=atan2(eyi1,eyr1)                                            
      delan = presa-pasta                                               
      if(delan .lt. -3.14159)delan = delan+6.28318                      
      if(delan .gt. 3.14159)delan = delan-6.28318                       
      delf = delan/edt                                                  
      if(tbusf .eq. 0.0)then                                            
         busfeq = -delf                                                 
      else                                                              
         busfeq =-(deltfq(ibno) +delf +delfqo(ibno)*(abus-2.))*abusr    
      endif                                                             
      if(keybrd(31) .ne. 0) then                                        
         write (outbuf, 400) idls, ind1, lshdec, busfeq, freq,          
     1                       shdcde(ind1,ilds)                          
  400    format('0',5x,' IDLS,IND1,LSHDEC,BUSFEQ,FREQ,SHDCDE',3i6,      
     1          2f7.5,f7.2)                                             
         call prtout (1)                                                
      endif                                                             
      if(busfeq .lt. freq) then                                         
C                                                                       
C       FREQUENCY ABOVE THRESHOLD                                       
C                                                                       
        shdcde(ind1,ilds) = 0.0                                        
        shddel(ind1,ilds) = 0.0                                        
        go to 950                                                      
      endif                                                             
C                                                                       
C     FREQUENCY BELOW THRESHOLD                                       
C                                                                       
      if(shdcde(ind1,ilds) .le. 0.0)then                                
C                                                                       
C        BELOW THRESHOLD THE FIRST TIME                                 
C                                                                       
         shddel(ind1,ilds) = to + shdtim(ind1,ilds)                     
         shdcde(ind1,ilds) = 1.0                                        
      endif                                                             
      if(shdcde(ind1,ilds) .eq. 1.) then                                
         if(shddel(ind1,ilds) .le. to) then                             
C                                                                       
C        THRESHOLD TIMED OUT START PCB TIMER                             
C                                                                       
            shdcde(ind1,ilds) = 2.0                                     
            shddel(ind1,ilds) = to + shdpcb(ind1,ilds)                  
            go to 480                                                   
         endif                                                          
         if(shddel(ind1,ilds) .lt. dnxrly)dnxrly = shddel(ind1,ilds)    
C                                                                       
C        THRESHOLD NOT TIMED OUT                                         
C                                                                       
         go to 950                                                      
      endif                                                             
      go to 480                                                         
C                                                                       
C     UNDER VOLTAGE LOAD SHEDDING LOGIC                               
C                                                                       
  450 if(lshdcd(ilds) .ne. 1) go to 470                                 
      volt = shdlev(ind1,ilds)                                          
      eyr1 = eyr(ibno)                                                  
      eyi1 = eyi(ibno)                                                  
      bvltsq = eyr1*eyr1+eyi1*eyi1                                      
      bvlt=sqrt(bvltsq)                                                 
      dcyc = shdtim(ind1,ilds)                                          
      if (bvlt .ge. volt) then                                          
C                                                                       
C        VOLTAGE ABOVE THRESHOLD                                         
C                                                                       
         shdcde(ind1,ilds) = 0.0                                        
         shddel(ind1,ilds) = 0.0                                        
         go to 950                                                      
      endif                                                             
C                                                                       
C     VOLTAGE DIFFERENCE BELOW THRESHOLD                              
C                                                                       
      if(shdcde(ind1,ilds) .le. 0.0)then                                
C                                                                       
C        BELOW THRESHOLD THE FIRST TIME                                 
C                                                                       
         shddel(ind1,ilds) = to + shdtim(ind1,ilds)                     
         shdcde(ind1,ilds) = 1.0                                        
      endif                                                             
      if(shdcde(ind1,ilds) .eq. 1.) then                                
         if(shddel(ind1,ilds) .le. to) then                             
C                                                                       
C           THRESHOLD TIMED OUT START PCB TIMER                             
C                                                                       
            shdcde(ind1,ilds) = 2.0                                     
            shddel(ind1,ilds) = to + shdpcb(ind1,ilds)                  
            go to 480                                                   
         endif                                                          
         if(shddel(ind1,ilds) .lt. dnxrly)dnxrly = shddel(ind1,ilds)    
C                                                                       
C        THRESHOLD NOT TIMED OUT                                         
C                                                                       
         go to 950                                                      
      endif                                                             
      go to 480                                                         
C                                                                       
C     VOLTAGE DIFFERENCE LOAD SHEDDING LOGIC                         
C                                                                       
  470 delvlt = shdlev(ind1,ilds)                                        
      eyr1 = eyr(ibno)                                                  
      eyi1 = eyi(ibno)                                                  
      bvltsq = eyr1*eyr1+eyi1*eyi1                                      
      bvlt=sqrt(bvltsq)                                                 
      emag = 1./emagrn(1,ibno)                                          
      dbvlt = emag - bvlt                                               
      dcyc = shdtim(ind1,ilds)                                          
      if (dbvlt .lt. delvlt) then                                       
C                                                                       
C        VOLTAGE DIFFERENCE ABOVE THRESHOLD                              
C                                                                       
         shdcde(ind1,ilds) = 0.0                                        
         shddel(ind1,ilds) = 0.0                                        
         go to 950                                                      
      endif                                                             
C                                                                       
C     VOLTAGE DIFFERENCE BELOW THRESHOLD                              
C                                                                       
      if(shdcde(ind1,ilds) .le. 0.0)then                                
C                                                                       
C        BELOW THRESHOLD THE FIRST TIME                                 
C                                                                       
         shddel(ind1,ilds) = to + shdtim(ind1,ilds)                     
         shdcde(ind1,ilds) = 1.0                                        
      endif                                                             
      if(shdcde(ind1,ilds) .eq. 1.) then                                
         if(shddel(ind1,ilds) .le. to) then                             
C                                                                       
C           THRESHOLD TIMED OUT START PCB TIMER                             
C                                                                       
            shdcde(ind1,ilds) = 2.0                                     
            shddel(ind1,ilds) = to + shdpcb(ind1,ilds)                  
            go to 480                                                   
         endif                                                          
         if(shddel(ind1,ilds) .lt. dnxrly)dnxrly = shddel(ind1,ilds)    
C                                                                       
C        THRESHOLD NOT TIMED OUT                                         
C                                                                       
         go to 950                                                      
      endif                                                             
  480 if (shddel(ind1,ilds) .lt. dnxrly) dnxrly = shddel(ind1,ilds)     
      if (shddel(ind1,ilds) .le. to) go to 500                          
      go to 950                                                         
C                                                                       
C     LOAD HAS BEEN SHED--CORRECT Y MATRIX                            
C                                                                       
  500 shdcde(ind1,ilds) =  3.                                           
      ivpc=2                                                            
      if (ilds .lt. lfrst) lfrst=ibno                                   
      pold = pltotn(1,ibno)                                             
      qold = pltotn(2,ibno)                                             
      shld = shdlde(ind1,ilds)                                          
      call getmat(ibno, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 550) ibno, atrow(ii-1),atrow(ii)                
  550    format('0 CNTRL, S550+9, B4, IBUS, GII, BII ', i10,            
     1            2e16.6)                                               
         call prtout (1)                                                
      endif                                                             
      emag = emagrn(1,ibno)                                             
      eang = emagrn(2,ibno)                                             
      vsq=emag*emag                                                     
      pdrop=pold*shld                                                   
      qdrop=qold*shld                                                   
      qshun = shdsht(ilds)*shld                                         
      atrow(ii-1)=atrow(ii-1)-pdrop*vsq                                 
      atrow(ii)=atrow(ii)+(qdrop + qshun)*vsq                           
      call putmat(ibno, ii)                                                 
      yreal(ibno) = yreal(ibno) - pdrop*vsq                             
      yimag(ibno) = yimag(ibno) + (qdrop + qshun)*vsq                   
      if (keybrd(30) .ne. 0) then                                       
          write (outbuf, 600)ibno, atrow(ii-1),atrow(ii)                
  600     format('0 CNTRL, S600-4, AFT, IBUS, GII, BII ', i10,          
     1            2e16.6)                                               
          call prtout (1)                                               
      endif                                                             
      name = bname(ibno)                                                
      bkv = buskv(ibno)                                                 
C                                                                       
C     IF THIS IS UNDER FREQ LOAD SHEDDING, CALL SUMUFL TO UPDATE      
C     UNDERFREQ LOAD SHEDDING SUMMARY TABLES.                         
C                                                                       
      if(lshdcd(ilds) .eq. 3)then                                       
         frq = frqbse - shdlev(ind1,ilds)*fconv                         
         pdrp = pdrop                                                   
         call sumufl(ibno,name,bkv,pdrp,frq)                            
         write (outbuf, 630) name, bkv, pdrop, qdrop, qshun,to,frq      
  630    format('0',5x,'LOAD SHED AT',1x,a8,1x,f5.1,' P(PU) ',f5.2,     
     1          ' Q(PU) ',f5.2,' SHUNT(PU) ',f5.2,' AT ',f7.2,          
     2           ' CYCLES. SHEDDING FREQUENCY IS ', f5.2, ' HERTZ ')      
         call prtout (1)                                                
         go to 660                                                      
      endif                                                             
      write (outbuf, 650) name, bkv, pdrop, qdrop, qshun,to             
  650 format('0', 5x, 'LOAD SHED AT', 1x, a8, 1x, f5.1, ' P(PU) ',
     &  f5.2,' Q(PU) ', f5.2, ' SHUNT(PU) ', f5.2, ' AT', f7.2,
     &  ' CYCLES')                   
      call prtout (1)                                                   
C                                                                       
C     CORRECT LOAD REPRESENTATION TABLES IF THEY EXIST                
C                                                                       
  660 if (lrep.eq.0) go to 950                                          
      iecsl = ldidxn(6,ibno)                                            
      if (iecsl.eq.0) go to 950                                         
      nitem = ldidxn(5,ibno)                                            
      ptot=0.0                                                          
      qtot=0.0                                                          
      do 700 i=1,nitem                                                  
      pd(i) = busldn(1,iecsl+ i -1)                                     
      qd(i) = busldn(2,iecsl +i -1)                                     
      ptot=ptot+pd(i)                                                   
      qtot=qtot+qd(i)                                                   
  700 continue                                                          
      do 750 i=1,nitem                                                  
      if(abs(qtot).gt.0.00001) qd(i)=qd(i)*(1.+qdrop/qtot)              
      if(abs(ptot).gt.0.00001) pd(i)=pd(i)*(1.+pdrop/ptot)              
      busldn(1,iecsl + i -1) = pd(i)                                    
      busldn(2,iecsl + i -1) = qd(i)                                    
  750 continue                                                          !DEM
  950 continue                                                          
 1000 continue                                                          
      return                                                            
      end                                                               
