C    %W% %G%
      subroutine rvsol                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONTAINS THE SOLUTION LOGIC FOR                 
C * * * THE RV VOLTAGE DIFFERENCE LOAD SHEDDING LOGIC.                  
C * * * IT IS CALLED BY RELAY. IT CALL GETMAT AND PUTMAT.               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/buslod.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/ldrep.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/lshed1.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/ldshdn.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/rvcom.inc' 
      include 'tspinc/relays.inc' 
      dimension tempn(2, 13)                                            
      dimension pd(4),qd(4)                                             
      character*8 name                                                  
C * * *                                                                 
C * * * DO NOT CHECK RELAYS IF A FAULT IS APPLIED IESC = 3              
C * * *                                                                 
      if(iesc .ne. 1)return                                             
      do 800 i = 1,kntrv                                                
C * * *                                                                 
C * * * IF IRVSW = 3 THIS RELAY HAS ALREADY DROPPED LOAD                
C * * *                                                                 
      ibus = irvbno(i)                                                  
      if(irvsw(i) .eq. 3) go to 800                                     
C * * *                                                                 
C * * * IF IRVSW = 2 RELAY HAS TRIPPED AND BREAKER DELAY TIMER HAS START
C * * *                                                                 
      if(irvsw(i) .eq. 2)go to 400                                      
      vmag = sqrt(eyr(ibus)*eyr(ibus) + eyi(ibus)*eyi(ibus))            
C * * *                                                                 
C * * * IF IRVSW = 0, THE BUS VOLTAGE HAS NOT YET EXCEEDED THE TRIP     
C * * * DEVIATION                                                       
C * * *                                                                 
      vdelta = rvstan(i) - vmag                                         
      if(rvdrp1(i) .gt. 0.0 .and. vdelta .lt. rvdrp1(i)) then           
         irvsw(i) = 0.0                                                 
         rvclk1(i) = 0.0                                                
         go to 800                                                      
      endif                                                             
      if(rvdrp1(i) .lt. 0.0 .and. vdelta .gt. rvdrp1(i)) then           
         irvsw(i) = 0.0                                                 
         rvclk1(i) = 0.0                                                
         go to 800                                                      
      endif                                                             
      if(irvsw(i) .eq. 1) go to 200                                     
      irvsw(i) = 1                                                      
      rvclk1(i) = to +rvtim1(i)                                         
      if(rvclk1(i) .lt. dnxrly)dnxrly = rvclk1(i)                       
C * * *                                                                 
C * * * IF IRVSW = 1 VOLTAGE HAS EXCEEDED TRIP DEVIATION AND UNDERVOLTAG
C * * * TIMER IS RUNNING                                                
C * * *                                                                 
 200  if(to .lt. rvclk1(i))then                                         
         go to 800                                                      
      else                                                              
         name = bname(ibus)                                             
         bkv = buskv(ibus)                                              
         write(outbuf,300)name,bkv,to                                   
300      format(5x,' VOLTAGE DIFFERENCE LOAD DROPPING RELAY AT ',a8,1x, 
     1          f5.1,' HAS TRIGGERED AT ',f8.2,' CYCLES.')              
         call prtout(1)                                                 
         write(outbuf,320)vmag,to                                       
320      format(5x,' VOLTAGE = ',f6.4,' AT TIME = ',f8.2, ' CYCLES. ')  
         call prtout(1)                                                 
         irvsw(i) = 2                                                   
         rvclk2(i) = to + rvbdy1(i)                                     
         if(rvclk2(i) .lt. dnxrly)dnxrly = rvclk2(i)                    
      endif                                                             
C * * *                                                                 
C * * * IF IRVSW = 2, THE RELAY HAS TRIPPED, SO CHECK TO SEE IF THE     
C * * * BREAKER DELAY TIME HAS BEEN REACHED. IF SO, DROP LOAD.          
C * * *                                                                 
  400 if(to .lt. rvclk2(i)) go to 800                                   
      irvsw(i) = 3                                                      
      if(ibus .lt. lfrst)lfrst = ibus                                   
      ivpc=2                                                            
C * * *                                                                 
C * * * MODIFY ADMITTANCE MATRIX DIAGONAL TO REFLECT LOAD SHED          
C * * *                                                                 
      call getmat(ibus, ii)                                                 
      emag = emagrn(1,ibus)                                             
      vsq=emag*emag                                                     
      pdrop = rvpdp1(i)                                                 
      qdrop = rvqdp1(i)                                                 
C * * *                                                                 
C * * * IF RVSHNT = 'S', CONVERT QDROP FROM NOMINAL VALUE TO            
C * * * VALUE AT PRESENT BUS VOLTAGE                                    
C * * *                                                                 
      if(rvshnt(i) .eq. 'S')then                                        
         vsqnow = eyr(ibus)*eyr(ibus) + eyi(ibus)*eyi(ibus)             
         qdrop = qdrop*vsqnow                                           
      endif                                                             
      atrow(ii-1)=atrow(ii-1)-pdrop*vsq                                 
      atrow(ii)=atrow(ii)+qdrop*vsq                                     
      call putmat(ibus, ii)                                                 
      yreal(ibus) = yreal(ibus) - pdrop*vsq                             
      yimag(ibus) = yimag(ibus) + qdrop*vsq                             
      name = bname(ibus)                                                
      bkv = buskv(ibus)                                                 
      write(outbuf,500)name,bkv,to                                      
  500 format(5x,' VOLTAGE DIFFERENCE RELAY AT ',a8,1x,f5.1,             
     1       ' DROPPED LOAD AT ',f8.2,' CYCLES.')                       
      call prtout(1)                                                    
      write(outbuf,600)pdrop,qdrop                                      
  600 format(5x,' PDROP(PU) = ',f6.2,' QDROP(PU) = ',f6.2)              
      call prtout(1)                                                    
C * * *                                                                 
C * * * CORRECT LOAD REPRESENTATION TABLES IF THEY EXIST                
C * * *                                                                 
      if (lrep.eq.0) go to 800                                          
      iecsl = ldidxn(6,ibus)                                            
      if (iecsl.eq.0) go to 800                                         
      nitem = ldidxn(5,ibus)                                            
      ptot=0.0                                                          
      qtot=0.0                                                          
      iecsl = iecsl -1                                                  
      do 700 j=1,nitem                                                  
      pd(j) = busldn(1,iecsl+j)                                         
      qd(j) = busldn(2,iecsl +j)                                        
      ptot=ptot+pd(i)                                                   
      qtot=qtot+qd(i)                                                   
  700 continue                                                          
      do 750 j=1,nitem                                                  
      if(abs(qtot).gt.0.00001) qd(i)=qd(j)*(1.+qdrop/qtot)              
      if(abs(ptot).gt.0.00001) pd(i)=pd(j)*(1.+pdrop/ptot)              
      busldn(1,iecsl+j) = pd(j)                                         
      busldn(2,iecsl+j) = qd(j)                                         
 750  continue                                                          
800   continue                                                          
      return                                                            
      end                                                               
