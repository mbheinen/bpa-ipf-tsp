C    %W% %G%
      subroutine busplt                                                
C                                                                       
C        THIS SUBROUTINE PLOTS THE REAL AND REACTIVE BUS LOADS.         
C        IT IS CALLED BY CALPLT                                         
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/newtab.inc' 
      include 'tspinc/lodbus.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/jbusdt.inc' 
      include 'tspinc/idgen.inc' 
      include 'tspinc/ovly6.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/out512.inc' 
      common/tplink/ paplim                                             
      dimension xsym(3),ysym(3)                                         
      dimension dreal(3),dimg(3)                                        
      character*80 blf                                                  
      character * 8 ibusc                                               
      character * 10 xzplt(4),xaxe(2)                                   
      character * 10 tic                                                
      character * 8 time                                                
      character * 10 yzplt(6),ybus(8),ylin(6),ydc(18),ylabl(28)         
      dimension itemp(15)                                               
      dimension ylmt(2)                                                 
      logical plotop                                                    
      data time /' CYCLES '/                                            
      data ybus  / '   BUS VOL', 'TAGE PU   ' ,'  BUS FREQ',            
     1                   ' DEV HERTZ',                                  
     2               '    BUS LO','AD MWS    ',                         
     3               '   BUS LOA','D MVARS   '/                         
      plotop(ind)=ind.eq.2.or.ind.eq.3.or.ind.eq.6.or.ind.eq.7          
     1            .or.ind.eq.12.or.ind.eq.13                            
      ibussw = 3                                                        
C                                                                       
C       LOOP THRU LOGIC TWICE, IBUSSW = 4, REAL BUS LOAD                
C       IBUSSW = 5, REACTIVE BUS LOAD                                   
C                                                                       
  100 ibussw=ibussw+1                                                   
      isw=0                                                             
C                                                                       
C       LOOP THRU USER-REQUESTED MAX AND MINS TO SEE IF THIS            
C       OPTION HAS EXTERNAL LIMITS                                      
C                                                                       
      do 200 jj=1,4                                                     
      if (ibuscl(jj).ne.ibussw-1) go to 200                             
      if( (busmax(jj).eq.0.0) .and. (busmin(jj).eq.0.0) )go to 250      
C                                                                       
C        IF BUSMAX EXISTS, EXTERNAL LIMITS HAVE BEEN SUPPLIED           
C        AND ISW = 1                                                    
C                                                                       
      if( busmax(jj) .ne.0.0)go to 150                                  
      if( sign(1.,busmax(jj)) .gt.0.0)go to 150                         
      if( sign(1.,busmin(jj)) .gt.0.0)go to 150                         
      go to 250                                                         
C                                                                       
C            GET THE USER-SUPPLIED VALUES                               
C                                                                       
  150 ymin=busmin(jj)                                                   
      ymax=busmax(jj)                                                   
C                                                                       
C       SCDIV = UNITS PER INCH, ON 8-INCH AXIS                          
C                                                                       
      isw=1                                                             
      scdiv=(ymax-ymin)/8.                                              
      valmin=ymin                                                       
      kxp=3                                                             
C                                                                       
C       PY = NUMBER OF INCHES ABOVE ZERO                                
C                                                                       
      py=-valmin/scdiv                                                  
  200 continue                                                          
C                                                                       
C       LOOP THROUGH NUMBER OF DISTINCT PLOTTING GROUPS, NOGRP          
C                                                                       
  250 do 850 kgrp=1,nogrp                                               
C                                                                       
C       KTPLOT IS THE COUNTER OF PLOTS ON THIS AXIS                     
C                                                                       
      ktplot=0                                                          
C                                                                       
C       IGRP IS THE USER-SUPPLIED GROUP NUMBER.                         
C                                                                       
      igrp=igrup(kgrp)                                                  
      i=0                                                               
      ibuskt = 0                                                        
C                                                                       
C       I IS THE COUNTER OF THE BUSSES PLOTTED.                         
C       NMX IS THE NUMBER OF BUSSES TO PLOT.                            
C                                                                       
  300 i=i+1                                                             
      if (i.gt.nmx) go to 450                                           
C                                                                       
C       CHECK JBUSDT TO SEE IF THIS OPTION IS TO BE PLOTTED             
C       JBUSDT(4,I) FOR REAL LOAD                                       
C       JBUSDT(5,I) FOR REACTIVE LOAD                                   
C                                                                       
        isw1 = jbusdt(ibussw,i)                                         
        if(isw1 .gt. 0)ibuskt = ibuskt + 1                              
C       ISW1 IS THE OUTPUT INDICATOR                                    
C       ISW1 = 0 - NO OUTPUT                                            
C       ISW1 = 1 - LIST                                                 
C       ISW1 = 2 - PLOT                                                 
C       ISW1 = 3 - LIST AND PLOT                                        
C                                                                       
      if(.not.plotop(isw1)) go to 400                                   
C                                                                       
C             SKIP IF NOT THE CURRENT GROUP NUMBER.                     
C                                                                       
      if(jbusdt(1,i) .ne. igrp) go to 400                               
      ktplot=ktplot+1                                                   
      idgenc(1,ktplot) = newtbc(i)                                      
      idgenn(1,ktplot) = inwtb(i)                                       
      itemp(ktplot) = ibuskt                                            
      if (isw.ne.1) go to 400                                           
C                                                                       
C       IF MAX AND MIN ARE USER SUPPLIED, CHECK DATA TO SEE IF IT       
C       EXCEEDS LIMITS AND TRUNCATE.                                    
C                                                                       
      do 350 jj=1,icount                                                
      if(ibussw .eq. 4)then                                             
         if(busmw(ibuskt,jj) .gt. ymax)busmw(ibuskt,jj) = ymax          
         if(busmw(ibuskt,jj) .lt. ymin)busmw(ibuskt,jj) = ymin          
      endif                                                             
      if(ibussw .eq. 5)then                                             
         if(busvar(ibuskt,jj) .gt. ymax)busvar(ibuskt,jj) = ymax        
         if(busvar(ibuskt,jj) .lt. ymin)busvar(ibuskt,jj) = ymin        
      endif                                                             
  350 continue                                                          
  400 continue                                                          
C                                                                       
C       IPLTKT IS THE MAXIMUM NUMBER OF PLOTS PER AXIS                  
C                                                                       
      if(ktplot.ne.ipltkt)go to 300                                     
  450 if (ktplot.eq.0) go to 850                                        
      isave=i                                                           
      if (isw.eq.1) go to 550                                           
C                                                                       
C            THIS SECTION DOES AUTMATIC SCALING IF                      
C            THE MAX AND MIN ARE NOT SUPPLIED IN DATA.                  
C                                                                       
      ymax = 0.0                                                        
      ymin = 0.0                                                        
      do 500 i=1,ktplot                                                 
C                                                                       
C            LOOP THROUGH THE PLOT DATA TO GET THE MAXIMA               
C            AND MINIMA FOR ALL PLOTS, TO FIND THE MAXIMUM              
C            MAXIMUM AND THE MINIMUM MINIMUM.  THIS IS SO               
C            THAT THEY ALL CAN FIT ON THE SAME GRAPH.                   
C                                                                       
      indx = itemp(i)                                                   
      if(ibussw .eq. 4)then                                             
         ylmt(1) = busmw(indx,icount+1)                                 
         ylmt(2) = busmw(indx,icount+2)                                 
      endif                                                             
      if(ibussw .eq. 5)then                                             
         ylmt(1) = busvar(indx,icount+1)                                
         ylmt(2) = busvar(indx,icount+2)                                
      endif                                                             
      if (ylmt(1).gt.ymax) ymax = ylmt(1)                               
      if (ylmt(2).lt.ymin) ymin = ylmt(2)                               
  500 continue                                                          
      smax=ymax                                                         
      smin=ymin                                                         
      trnge=smax-smin                                                   
C                                                                       
C            DONT PLOT THIS GROUP, IF THE RANGE IS CLOSE TO ZERO        
C                                                                       
      if (abs(trnge).lt..001) go to 850                                 
C                                                                       
C       CALL SCAL TO CALCULATE MAXIMUM, MINIMUM, AND UNITS PER INCH     
C                                                                       
      call scal                                                         
C                                                                       
C       FILL IN THE AXIS LABELS AND DO THE GRAPH                        
C                                                                       
  550 dy=scdiv                                                          
      yaxe(1) (1:10)=ybus(2*ibussw-3)                                          
      yaxe(1)(11:20)=ybus(2*ibussw-2)                                          
C                                                                       
C          CALL PLOTTING ROUTINES TO FORM GRAPH                         
C          SET THE PLOT LENGTH TO THE GREATER OF                        
C          8.5 INCHES OR AXIS LENGTH PLUS ONE.  THIS MODIFICATION       
C          PLUS COMMON/TPLINK/ ALLOW THE PROGRAM TO CHANGE XMAX         
C          IN PLOTS WHICH WAS NORMALLY SET IN A DATA STATMENT THERE.    
C                                                                       
      paplim = 8.5                                                      
      if(tlenth.gt.7.0) paplim = tlenth + 1.5                           
      call plots(1,0,0)                                              
      xcom=1.0                                                          
      ycom=9.85                                                         
      xnam=1.5                                                          
      xsym(1)=0.5                                                       
      xsym(2) = 0.0                                                     
      ysym(2) = 0.0                                                     
      xsym(3) = 1.0                                                     
      ysym(3) = 1.0                                                     
      dreal(2) = 0.0                                                    
      dimg(2) = 0.0                                                     
      dreal(3) = 1.0                                                    
      dimg(3) = 1.0                                                     
      space=0.15                                                        
      size=0.10                                                         
*     call grid (1.,.5,10*ifix(tlenth),.1,80,.1,'88888888'X)            
*     call grid (1.,.5,   ifix(tlenth),1.,8 ,1.,'AAAAAAAA'X)            
      call grid (1.,.5,10*ifix(tlenth),.1,80,.1, 0.8)            
      call grid (1.,.5,   ifix(tlenth),1.,8 ,1., 0.0)            
      call axis( 1.0, 0.5, yaxe(1), 20, 8.0, 90.0, valmin,      
     1           scdiv )                                                
      call axis( 1.0, 0.5, time, -8, tlenth, 0.0, tmin, dt )    
      if (ktcom .ne. 0) then                                            
         do 600 k=1,ktcom                                               
         k1=(k-1)*8+1                                                   
         call symbol( xcom, ycom, .0857, clf(k1), 0.0, 80 )     
  600    ycom=ycom-space                                                
      end if                                                            
                                                                        
      if (py.lt.0.0) py=0.0                                             
      call plot (1.0,0.5,-3)                                            
      call plot (0.0,py,3)                                              
      call plot (tlenth,py,2)                                           
      call plot (0.0,8.0,3)                                             
      call plot (0.0,0.0,2)                                             
      ynam=ycom-0.5-space                                               
      ysym(1)=ynam+.075                                                 
      do 800 itrr = 1,ktplot                                            
      ibusc = idgenc(1,itrr)                                            
      ibase = idgenn(1,itrr)                                            
      base=basekv(ibase)                                                
      write (blf,650) ibusc,base                                        
  650 format (a8,2x,f5.1,5x)                                            
      call line (xsym,ysym,1,1,1,itrr)                                  
      call symbol( xnam, ynam, size, blf, 0.0, 20 )             
  750 ysym(1)=ysym(1)-space                                             
      ynam=ynam-space                                                   
      indx = itemp(itrr)                                                
      if(ibussw .eq. 4)then                                             
         do 760 jtrr = 1,icount                                         
         worksp(jadr+jtrr) = busmw(indx,jtrr)                           
 760     continue                                                       
      endif                                                             
      if(ibussw .eq. 5)then                                             
         do 770 jtrr = 1,icount                                         
         worksp(jadr+jtrr) = busvar(indx,jtrr)                          
 770     continue                                                       
      endif                                                             
      worksp(jadr+icount +1) = valmin                                   
      worksp(jadr+icount +2) = scdiv                                    
      call line(t,worksp(jadr+1),icount,1,nsymskp,itrr)                       
  800 continue                                                          
      call plot(0.0,0.0,-999)                                               
      if (i .gt. nmx) go to 850                                         
      ktplot=0                                                          
      go to 300                                                         
  850 continue                                                          
C                                                                       
C       GO ON TO PLOT REACTIVE BUS LOAD                                 
C                                                                       
      if (ibussw.eq.4) go to 100                                        
      return                                                            
      end                                                               
