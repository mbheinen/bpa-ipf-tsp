C    %W% %G%
      subroutine sptplt                                                 
c                                                                       
c     THIS SUBROUTINE PLOTS THE SPARE DATA POINTS REQUESTED ON THE   
c     SP CARD.  IT IS CALLED BY CALPLT                               
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/comn56.inc' 
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
      include 'tspinc/spare1.inc' 
      include 'tspinc/spare2.inc' 
      data time /' CYCLES '/                                            

      do 1000 jj= 1,ispknt                                              
c                                                                       
c     CHECK TO SEE IF THIS OPTION IS TO BE PLOTTED                    
c                                                                       
      if(ispcde(jj) .eq. 0)go to 1000                                   
      ymax = 0.0                                                        
      ymin = 0.0                                                        
      isw=0                                                             
c                                                                       
c     LOOP THRU DATA AND PICK OUT MAX AND MIN POINTS                  
c                                                                       
      do 100 ll = 1,icount                                              
      if(spdata(ll,jj) .gt. ymax)ymax = spdata(ll,jj)                   
      if(spdata(ll,jj) .lt. ymin)ymin = spdata(ll,jj)                   
  100 continue                                                          
      spdata(icount+1,jj) = ymax                                        
      spdata(icount+2,jj) = ymin                                        
c                                                                       
c     CHECK USER-REQUESTED MAX AND MINS TO SEE IF THIS                
c     OPTION HAS EXTERNAL LIMITS                                      
c                                                                       
      if( (spmax(jj).eq.0.0) .and. (spmin(jj).eq.0.0) )go to 250        
c                                                                       
c     IF SPMAX EXISTS, EXTERNAL LIMITS HAVE BEEN SUPPLIED            
c     AND ISW = 1                                                    
c                                                                       
      if( spmax(jj) .ne.0.0)go to 150                                   
      if( sign(1.,spmax(jj)) .gt.0.0)go to 150                          
      if( sign(1.,spmin(jj)) .gt.0.0)go to 150                          
      go to 250                                                         
c                                                                       
c     GET THE USER-SUPPLIED VALUES                               
c                                                                       
  150 ymin = spmin(jj)                                                  
      ymax = spmax(jj)                                                  
c                                                                       
c     SCDIV = UNITS PER INCH, ON 8-INCH AXIS                          
c                                                                       
      isw=1                                                             
      scdiv=(ymax-ymin)/8.                                              
      valmin=ymin                                                       
      kxp=3                                                             
c                                                                       
c     PY = NUMBER OF INCHES ABOVE ZERO                                
c                                                                       
      if (scdiv .gt. 0.0) then
         py = -valmin/scdiv                                                  
      else
         py = 0.0
      endif
 250  if (isw.ne.1) go to 400                                           
c                                                                       
c     IF MAX AND MIN ARE USER SUPPLIED, CHECK DATA TO SEE IF IT       
c     EXCEEDS LIMITS AND TRUNCATE.                                    
c                                                                       
      do 350 kk = 1,icount                                              
      if(spdata(kk,jj) .gt. ymax)spdata(kk,jj) = ymax                   
      if(spdata(kk,jj) .lt. ymin)spdata(kk,jj) = ymin                   
  350 continue                                                          
  400 if (isw.eq.1) go to 550                                           
c                                                                       
c     THIS SECTION DOES AUTMATIC SCALING IF                      
c     THE MAX AND MIN ARE NOT SUPPLIED IN DATA.                  
c                                                                       
      ymax = 0.0                                                        
      ymin = 0.0                                                        
c                                                                       
c     LOOP THROUGH THE PLOT DATA TO GET THE MAXIMA               
c     AND MINIMA FOR ALL PLOTS, TO FIND THE MAXIMUM              
c     MAXIMUM AND THE MINIMUM MINIMUM.  THIS IS SO               
c     THAT THEY ALL CAN FIT ON THE SAME GRAPH.                   
c                                                                       
      ylmt(1) = spdata(icount+1,jj)                                     
      ylmt(2) = spdata(icount+2,jj)                                     
      if (ylmt(1).gt.ymax) ymax = ylmt(1)                               
      if (ylmt(2).lt.ymin) ymin = ylmt(2)                               
      smax=ymax                                                         
      smin=ymin                                                         
      trnge=smax-smin                                                   
c                                                                       
c     DONT PLOT THIS DATA, IF THE RANGE IS CLOSE TO ZERO         
c                                                                       
      if (abs(trnge).lt..001) go to 1000                                
c                                                                       
c     CALL SCAL TO CALCULATE MAXIMUM, MINIMUM, AND UNITS PER INCH     
c                                                                       
      call scal                                                         
c                                                                       
c     FILL IN THE AXIS LABELS AND DO THE GRAPH                        
c                                                                       
  550 dy=scdiv                                                          
      yaxe(1)( 1:10) = spaxis(1,jj)                                            
      yaxe(1)(11:20) = spaxis(2,jj)                                            
c                                                                       
c     CALL PLOTTING ROUTINES TO FORM GRAPH                         
c     SET THE PLOT LENGTH TO THE GREATER OF                        
c     8.5 INCHES OR AXIS LENGTH PLUS ONE.  THIS MODIFICATION       
c     PLUS COMMON/TPLINK/ ALLOW THE PROGRAM TO CHANGE XMAX         
c     IN PLOTS WHICH WAS NORMALLY SET IN A DATA STATMENT THERE.    
c                                                                       
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
      call grid (1.,.5,10*ifix(tlenth),.1,80,.1, 0.8)            
*     call grid (1.,.5,   ifix(tlenth),1.,8 ,1.,'AAAAAAAA'X)            
      call grid (1.,.5,   ifix(tlenth),1.,8 ,1.,0.0)            
      call axis( 1.0, 0.5, yaxe(1), 20, 8.0, 90.0, valmin,      
     1           scdiv )                                                
      call axis( 1.0, 0.5, time, -8, tlenth, 0.0, tmin, dt )    
                                                                        
      if ( ktcom .ne. 0 ) then                                          
         do 600 k=1,ktcom                                               
         k1=(k-1)*8+1                                                   
         call symbol( xcom, ycom, .0857, clf(k1), 0.0, 80 )     
  600    ycom=ycom-space                                                
      end if                                                            
                                                                        
      if ( py .lt. 0.0 ) py = 0.0                                       
      call plot( 1.0, 0.5, -3 )                                         
      call plot( 0.0, py, 3 )                                           
      call plot( tlenth, py, 2 )                                        
      call plot( 0.0, 8.0, 3 )                                          
      call plot( 0.0, 0.0, 2 )                                          
      ynam=ycom-0.5-space                                               
      ysym(1)=ynam+.075                                                 
      ynam=ynam-space                                                   
                                                                        
      do 770 jtrr = 1,icount                                            
         worksp(jadr+jtrr) = spdata(jtrr,jj)                            
 770  continue                                                          
                                                                        
      worksp(jadr+icount +1) = valmin                                   
      worksp(jadr+icount +2) = scdiv                                    
      call line(t,worksp(jadr+1),icount,1,nsymskp,itrr)                       
      call plot(0.0,0.0,-999)                                               
 1000 continue                                                          
      return                                                            
      end                                                               
