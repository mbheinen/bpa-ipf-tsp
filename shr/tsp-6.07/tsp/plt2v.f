C    %W% %G%
      subroutine plt2v(lopt)                                            
c                                                                       
c       THIS SUBROUTINE MAKES THE NECESSARY CALLS TO THE PLOT           
c       LIBRARY SUBROUTINES TO FORM A PLOT OUTPUT FILE FOR              
c       TWO OPTION PLOTS.  IT IS CALLED BY MVPLT.                       
c       LOPT IS THE REQUESTED OPTION BEING PLOTTED                      
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/dc.inc' 
      include 'tspinc/ovly6.inc' 
      include 'tspinc/lindat.inc' 
      include 'tspinc/mvpltn.inc' 
      include 'tspinc/mvn.inc' 
      dimension xsym(3),ysym(3)                                         
      character * 8 mdir,ibus1c,ibus2c,idummc,ibusc,ipar,id             
      character * 10 tic                                                
      call plots(1,0,0)                                              
      xlenth = 8.0                                                      
      if(mvcde(lopt+1) .eq. 'T')then                                    
         vlxmin = t(icount+1)                                           
         scdivx = t(icount+2)                                           
         xlenth = tlenth                                                
      endif                                                             
      xcom=1.0                                                          
      ycom=9.85                                                         
      xnam=1.0                                                          
      xsym(1)=0.5                                                       
      xsym(2) = 0.0                                                     
      ysym(2) = 0.0                                                     
      xsym(3) = 1.0                                                     
      ysym(3) = 1.0                                                     
      space=0.15                                                        
      size=0.10                                                         
* * *                                                                   
* * * DRAW X AND Y AXIS AND GRID                                        
* * *                                                                   
*     call grid (1.,.5,10*ifix(xlenth),.1,80,.1,'88888888'X)            
      call grid (1.,.5,10*ifix(xlenth),.1,80,.1, 0.8)            
*     call grid (1.,.5,   ifix(xlenth),1.,8 ,1.,'AAAAAAAA'X)            
      call grid (1.,.5,   ifix(xlenth),1.,8 ,1., 0.0)            
      call axis( 1.0, 0.5, yaxe(1), 20, 8.0, 90.0, vlymin,      
     1           scdivy )                                               
      call axis( 1.0, 0.5, xaxe(1), -20, xlenth, 00.0, vlxmin,  
     1           scdivx)                                                
c                                                                       
c       PRINT COMMENT CARDS AT TOP OF PLOT                              
c                                                                       
      if (ktcom .ne. 0) then                                            
         do k=1,ktcom                                               
            k1=(k-1)*8+1                                                
            call symbol( xcom, ycom, .0857, clf(k1), 0.0, 80 )  
            ycom=ycom-space                                             
         end do
      endif                                                            
                                                                        
      ihdsw=1                                                           
      if (py.lt.0.0) then 
        py=0.0                                             
      endif
      call plot (1.0,0.5,-3)                                            
      call plot (0.0,py,3)                                              
      call plot (xlenth,py,2)                                           
      call plot (0.0,8.0,3)                                             
      call plot (0.0,0.0,2)                                             
      ynam=ycom-0.5-space                                               
      ysym(1)=ynam+.075                                                 
      call line(xsym,ysym,1,1,1,1)                                      
c                                                                       
c       WRITE DESCRIPTION FOR Y AXIS                                    
c                                                                       
      call symbol( xnam, ynam, size, blfmv(1), 0.0, 80 )        
      ysym(1)=ysym(1)-space                                             
      ynam=ynam-space                                                   
c                                                                       
c       WRITE DESCRIPTION FOR X AXIS                                    
c                                                                       
      if(mvcde(lopt+1) .ne. 'T')then                                    
         call symbol( xnam, ynam, size, blfmv(2), 0.0, 80 )     
         ixstrt = mvadr(lopt+1)                                          
      else                                                              
         ixstrt = 0                                                      
      endif                                                            
                                                                        
      iystrt = mvadr(lopt)                                               
c                                                                       
c       ixstrt IS THE STARTING ADDRESS IN VSPCE FOR THE X AXIS DATA      
c       iystrt IS THE STARTING ADDRESS IN VSPCE FOR THE Y AXIS DATA      
c       VLXMIN IS THE MINIMUM VALUE OF THE X AXIS QUANTITY              
c       VLYMIN IS THE MINIMUM VALUE OF THE Y AXIS QUANTITY              
c       SCDIVX IS THE UNITS/INCH FOR THE X AXIS QUANTITY                
c       SCDIVY IS THE UNITS/INCH FOR THE Y AXIS QUANTITY                
c                                                                       
 360  vspce(iystrt+icount+1)=vlymin                                      
      vspce(iystrt+icount+2)=scdivy                                      
c                                                                       
c       DRAW THE CURVE FOR ICOUNT DATA POINTS                           
c                                                                       
      if(mvcde(lopt+1) .ne. 'T')then                                    
         vspce(ixstrt+icount+1)=vlxmin                                   
         vspce(ixstrt+icount+2)=scdivx                                   
         call line (vspce(ixstrt+1),vspce(iystrt+1),icount,1,1,1)         
      else                                                              
         call line (t(ixstrt+1),vspce(iystrt+1),icount,1,1,1)             
      endif                                                             
c                                                                       
c       LABEL POINTS ON CURVE WITH TIME IN CYCLES                       
c       MAKE NTIM A VARIABLE SOME DAY                                   
c                                                                       
      if(mvcde(lopt+1) .eq. 'T') then 
        go to 500                               
      endif
      ntim = 4                                                          
                                                                        
      if(ntim .ne. 0) then                                              
         do ki=1,icount,ntim                                        
           yin=((vspce(iystrt+ki)-vlymin)/scdivy)+0.1                      
           xin=(vspce(ixstrt+ki)-vlxmin)/scdivx                            
           write (tic,380) t(ki)                                          
  380      format(f6.1,6x)                                                
           call symbol( xin, yin, size, tic, 0.0, 10 )            
         enddo
      endif                                                            
                                                                        
  500 call plot( 0.0, 0.0, -999 )                                           
      return                                                            
      end                                                               
