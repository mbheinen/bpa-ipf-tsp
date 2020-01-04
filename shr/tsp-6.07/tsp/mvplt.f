C    %W% %G%
      subroutine mvplt                                                  
C                                                                       
C       THIS SUBROUTINE DECODES PLOTS THE TWO VARIABLE DATA             
C       REQUESTED ON THE MV CARDS.  IT IS CALLED BY CALPLT.             
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/nwgntn.inc' 
      include 'tspinc/ovly6.inc' 
      include 'tspinc/lindat.inc' 
      include 'tspinc/mvn.inc' 
      include 'tspinc/mvpltn.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/dc.inc' 
      include 'tspinc/reread.inc' 


      character * 10 yzplt(6),ybus(4),ylin(6),ydc(18),ylabl(28)         
      character * 10 xzplt(4)                                           
      character * 10 zzplt(4)                                           
      character*8 name1,name2                                           
      character * 8 time                                                
C                                                                       
C         YLABL IS THE TABLE CONTAINING ALL Y-AXIS LABELS               
C              APPARENT IMPEDANCE GRAPH                                 
        data yzplt /   '   APPAREN',  'T Z-IMAG  ','   APPAREN','T Z-DOT
     1','   apparen','T   R-DOT   '/                                    
      data zzplt /   '  APPARENT' , ' Z-REAL   ' , ' Z-MAG ',           
     1' R-MAG   '/                                                      
C              BUS VOLTAGE AND FREQUENCY GRAPH                          
        data ybus  / '   BUS VOL', 'TAGE PU   ' ,'  BUS FREQ',          
     1                   ' DEV HERTZ'/                                  
C             LINE MW AND MVAR GRAPHS                                   
      data ylin  / '    LINE F','LOW MW    ','   LINE FL',              
     1             'OW MVAR','   LINE CU','RRENT AMP '/                 
C             DC FIRING ANGLE, CURRENT AND POWER                        
      data ydc  / 'FIRING ANG','LE DEGREES','DC CURRENT',               
     1' KILOAMPS ','      DC V','OLTS KV   ','MODSIG1   ','MODSIG2   ', 
     2'EXTINCTION',' ANGLE DEG',' V ALPHA P','ER UNIT   ',              
     3'V ALPHA PR','IME PU    ','OVERLAP AN','GLE DEG   ',              
     4'DC POWER  ','MW        '/                                        
C             GENERATOR GRAPHS                                          
      data ylabl/   'REL ANGLE ', 'DEGREES   ', ' GEN FREQ ',
     &              'DEV HERTZ ', '    FIELD ', 'VOLTS PU  ',
     &              '     FLUX ', 'LINKAGE   ', '  MAIN FIE',
     &              'LD SAT PU ', '   TURBINE', ' POWERMW  ',
     &              ' GENERATOR', ' POWER MW ', ' EXCITATIO',
     &              'N SAT PU  ', ' REG OUTPU', 'T VOLT PU ',
     &              ' ACCELERAT', 'ING PWR MW', '   GENERAT', 
     &              'OR MVAR   ', 'EXCITER SU', 'PP SIG PU ',
     &              ' TORQUEDAM', 'PING MW   ', ' FIELD CUR', 
     &              'RENT PU   '  /                                
c     -  Already initizlized above
c     DATA ZZPLT /   '  APPARENT' , ' Z-REAL   ' , ' Z-MAG ',           !dem
c    1' R-MAG   '/                                                      !dem
      data time /' CYCLES '/                                            
      do 1000 n= 1,mvkt                                                 
      l = 2*n-1                                                         
C                                                                       
C       PROCESSING THE Y AXIS VARIABLE                                  
C                                                                       
      isw=0                                                             
C                                                                       
C       SWITCH FOR USER-SUPPLIED MAX AND MIN                            
C       IF ISW = 0 NO USER SUPPLIED LIMITS                              
C       IF ISW = 1 USER HAS SUPPLIED LIMITS ON GH CARD                  
C                                                                       
      ind = mvopt(l)                                                    
C                                                                       
C        IND IS THE OUTPUT OPTION REQUESTED                             
C                                                                       
      if(mvcde(l) .eq. 'G'.or. mvcde(l) .eq. 'Y')then                   
         do 200 jj=1,14                                                 
         if (igencl(jj).ne.ind) go to 200                               
         if( (genmax(jj).eq.0.0) .and. (genmin(jj).eq.0.0))go to 200    
C                                                                       
C       IF THERE IS A GENMAX OR GENMIN, THEN ASSUME BOTH                
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         ymin=genmin(jj)                                                
         ymax=genmax(jj)                                                
         isw = 1                                                        
 200     continue                                                       
      endif                                                             
      if(mvcde(l) .eq. 'B')then                                         
         do 300 jj=1,2                                                  
         if (ibuscl(jj).ne.ind) go to 300                               
         if( (busmax(jj).eq.0.0) .and. (busmin(jj).eq.0.0))go to 300    
C                                                                       
C       IF THERE IS A BUSMAX OR BUSMIN, THEN ASSUME BOTH                
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         ymin=busmin(jj)                                                
         ymax=busmax(jj)                                                
         isw = 1                                                        
 300     continue                                                       
      endif                                                             
      if(mvcde(l) .eq. 'L')then                                         
         il = mvgen(l)                                                  
         if(ind .eq. 1)then                                             
            gmax = brnmax(1)                                            
            gmin = brnmin(1)                                            
            yaxe(1)( 1:10) = ylin(1)                                           
            yaxe(1)(11:20) = ylin(2)                                           
          endif                                                         
         if(ind .eq. 2)then                                             
            gmax = brnmax(2)                                            
            gmin = brnmin(2)                                            
            yaxe(1)( 1:10) = ylin(3)                                           
            yaxe(1)(11:20) = ylin(4)                                           
          endif                                                         
         if(ind .eq. 5)then                                             
            gmax = brnmax(3)                                            
            gmin = brnmin(3)                                            
            yaxe(1)( 1:10) = ylin(5)                                           
            yaxe(1)(11:20) = ylin(6)                                           
          endif                                                         
         if(ind .eq. 3)then                                             
            gmax = bndatn(8,1,il)                                       
            gmin = bndatn(8,2,il)                                       
            yaxe(1)( 1:10) = zzplt(1)                                          
            yaxe(1)(11:20) = zzplt(4)                                          
          endif                                                         
         if(ind .eq. 4)then                                             
            gmax = bndatn(9,1,il)                                       
            gmin = bndatn(9,2,il)                                       
            yaxe(1)( 1:10) = yzplt(1)                                          
            yaxe(1)(11:20) = yzplt(2)                                          
          endif                                                         
         if(ind .eq. 6)then                                             
            gmax = bndatn(8,3,il)                                       
            gmin = bndatn(8,4,il)                                       
            yaxe(1)( 1:10) = zzplt(1)                                          
            yaxe(1)(11:20) = zzplt(3)                                          
          endif                                                         
         if(ind .eq. 7)then                                             
            gmax = bndatn(9,3,il)                                       
            gmin = bndatn(9,4,il)                                       
            yaxe(1)( 1:10) = yzplt(3)                                          
            yaxe(1)(11:20) = yzplt(4)                                          
          endif                                                         
         if(ind .eq. 8)then                                             
            gmax = bndatn(9,3,il)                                       
            gmin = bndatn(9,4,il)                                       
            yaxe(1)( 1:10) = yzplt(5)                                          
            yaxe(1)(11:20) = yzplt(6)                                          
          endif                                                         
          if(gmax .ne. 0.0 .or. gmin .ne. 0.0)then                      
             isw =1                                                     
             ymax = gmax                                                
             ymin = gmin                                                
          endif                                                         
      endif                                                             
      if(mvcde(l) .eq. 'D')then                                         
         do 350 jj=1,8                                                  
         if (idcl(jj).ne.ind) go to 350                                 
         if( (dcmax(jj).eq.0.0) .and. (dcmin(jj).eq.0.0))go to 350      
C                                                                       
C       IF THERE IS A DCMAX OR DCMIN, THEN ASSUME BOTH                  
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         ymin=dcmin(jj)                                                 
         ymax=dcmax(jj)                                                 
         isw = 1                                                        
 350     continue                                                       
      endif                                                             
      if( isw .eq. 1) then                                              
C                                                                       
C       SCDIVY = UNITS PER INCH, ON 8-INCH AXIS                         
C                                                                       
         scdivy=(ymax-ymin)/8.0                                         
         vlymin=ymin                                                    
         kxp=1                                                          
C                                                                       
C       PY = NUMBER OF INCHES ABOVE ZERO                                
C                                                                       
         py=-vlymin/scdivy                                              
         if (ymax.eq.0.0) py=8.0                                        
C                                                                       
C       PROCESSING FOR USER-SUPPLIED MAX AND MIN . . .                  
C       IN THIS LOOP, SIFT THRU THE DATA TO SEE IF IT                   
C       STEPS OUT OF BOUNDS, THEN TRUNCATE IT.                          
C                                                                       
         do 370 jj=1,icount                                             
         jindx =mvadr(l)+jj                                             
         if (vspce(jindx).gt.ymax) vspce(jindx)=ymax                    
  370    if (vspce(jindx).lt.ymin) vspce(jindx)=ymin                    
      else                                                              
C                                                                       
C       IF THERE ARE NO USER SUPPLIED LIMITS,                           
C       CALL SCAL TO GET THE MINIMUM (VLYMIN), AND                      
C       UNITS PER INCH (SCDIVY)                                         
C                                                                       
         ymax = vspce(mvadr(l) +icount+1)                               
         ymin = vspce(mvadr(l) +icount+2)                               
         smax=ymax                                                      
         smin=ymin                                                      
         trnge=smax-smin                                                
         call scal                                                      
         scdivy = scdiv                                                 
         vlymin = valmin                                                
      endif                                                             
C                                                                       
C        FILL IN THE AXIS LABELS                                        
C                                                                       
      dy=scdivy                                                         
      id1=2*ind-1                                                       
      if(mvcde(l) .eq. 'G')then                                         
         yaxe(1)( 1:10)=ylabl(id1)                                             
         yaxe(1)(11:20)=ylabl(id1+1)                                           
         write(blfmv(1),400)mvnm11(l),pkv11(l),mvid(l)                  
 400     format(' Y AXIS IS GENERATOR ',1x,a8,1x,f5.1,1x,a1)            
      endif                                                             
      if(mvcde(l) .eq. 'Y')then                                         
         yaxe(1)( 1:10)=ylabl(id1)                                             
         yaxe(1)(11:20)=ylabl(id1+1)                                           
         k2 = mvgen2(l)                                                 
      ibase = nwgntn(k2)                                                
         base1 = basekv(ibase)                                          
         write(blfmv(1),405)mvnm11(l),pkv11(l),mvid(l),                 
     1              nwgntc(1,k2),base1,nwgntc(2,k2)                     
 405     format(' Y AXIS IS DIFFERENCE BETWEEN GENERATORS ',1x,a8,      
     1          1x,f5.1,1x,a1,' AND ',a8,1x,f5.1,1x,a1)                 
      endif                                                             
      if(mvcde(l) .eq. 'B')then                                         
         yaxe(1)( 1:10)=ybus(id1)                                              
         yaxe(1)(11:20)=ybus(id1+1)                                            
         write(blfmv(1),410)mvnm11(l),pkv11(l)                          
 410     format(' Y AXIS IS BUS ',1x,a8,1x,f5.1)                        
      endif                                                             
      if(mvcde(l) .eq. 'L')then                                         
         write(blfmv(1),420)mvnm11(l),pkv11(l),mvnm12(l),pkv12(l),      
     1           mvid(l)                                                
 420     format(' Y AXIS IS LINE ',1x,a8,1x,f5.1,2x,a8,1x,f5.1,1x,a1)   
      endif                                                             
      if(mvcde(l) .eq. 'D')then                                         
         yaxe(1)( 1:10)=ydc(id1)                                               
         yaxe(1)(11:20)=ydc(id1+1)                                             
         write(blfmv(1),430)mvnm11(l),pkv11(l)                          
 430     format(' Y AXIS IS DC BUS ',1x,a8,1x,f5.1)                     
      endif                                                             
C                                                                       
C       PROCESSING THE X AXIS VARIABLE                                  
C                                                                       
 500  isw=0                                                             
C                                                                       
C       SWITCH FOR USER-SUPPLIED MAX AND MIN                            
C       IF ISW = 0 NO USER SUPPLIED LIMITS                              
C       IF ISW = 1 USER HAS SUPPLIED LIMITS ON GH CARD                  
C                                                                       
      ind = mvopt(l+1)                                                  
C                                                                       
C        IND IS THE GENERATOR OPTION REQUESTED                          
C                                                                       
      if(mvcde(l+1) .eq. 'G'.or. mvcde(l+1) .eq. 'Y')then               
         do 600 jj=1,14                                                 
         if (igencl(jj).ne.ind) go to 600                               
         if( (genmax(jj).eq.0.0) .and. (genmin(jj).eq.0.0))go to 600    
C                                                                       
C       IF THERE IS A GENMAX OR GENMIN, THEN ASSUME BOTH                
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         xmin=genmin(jj)                                                
         xmax=genmax(jj)                                                
         isw = 1                                                        
 600     continue                                                       
      endif                                                             
      if(mvcde(l+1) .eq. 'B')then                                       
         do 700 jj=1,2                                                  
         if (ibuscl(jj).ne.ind) go to 700                               
         if( (busmax(jj).eq.0.0) .and. (busmin(jj).eq.0.0))go to 700    
C                                                                       
C       IF THERE IS A BUSMAX OR BUSMIN, THEN ASSUME BOTH                
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         xmin=busmin(ind)                                               
         xmax=busmax(ind)                                               
         isw = 1                                                        
 700     continue                                                       
      endif                                                             
      if(mvcde(l+1) .eq. 'L')then                                       
         il = mvgen(l+1)                                                
         if(ind .eq. 1)then                                             
            gmax = brnmax(1)                                            
            gmin = brnmin(1)                                            
            xaxe(1) = ylin(1)                                           
            xaxe(2) = ylin(2)                                           
          endif                                                         
         if(ind .eq. 2)then                                             
            gmax = brnmax(2)                                            
            gmin = brnmin(2)                                            
            xaxe(1) = ylin(3)                                           
            xaxe(2) = ylin(4)                                           
          endif                                                         
         if(ind .eq. 5)then                                             
            gmax = brnmax(3)                                            
            gmin = brnmin(3)                                            
            xaxe(1) = ylin(5)                                           
            xaxe(2) = ylin(6)                                           
          endif                                                         
         if(ind .eq. 3)then                                             
            gmax = bndatn(8,1,il)                                       
            gmin = bndatn(8,2,il)                                       
            xaxe(1) = zzplt(1)                                          
            xaxe(2) = zzplt(4)                                          
          endif                                                         
         if(ind .eq. 4)then                                             
            gmax = bndatn(9,1,il)                                       
            gmin = bndatn(9,2,il)                                       
            xaxe(1) = yzplt(1)                                          
            xaxe(2) = yzplt(2)                                          
          endif                                                         
         if(ind .eq. 6)then                                             
            gmax = bndatn(8,3,il)                                       
            gmin = bndatn(8,4,il)                                       
            xaxe(1) = zzplt(1)                                          
            xaxe(2) = zzplt(3)                                          
          endif                                                         
         if(ind .eq. 7)then                                             
            gmax = bndatn(9,3,il)                                       
            gmin = bndatn(9,4,il)                                       
            xaxe(1) = yzplt(3)                                          
            xaxe(2) = yzplt(4)                                          
          endif                                                         
         if(ind .eq. 8)then                                             
            gmax = bndatn(9,3,il)                                       
            gmin = bndatn(9,4,il)                                       
            xaxe(1) = yzplt(5)                                          
            xaxe(2) = yzplt(6)                                          
          endif                                                         
          if(gmax .ne. 0.0 .or. gmin .ne. 0.0)then                      
             isw =1                                                     
             xmax = gmax                                                
             xmin = gmin                                                
          endif                                                         
      endif                                                             
      if(mvcde(l+1) .eq. 'D')then                                       
         do 720 jj=1,8                                                  
         if (idcl(jj).ne.ind) go to 720                                 
         if( (dcmax(jj).eq.0.0) .and. (dcmin(jj).eq.0.0))go to 720      
C                                                                       
C       IF THERE IS A GENMAX OR GENMIN, THEN ASSUME BOTH                
C       MAXIMUM AND MINIMUM ARE USER-SUPPLIED.                          
C       THEREFORE ISW=1.                                                
C                                                                       
         xmin=dcmin(jj)                                                 
         xmax=dcmax(jj)                                                 
         isw = 1                                                        
 720     continue                                                       
      endif                                                             
      if( isw .eq. 1) then                                              
C                                                                       
C       SCDIFX = UNITS PER INCH, ON 8-INCH AXIS                         
C                                                                       
         scdivx=(xmax-xmin)/8.0                                         
         vlxmin=xmin                                                    
         kxp=1                                                          
         isw=1                                                          
C                                                                       
C       PROCESSING FOR USER-SUPPLIED MAX AND MIN . . .                  
C       IN THIS LOOP, SIFT THRU THE DATA TO SEE IF IT                   
C       STEPS OUT OF BOUNDS, THEN TRUNCATE IT.                          
C                                                                       
         do 750 jj=1,icount                                             
         jindx =mvadr(l+1)+jj                                           
         if (vspce(jindx).gt.xmax) vspce(jindx)=xmax                    
  750    if (vspce(jindx).lt.xmin) vspce(jindx)=xmin                    
      else                                                              
C                                                                       
C       IF THERE ARE NO USER SUPPLIED LIMITS,                           
C       CALL SCAL TO GET THE MINIMUM (VLXMIN), AND                      
C       UNITS PER INCH (SCDIVX)                                         
C                                                                       
         xmax = vspce(mvadr(l+1) +icount+1)                             
         xmin = vspce(mvadr(l+1) +icount+2)                             
         if(mvcde(l+1) .eq. 'T')then                                    
            xmax = tmax                                                 
            xmin = tmin                                                 
         endif                                                          
         smax=xmax                                                      
         smin=xmin                                                      
         trnge=smax-smin                                                
         call scal                                                      
         scdivx = scdiv                                                 
         vlxmin = valmin                                                
      endif                                                             
C                                                                       
C        FILL IN THE AXIS LABEL                                         
C                                                                       
      id1=2*ind-1                                                       
      if(mvcde(l+1) .eq. 'G')then                                       
         xaxe(1)=ylabl(id1)                                             
         xaxe(2)=ylabl(id1+1)                                           
         write(blfmv(2),900)mvnm11(l+1),pkv11(l+1),mvid(l+1)            
  900    format(' X AXIS IS GENERATOR ',1x,a8,1x,f5.1,1x,a1)            
      endif                                                             
      if(mvcde(l+1) .eq. 'Y')then                                       
         xaxe(1)=ylabl(id1)                                             
         xaxe(2)=ylabl(id1+1)                                           
         k2 = mvgen2(l+1)                                               
      ibase = nwgntn(k2)                                                
         base1 = basekv(ibase)                                          
         write(blfmv(2),905)mvnm11(l+1),pkv11(l+1),mvid(l+1),           
     1              nwgntc(1,k2),base1,nwgntc(2,k2)                     
 905     format(' Y AXIS IS DIFFERENCE BETWEEN GENERATORS ',1x,a8,      
     1          1x,f5.1,1x,a1,' AND ',a8,1x,f5.1,1x,a1)                 
      endif                                                             
      if(mvcde(l+1) .eq. 'B')then                                       
         xaxe(1)=ybus(id1)                                              
         xaxe(2)=ybus(id1+1)                                            
         write(blfmv(2),910)mvnm11(l+1),pkv11(l+1)                      
  910    format(' X AXIS IS BUS ',1x,a8,1x,f5.1)                        
      endif                                                             
      if(mvcde(l+1) .eq. 'L')then                                       
         write(blfmv(2),920)mvnm11(l+1),pkv11(l+1),mvnm12(l+1),         
     1          pkv12(l+1), mvid(l+1)                                   
 920     format(' X AXIS IS LINE ',1x,a8,1x,f5.1,2x,a8,1x,f5.1,1x,a1)   
      endif                                                             
      if(mvcde(l+1) .eq. 'D')then                                       
         xaxe(1)=ydc(id1)                                               
         xaxe(2)=ydc(id1+1)                                             
         write(blfmv(2),930)mvnm11(l+1),pkv11(l+1)                      
  930    format(' X AXIS IS DC BUS ',1x,a8,1x,f5.1)                     
      endif                                                             
      if(mvcde(l+1) .eq. 'T')then                                       
         xaxe(1) = time                                                 
         xaxe(2) = '        '                                           
      endif                                                             
      call plt2v(l)                                                     
 1000 continue                                                          
      return                                                            
      end                                                               
