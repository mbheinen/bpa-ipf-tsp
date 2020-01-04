C    %W% %G%
      subroutine plt                                                    
c                                                                       
c       THIS SUBROUTINE MAKES THE NECESSARY CALLS TO THE PLOT           
c       LIBRARY SUBROUTINES TO FORM A PLOT OUTPUT FILE                  
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/rddat.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/comn56.inc' 
      dimension xclf(15)                                                
      character*80 xclf                                                 
      equivalence (xclf,clf)                                            
      include 'tspinc/link56.inc' 
      include 'tspinc/dc.inc' 
      include 'tspinc/ovly6.inc' 
      include 'tspinc/nwgntn.inc' 
      include 'tspinc/newtab.inc' 
      include 'tspinc/lindat.inc' 
      include 'tspinc/idgen.inc' 
      include 'tspinc/dcname.inc' 
      include 'tspinc/out512.inc' 
      logical normal, bias                                              
      common/tplink/ paplim                                             
      dimension xsym(3),ysym(3)                                         
      dimension dreal(3),dimg(3)                                        
      character*80 blf                                                  
      character * 8 mdir,ibus1c,ibus2c,idummc,ibusc,ipar,id             
      character * 10 xzplt(4),xaxe(2)                                   
      character * 10 tic                                                
      character * 8 time                                                
      data time /' CYCLES '/                                            
      data xzplt /   '  APPARENT' , ' Z-REAL   ' , ' Z-MAG ',           
     1' R-MAG   '/                                                      
C         SET THE PLOT LENGTH TO THE GREATER OF                         
C         8.5 INCHES OR AXIS LENGTH PLUS ONE.  THIS MODIFICATION        
C         PLUS COMMON/TPLINK/ ALLOW THE PROGRAM TO CHANGE XMAX          
C         IN PLOTS WHICH WAS NORMALLY SET IN A DATA STATMENT THERE.     
C         A NEW PLOTS ROUTINE NOW RESIDES IN THE SWING LIBRARY.         
      paplim = 8.5                                                      
      if(tlenth.gt.7.0) paplim = tlenth + 1.5                           
      call plots( 1, 0, 0 )                                          
      normal = .false. ! PORTRAIT ORIENTATION FOR THIS PLOT             
      call pltori( normal, bias )                                       
      xaxe(1) = xzplt(1)                                                
      xaxe(2) =xzplt(ilinsw+1)                                          
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
      if (ipltsw.eq.3) go to 140                                        
*     call grid (1.,.5,10*ifix(tlenth),.1,80,.1,'88888888'X)            
*     call grid (1.,.5,   ifix(tlenth),1.,8 ,1.,'AAAAAAAA'X)            
      call grid (1.,.5,10*ifix(tlenth),.1,80,.1, 0.8)            
      call grid (1.,.5,   ifix(tlenth),1.,8 ,1., 0.0)            
      call axis( 1.0, 0.5, time, -8, tlenth, 0.0, tmin, dt )    
      go to 160                                                         
  140 call grid (1.,.5,10*ifix(tlenth),.1,80,.1, 0.8)            
      call grid (1.,.5,   ifix(tlenth),1.,8 ,1., 0.0)            
* 140 call grid (1.,.5,10*ifix(tlenth),.1,80,.1,'88888888'X)            
*     call grid (1.,.5,   ifix(tlenth),1.,8 ,1.,'AAAAAAAA'X)            
      call axis( 1.0, 0.5, xaxe(1), -20, tlenth, 0.0, xmin, dx )
160   continue                                                          
      call axis( 1.0, 0.5, yaxe(1), 20, 8.0, 90.0, valmin,      
     1           scdiv )                                                
      if (ktcom.eq.0) go to 200                                         
                                                                        
      do 180 k = 1, ktcom                                               
         k1 = (k-1)*8 + 1                                               
         call symbol( xcom, ycom, .0857, xclf(k), 0.0, 80 )     
  180    ycom = ycom - space                                            
  200 continue                                                          
                                                                        
      ihdsw=1                                                           
      if ( py .lt. 0.0 ) py = 0.0                                       
      call plot( 1.0, 0.5, -3 )                                         
      call plot( 0.0, py, 3 )                                           
      call plot( tlenth, py, 2 )                                        
      call plot( 0.0, 8.0, 3 )                                          
      call plot( 0.0, 0.0, 2 )                                          
      ynam=ycom-0.5-space                                               
      ysym(1)=ynam+.075                                                 
      do 420 i=1,ktplot                                                 
C           THIS CORRECTS TJHE BUG IN THE 1900 BUS TEST CASE            
            if(idgenn(1, i) .eq. 0) go to 420                           
C       <>  SEE SUBROUTINE CALPLT FOR A DESCRIPTION OF HOW  <>          
C       <>  IDGEN IS FILLED.  THE INFORMATION IT CONTAINS IS<>          
C       <>  DEPENDENT UPON HOW IPLTSW IS SET.               <>          
      if (ipltsw.ge.2) go to 240                                        
      ibusc = idgenc(1,i)                                               
      if (ipltsw.eq.1) go to 280                                        
      ibase = idgenn(1,i)                                               
      base=basekv(ibase)                                                
      id = idgenc(2,i)                                                  
      write (blf,220) ibusc,base,id                                     
  220 format (a8,2x,f5.1,2x,a1,2x)                                      
      go to 320                                                         
  240 if(ipltsw.ge.4) go to 310                                         
      ibus1 = idgenn(1,i)                                               
      ibus2 = idgenn(2,i)                                               
      ipar = idgenc(1,i)                                                
C       <>  THE ARRAY NEWTAB WAS WRITTEN AT KNEWT IN  <>                
C       <>  OUTPUT2.  NEW ARRAYS ARE NEWTBC AND      <>                 
C       <>  INWTB.                                  <>                  
      ibus1c = newtbc(ibus1)                                            
      ibus2c = newtbc(ibus2)                                            
      ibase1 = inwtb(ibus1)                                             
      ibase2 = inwtb(ibus2)                                             
      base1=basekv(ibase1)                                              
      base2=basekv(ibase2)                                              
      write (blf,260) ibus1c,base1,ibus2c,base2,ipar                    
  260 format (a8,2x,f5.1,2x,a8,2x,f5.1,2x,a2,4x)                        
      idum=i                                                            
      if (ipltsw.eq.3) idum=3                                           
      call line (xsym,ysym,1,1,1,idum)                                  
      call symbol( xnam, ynam, size, blf, 0.0, 40 )             
      go to 340                                                         
  280 ibase = idgenn(1,i)                                               
      base=basekv(ibase)                                                
      write (blf,300) ibusc,base                                        
  300 format (a8,2x,f5.1,5x)                                            
      go to 320                                                         
C     LOGIC TO HANDLE PLOTS FOR DC BRANCHES                             
 310  ibrn = idgenn(1,i)                                                
      idir = idgenn(2,i)                                                
      inout = idgenn(3,i)                                               
      ibus1 = idcbk2(2*ibrn-1)                                          
      ibus1c = dcnme2(2*ibrn-1)                                         
      ibus2 = idcbk2(2*ibrn)                                            
      ibus2c = dcnme2(2*ibrn)                                           
C    *   IF IDIR = 1, BRANCH AS DEFINED IN THE OUTPUT CARD IS IN THE    
C    *   SAME SENSE AS IT IS IN THE DC BRANCH TABLE.  OTHERWISE IT IS   
C    *   OPPOSITE                                                       
      if(idir.eq.1) go to 311                                           
      idumm = ibus1                                                     
      idummc = ibus1c                                                   
      ibus1 = ibus2                                                     
      ibus1c = ibus2c                                                   
      ibus2 = idumm                                                     
      ibus2c = idummc                                                   
  311 continue                                                          
C    *   INOUT=1 MEANS POWER INTO THE BRANCH AT THE LH BUS              
C    *   INOUT=2 MEANS POWER OUT OF THE BRANCH AT THE RH BUS            
      if(inout.eq.0) go to 312                                          
      if(inout .eq. 1) mdir = ' IN '                                    
      if(inout .eq. 2) mdir = 'OUT '                                    
  312 ibase1 = ibus1                                                    
      ibase2 = ibus2                                                    
      base1 = basekv(ibase1)                                            
      base2 = basekv(ibase2)                                            
      if(ipltsw.eq.5) go to 316                                         
C     CURRENT OUTPUT OPTION                                             
      write (blf,315) ibus1c,base1,ibus2c,base2                         
  315 format(a8,2x,f5.1,2x,a8,2x,f5.1,8x)                               
      go to 330                                                         
C     POWER OUTPUT OPTION                                               
  316 write (blf,317) ibusc1,base1,ibus2c,base2,mdir                    
  317 format(a8,2x,f5.1,2x,a8,2x,f5.1,2x,a4,2x)                         
      go to 330                                                         
  320 call line (xsym,ysym,1,1,1,i)                                     
      call symbol( xnam, ynam, size, blf, 0.0, 20 )             
      go to 340                                                         
  330 call line(xsym,ysym,1,1,1,i)                                      
      call symbol( xnam, ynam, size, blf, 0.0, 40 )             
  340 ysym(1)=ysym(1)-space                                             
      ynam=ynam-space                                                   
      if (ipltsw.eq.3) go to 360                                        
      kecs=kwork+(i-1)*icoun2                                           
      call redecp (work(jadr),kecs,icoun2)                              
      work(jadr+icount)=valmin                                          
      work(jadr+icount+1)=scdiv                                         
      if (ipltsw.eq.3) go to 360                                        
      call line (t,work(jadr),icount,1,nsymskp,i)                             
      go to 420                                                         
  360 work(jstart+icount+1)=xmin                                        
      work(jstart+icount+2)=dx                                          
      work(jadr+icount+1)=valmin                                        
      work(jadr+icount+2)=scdiv                                         
      if (nsym.eq.0) nsym=4                                             
      call line (work(jstart+1),work(jadr+1),icount,1,nsym,3)           
c                                                                       
c       PLOT TRIPPING LINE FOR R-RDOT RELAY IF REQUESTED                
c                                                                       
      if(ilinsw .eq. 3 .and. irdknt .ne. 0)then                         
         do 365 ltrr = 1, irdknt                                        
         if(rname1(ltrr).ne.ibus1c.or.rname2(ltrr).ne.ibus2c)go to 365  
         if(rbkv1(ltrr).ne.base1.or.rbkv2(ltrr).ne.base2)go to 365      
         if(rpar(ltrr) .ne. ipar) go to 365                             
         r1 = rzero(ltrr)/dx + px                                       
         call plot( r1, py, 3 )                                         
         r2 = r1 + valmin/(rslope(ltrr)*dx)                             
         rd2 = valmin/scdiv + py                                        
         call plot( r2, rd2, 2 )                                        
         call plot( px, py, 3 )                                         
 365     continue                                                       
      endif                                                             
                                                                        
      if(ilinsw .gt. 1) go to 2300                                      
      call plot( px, py - 0.5, 3 )                                      
      call plot( px, py + 0.5, 2 )                                      
      call plot( px - 0.5, py, 3 )                                      
      call plot( px + 0.5, py, 2 )                                      
      dreal(1)=actr/dx+px                                               
      dimg(1)=actx/scdiv+py                                             
      call plot( px, py, 3 )                                            
      call plot( dreal(1), dimg(1), 2 )                                 
      call line( dreal, dimg, 1, 1, 1, 3 )                              
 2300 continue                                                          
      if (ntim.eq.0) go to 420                                          
      do 400 ki=1,icount,ntim                                           
      yin=((work(jadr+ki)-valmin)/scdiv)+0.1                            
      xin=(work(jstart+ki)-xmin)/dx                                     
      write (tic,380) t(ki)                                             
  380 format(f6.1,6x)                                                   
  400 call symbol( xin, yin, size, tic, 0.0, 10 )               
  420 continue                                                          
      call plot( 0.0, 0.0, -999 )                                           
      return                                                            
      end                                                               
