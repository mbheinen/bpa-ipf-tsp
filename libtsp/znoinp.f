C    %W% %G%
      subroutine znoinp                                                 
c                                                                       
c       THIS SUBROUTINE FORMS THE INITAL TABLES FOR THE                 
c       ZNO ARRESTOR MODEL AND OBTAINS THE EQUIVALENT ADMITTANCES       
c       FROM THE BRANCH DATA TABLE.  IT IS CALLED BY INPUT2.            
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/tmptbl.inc' 
      include 'tspinc/znox2.inc' 
      include 'tspinc/vy1.inc' 
      include 'tspinc/vym.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc'
      include 'tspinc/vymn.inc' 

      complex yeq(2,2), y1(2,2), y2(2,2), yxy(2,2), y3(2,2), y_c, y_o,
     &        y_v, z_v
      complex * 16 yscr(3,3)      
      character*80 buf80(MAXZN)                                         
      equivalence  (buf80,izncp)                                        
      character*8 name1,name2,name3,name4,no1,no2                       
      character*1 ipori,sstyp,ipar,iparo                                
      real meto1

      data degrad/0.0174533/                                            
      data pi /3.1415927/                                               

      sqrt3 = sqrt (3.0)                                               
      iznmax = 0                                                        
      do 1000 i = 1,iznkt                                               
      read(buf80(i),100)sstyp,name1,bkv1,name2,bkv2,ipar,isec           
  100 format(bz,3x,a1,2x,a8,f4.0,1x,a8,f4.0,a1,i1)                         
      if (name1 .ne. no1) go to 50                                        
      if (name2 .ne. no2) go to 50                                        
      if (bkv1 .ne. bkv1o) go to 50                                       
      if (bkv2 .ne. bkv2o) go to 50                                       
      if (ipar .ne. iparo) go to 50                                       
      if (isec .ne. iseco) go to 50                                       
      go to 75                                                          
 50   iznmax = iznmax + 1                                               
      no1 = name1                                                       
      no2 = name2                                                       
      bkv1o = bkv1                                                      
      bkv2o = bkv2                                                      
      iparo = ipar                                                      
      iseco = isec                                                      
      kb1 = nambas(bkv1)                                                
      kb2 = nambas(bkv2)                                                
      j1 = inam(name1,kb1)                                              
      j2 = inam(name2,kb2)                                              
      if (j1.eq. 0 .or. j2 .eq.0) then                                    
         write(errbuf(1), 200) buf80(i)                                   
  200    format('0',1x,a80,'BUS NAMES OR BASE KVS DO NOT EXIST')        
         call prterr( 'E',1)                                            
         iznmax = iznmax - 1                                            
         go to 1000                                                     
       endif                                                            
       iznbus(iznmax) = j1                                              
       jznbus(iznmax) = j2                                              
       iznpar(iznmax) = ipar                                            
       iznsec(iznmax) = isec                                            
  75  if (sstyp .eq. 'A') then                                            
         read(buf80(i),150)name1,bkv1,name2,bkv2,iznpar(iznmax),        
     1   iznsec(iznmax), znxco(iznmax), cznpro(iznmax), znjou(iznmax),  
     2   enrcyc(iznmax)                                                 
  150    format(bz,6x,a8,f4.0,1x,a8,f4.0,a1,i1, f6.5, 3f6.1 )              
c                                                                       
c       SET CODE FOR ZNO CAP. REP.                                      
c                                                                       
         if ( cznpro(iznmax) .ne. 0.0 ) iznosw(iznmax) = 1              
c                                                                       
c       SET IZNBYP = 0 TO INDICATE THE ZNO IS NOT CONDUCTING            
c                                                                       
         iznbyp(iznmax) = 0                                             
c                                                                       
c       INTIALIZE PAST VALUES OF IZNBYP(  ) AND CTR USED IN SUBRT ZNOSOL
c                                                                       
         izop(iznmax) = 0.0                                             
         ctro(iznmax) = 0.0                                             
c                                                                       
c       CONVERT CURRENT TO PER UNIT                                     
c                                                                       
         cbase = (bmva* 1000. ) / (sqrt3*bkv1)                          
         cznpro(iznmax) = cznpro(iznmax)/cbase                          
c                                                                       
c       CONVERT MEGAJOULES TO PER UNIT MEGAWATT CYCLES                  
c       MEGAJOULES IS A SINGLE PHASE QUANTITY                           
c                                                                       
          znjou(iznmax) = 3.0*0.01*frqbse*znjou(iznmax)                 
          zngc(iznmax) = 0.0                                            
          znxco(iznmax) = -znxco(iznmax)                                
          if (znxco(iznmax) .ne. 0.0) then                                
             znbc(iznmax) = - 1.0/znxco(iznmax)                         
          else                                                          
             znbc(iznmax) = 0.0                                         
          endif                                                         
       endif                                                            
       if (sstyp .eq. 'B') then                                           
         read(buf80(i),300)name1,bkv1,name2,bkv2,                       
     1                 vyar1(iznmax),vyar2(iznmax),      
     2   vyar3(iznmax),vyar4(iznmax),vyar5(iznmax),vyar6(iznmax)        
  300     format(bz,6x,a8,f4.0,1x,a8,f4.0,2x,14x,6f5.4)         
         kb1 = nambas(bkv1)                                             
         kb2 = nambas(bkv2)                                             
         j1 = inam(name1,kb1)                                           
         j2 = inam(name2,kb2)                                           
         if (j1.eq. 0 .or. j2 .eq.0) then                                 
            write(errbuf(1), 200) buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
          endif                                                         
c                                                                       
c       CONVERT TIME CONSTANTS TO CYCLES AND FORM TIME FACTORS          
c                                                                       
          vyar1(iznmax) = 1. + 2.*(vyar1(iznmax)*frqbse)/dt             
          vyar2(iznmax) = 1. + 2.*(vyar2(iznmax)*frqbse)/dt             
          vyar3(iznmax) = 1. + 2.*(vyar3(iznmax)*frqbse)/dt             
          vyar4(iznmax) = 1. + 2.*(vyar4(iznmax)*frqbse)/dt             
          vyar5(iznmax) = 1. + 2.*(vyar5(iznmax)*frqbse)/dt             
          vyar6(iznmax) = 1. + 2.*(vyar6(iznmax)*frqbse)/dt             
          if (cznpro(iznmax) .ne. 0.0) then
             iznosw(iznmax) = 2                                            
          else
             iznosw(iznmax) = 3                                            
          endif
       endif                                                            
c                                                                       
c       GET ADMITTANCES FROM BRANCH DATA TABLE FOR EITHER ZNO CAPACITOR 
c       SSTYP = A OR RANI SSTYP = B                                     
c                                                                       
       if (sstyp .eq. 'A' .or. sstyp .eq. 'B') then                       
          call getyeq (j1,j2,iparo,iseco,yeq,y1,yxy,y2,y3,ierr)         
          if (ierr .eq. -1) then                                          
             write(errbuf(1),320)name1,bkv1,name2,bkv2,iparo,isec       
  320        format(5x,2(a8,1x,f5.1,1x),1x,a1,1x,i1)                    
             write(errbuf(2),340)                                       
  340        format(5x,' THIS LINE FROM AN RZ DATA CARD COULD ',        
     2              'NOT BE FOUND IN THE BRANCH DATA TABLE.')           
             call prterr('E',2)                                         
             iabort = 1                                                 
             go to 1000                                                 
         endif                                                          
         if (ierr .eq. -3) then                                           
             write(errbuf(1),320)name1,bkv1,name2,bkv2,iparo,isec       
             write(errbuf(2),360)                                       
  360        format(5x,' THIS LINE FROM AN RZ DATA CARD IS ',           
     1             'EITHER A DC LINE OR A REGULATING TRANSFORMER AND ', 
     2             'CANNOT BE USED.')                                   
             call prterr('E',2)                                         
             iabort = 1                                                 
             go to 1000                                                 
         endif                                                          
c        -  for next 20 lines, change misspelling of array index
c           (IIZNKT should be IZNMAX)
c                                                                       
c       STORE TOTAL LINE EQUIVALENT ADMITTANCE                          
c                                                                       
         zngij(iznmax) =  -real(yeq(1,2))                               !dem
         znbij(iznmax) =  -aimag(yeq(1,2))                              !dem
         zngii(iznmax) =  real(yeq(1,1) + yeq(1,2))                     !dem
         znbii(iznmax) =  aimag(yeq(1,1) +yeq(1,2))                     !dem
         zngjj(iznmax) =  real(yeq(2,2) + yeq(2,1))                     !dem
         znbjj(iznmax) =  aimag(yeq(2,2) + yeq(2,1))                    !dem
c                                                                       
c       STORE EQUIVALENT ADMITTANCE OF ALL SECTIONS TO THE RIGHT        
c                                                                       
         zng23(iznmax) = -real(y2(1,2))                                 !dem
         znb23(iznmax) = -aimag(y2(1,2))                                !dem
         zng33l(iznmax) = real(y2(1,1) + y2(1,2))                       !dem
         znb33l(iznmax) = aimag(y2(1,1) +y2(1,2))                       !dem
         zng33r(iznmax) = real(y2(2,2) + y2(2,1))                       !dem
         znb33r(iznmax) = aimag(y2(2,2) + y2(2,1))                      !dem
c                                                                       
c       STORE EQUIVALENT ADMITTANCE OF SECTIONS TO THE LEFT             
c                                                                       
         zng14(iznmax) = -real(y1(1,2))                                 !dem
         znb14(iznmax) = -aimag(y1(1,2))                                !dem
         zng44l(iznmax) = real(y1(1,1) + y1(1,2))                       !dem
         znb44l(iznmax) = aimag(y1(1,1) +y1(1,2))                       !dem
         zng44r(iznmax) = real(y1(2,2) + y1(2,1))                       !dem
         znb44r(iznmax) = aimag(y1(2,2) + y1(2,1))                      !dem
         go to 1000                                                     
      endif                                                             
       if (sstyp .eq. 'C') then                                           
         read(buf80(i),400)name1,bkv1,name2,bkv2,vyk1(iznmax),          
     1    vypmax(iznmax),vypmin(iznmax),vyimax(iznmax),                 
     2    vyimin(iznmax),vygo(iznmax),vybo(iznmax),
     3    vyxint(iznmax),ipori                                          
  400     format(bz,6x,a8,f4.0,1x,a8,f4.0,2x,f5.0,4f5.1,2f5.2,5x,f5.5,
     &       1x,a1)  
         kb1 = nambas(bkv1)                                             
         kb2 = nambas(bkv2)                                             
         j1 = inam(name1,kb1)                                           
         j2 = inam(name2,kb2)                                           
         if (j1.eq. 0 .or. j2 .eq.0) then                                 
            write(errbuf(1), 200) buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
          endif                                                         
c                                                                       
c       IF IVYPI = 1 CONSTANT POWER =2 CONSTANT CURRENT                 
c                                                                       
         if (ipori .eq. 'P') ivypi(iznmax) = 1                           
         if (ipori .eq. 'I') ivypi(iznmax) = 2                           
c                                                                       
c         CONVERT GAIN TO RADIANS/PUR CURRENT                           
c                                                                       
         cbase = (bmva*1000.) / (sqrt3*bkv1)                            
         vyk1(iznmax) = vyk1(iznmax)*degrad*cbase                       
c                                                                       
c       CONVERT IMPEDANCE TO PER UNIT                                   
c                                                                       
          zbase = (bkv1*bkv1)/bmva                                      
          vygo(iznmax) = vygo(iznmax)/zbase                             
          vybo(iznmax) = vybo(iznmax)/zbase                             
          y_c = -yxy(1,2) 
          y_o = cmplx (1.0, 0.0) / cmplx (vygo(iznmax), vybo(iznmax))
          y_v = y_c - y_o
          z_v = cmplx (1.0, 0.0) / y_v
          vygv(iznmax) = real (z_v)
          vybv(iznmax) = aimag (z_v)
c
c         If vyxinit() is not specified, compute its value from yvbv() 
c         and sigma = 90.0 degrees.
c
          if (vyxint(iznmax) .eq. 0.0) then
             vyxint(iznmax) = (0.5 * pi - 1.0) / (-pi * aimag (y_v)) 
          else
             vyxint(iznmax)=vyxint(iznmax)/zbase                            
          endif
c                                                                       
c       CALCULATE ALPHA FROM BV AND XINT USING NEWTON'S METHOD          
c                                                                       
          bv = -aimag(y_v)
          xk = bv * vyxint(iznmax) * pi                                     
          if (xk .lt. 0.0 .or. xk .gt. pi) then
             write (errbuf(1), 201) buf80(i)                                
  201        format (a)
             write (errbuf(2), 202) xint, bv
  202        format (' Infeasible initial values for xint, bv ', 2e10.3)
             iznmax = iznmax - 1                                         
             call prterr ('E', 2)                                          
             go to 1000                                                  
          else
             sigo = 0.5 * pi
             dif = 1.0e10
             do while (dif .gt. 1.0e-6)
                fp = 1.0 - cos(sigo)                                           
                f = xk + sin(sigo) - sigo                                     
                sig = (f/fp) + sigo                                           
                dif = abs( sig - sigo )                                       
                sigo = sig                                                  
             enddo
          endif
          vyalfi(iznmax) = pi - sig/2.                                  
c                                                                       
c       CONVERT IMPEDANCES TO ADMITTANCES                               
c                                                                       
          recpi=1./(vygv(iznmax)*vygv(iznmax)                           
     1         +vybv(iznmax)*vybv(iznmax))                                                  
          vygv(iznmax) = vygv(iznmax)*recpi
          vybv(iznmax) = - vybv(iznmax)*recpi                           !PDOLAN 
          vygtot(iznmax)  =  vygv(iznmax)                               
          vybtot(iznmax)  =  vybv(iznmax) 

          if (vygo(iznmax).ne. 0.0 .or. vybo(iznmax).ne. 0.0) then        
          recpi=1./(vygo(iznmax)*vygo(iznmax)                           
     1         +vybo(iznmax)*vybo(iznmax))
C
C     added for GE control
C
          xfcyw(iznmax)=vybo(iznmax)
C                              
          vygo(iznmax) = vygo(iznmax)*recpi                             
          vybo(iznmax) = - vybo(iznmax)*recpi                             
          vygtot(iznmax)  = vygtot(iznmax) + vygo(iznmax)               
          vybtot(iznmax) =  vybtot(iznmax) + vybo(iznmax)
c         only for debug 
          reyw=vygtot(iznmax)**2+vybtot(iznmax)**2
          xtcscyw(iznmax)=-vybtot(iznmax)/reyw               
          xtcscge(iznmax)=xtcscyw(iznmax)
          rtcscyw(iznmax)=vygtot(iznmax)/reyw
c
          endif                                                         
          go to 1000                                                    
      endif                                                             
       if (sstyp .eq. 'D') then                                           
         read(buf80(i),500)name1,bkv1,name2,bkv2,vypint(iznmax),        
     1                   vyamax(iznmax),vyamin(iznmax),vybmax(iznmax),  
     2                   vybmin(iznmax),vyfreq(iznmax),
     3                   iranityp(iznmax),xtcscma(iznmax),
     4                   xtcscmi(iznmax),iparl(iznmax)                  
  500    format(bz,6x,a8,f4.0,1x,a8,f4.0,2x,f5.1,2f4.1,2f5.1,f5.4,
     1          1x,i1,2f5.1,i1)         
         kb1 = nambas(bkv1)                                             
         kb2 = nambas(bkv2)                                             
         j1 = inam(name1,kb1)                                           
         j2 = inam(name2,kb2)                                           
         if (j1.eq. 0 .or. j2 .eq.0) then                                 
            write(errbuf(1),200)buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
          endif                                                         
c                                                                       
c       CONVERT ALPHA MAX AND ALPHA MIN TO RADIANS                      
c                                                                       
          vyamax(iznmax) = vyamax(iznmax)*degrad                        
          vyamin(iznmax) = vyamin(iznmax)*degrad                        
c                                                                       
c       CONVER BMAX AND BMIN TO PER UNIT IMPEDANCE/CYCLE                
c                                                                       
          zbase = (bkv1*bkv1)/bmva                                      
          vybmax(iznmax) =  vybmax(iznmax)/(frqbse*zbase)               
          vybmin(iznmax) = -vybmin(iznmax)/(frqbse*zbase)
c
c       added by Yu Wang for TCSC control test
c
          xtcscma(iznmax)=xtcscma(iznmax)/zbase
          xtcscmi(iznmax)=xtcscmi(iznmax)/zbase
c               
c                                                                       
c       CONVERT OMEGA TO RADIANS/CYCLE                                  
c                                                                       
          vyfreq(iznmax) = vyfreq(iznmax)*(2.*pi/frqbse)                
          go to 1000                                                    
      endif                                                             
      write(errbuf(1),900)buf80(i)                                      
c                                                                       
c       DECODE MODULATION CARDS IF PRESENT                              
c                                                                       
       if (sstyp .eq. 'F') then                                           
         read(buf80(i),600)name1,bkv1,name2,bkv2,vymare(iznmax),        
     1                   vymarf(iznmax),vymarg(iznmax),vymarh(iznmax),  
     2                   vymca(iznmax),vymcb(iznmax),vymcc(iznmax),     
     3                   vymcd(iznmax),vymce(iznmax),vymcf(iznmax),    
     4                   vymarf0(iznmax),vymca0(iznmax)             
  600    format(bz,6x,a8,f4.0,1x,a8,f4.0,2x,4f5.4,5f3.0,3f4.1)             
         kb1 = nambas(bkv1)                                             
         kb2 = nambas(bkv2)                                             
         j1 = inam(name1,kb1)                                           
         j2 = inam(name2,kb2)
         vymimx(iznmax)=800.
         vymimn(iznmax)=-800.                                           
         if (j1.eq. 0 .or. j2 .eq.0) then                                 
            write(errbuf(1),200) buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
          endif                                                         
c                                                                       
c         CONVERT CURRENT LIMITS TO PER UNIT                              
c                                                                       
         cbase = (bmva*1000.)/ (sqrt3*bkv1)                             
         vymimx(iznmax) = vymimx(iznmax)/cbase                          
         vymimn(iznmax) = vymimn(iznmax)/cbase                          

c     Mittelstadt requested to add capability of user-input data for 
c     the corner points of the control function: Time(cycles) and 
c     Xorder (ohms) on the RZ F card.  After checking with Dr. Yu Wang who
c     originally provided the hardcoded logic for this in VYSOL.FOR, the 
c     offset is taken into consideration for the Xorders, i.e. using the 
c     1st Xorder as reference.  Tsu-huei Liu, Nov. 22, 1993

         if (iranityp(iznmax) .eq. 3) then
           zbase=bkv1*bkv1/bmva
   	   if (vymcb(iznmax) .ne. 0.0) then
              write (errbuf(1), 601) buf80(i)(1:33), name1, bkv1, name2,
     &                               bkv2, vymcb(iznmax)
  601         format (' Record ', a, ' has non-zero "B" (', e10.3, ').')
              write (errbuf(2), 602) 
  602         format (' Coefficents "D", "F", and "A0" are biased ')
           endif
           vyb = vymcb(iznmax)
           vymcb(iznmax) = 0.0   ! Per Unit values for input signal
           vymcd(iznmax) = (vymcd(iznmax) - vyb) / zbase	! corners
  	   vymcf(iznmax) = (vymcf(iznmax) - vyb) / zbase	! are
	   vymca0(iznmax) = (vymca0(iznmax) - vyb) / zbase ! calculated
           go to 1000
         endif

c     end of added code.  These data will be used in VYSOL.FOR, Tsu-huei.
c                                                                       
c        CONVERT CONSTANTS FROM SECONDS TO CYCLES                        
c                    
c           VYMCA(IZNMAX) = VYMCA(IZNMAX)/FRQBSE                           
c           VYMCC(IZNMAX) = VYMCC(IZNMAX)/FRQBSE                           
c           VYMCB(IZNMAX) = VYMCB(IZNMAX)/(FRQBSE*FRQBSE)                  
c           VYMCD(IZNMAX) = VYMCD(IZNMAX)/(FRQBSE*FRQBSE)                  
c
       togflag=0                                                         !PDOLAN
       if (vymca0(iznmax).eq.0) then                                      !PDOLAN
         togflag=1                                                       !PDOLAN
         roota1 = vymcb(iznmax)                                          !PDOLAN
       endif                                                             !PDOLAN
       if (vymca0(iznmax).ne.0) then                                      !PDOLAN
         togflag=0                                                        !PDOLAN
         arga = (vymca(iznmax)**2)-4*vymca0(iznmax)*vymcb(iznmax)         !PDOLAN
         roota1=(vymca(iznmax)+sqrt(arga))/(2*vymca0(iznmax))             !PDOLAN
         roota2=(vymca(iznmax)-sqrt(arga))/(2*vymca0(iznmax))             !PDOLAN
       endif                                                             !PDOLAN
       argb = (vymcc(iznmax)**2)-4*vymcd(iznmax)                        !PDOLAN
       rootb1=(vymcc(iznmax)+sqrt(argb))/2                              !PDOLAN
       rootb2=(vymcc(iznmax)-sqrt(argb))/2                              !PDOLAN
  
C * * *                                                                 
C * * * FORM TIME FACTORS                                               
C * * *                                                                 
         vymar(iznmax) = 2.0 * frqbse / dt                                      !PDOLAN     
         vymare(iznmax) = 1. + 2.*vymare(iznmax)*frqbse/dt              
         vymarf(iznmax) = 1. + 2.*vymarf(iznmax)*frqbse/dt      
c        added by Yu Wang
         vymarf0(iznmax)= vymcf(iznmax)+2.*vymarf0(iznmax)*frqbse/dt
c        
         vymarg(iznmax) = vymce(iznmax)+ 2.*vymarg(iznmax)*frqbse/dt    
         vymarh(iznmax) = 1. + 2.*vymarh(iznmax)*frqbse/dt              
C
           vymab(iznmax)  = (vymca0(iznmax)
     1              + vymca(iznmax)/vymar(iznmax)             
     2              + vymcb(iznmax)/(vymar(iznmax)*vymar(iznmax)))      
           vymdc(iznmax)  = (1. + vymcc(iznmax)/vymar(iznmax)             
     1              + vymcd(iznmax)/(vymar(iznmax)*vymar(iznmax)))
C
         go to 1000                                                     
       endif                                                             
       if (sstyp .eq. 'G') then                                           
         read(buf80(i),700)name1,bkv1,name2,bkv2,ipori,vymck(iznmax),   
     1       zsyn1ge(iznmax),zsyn2ge(iznmax),ibtyp(iznmax),name3,
     2       bkv3,name4,bkv4
  700    format(bz,6x,a8,f4.0,1x,a8,f4.0,2x,a1,f6.2,2f5.0,i1,5x,
     &          2(a8,f4.0))        

         open(89,file='METO.DAT',status='NEW')   
         meto1=vymck(iznmax)                     
         write(89,701)meto1                      
  701    format(' ',1pe11.4)                     
         close(unit=89)                          
C
         zbase=bkv1*bkv1/bmva
         zsyn1ge(iznmax)=zsyn1ge(iznmax)/zbase
         zsyn2ge(iznmax)=zsyn2ge(iznmax)/zbase
C
         kb1 = nambas(bkv1)                                             
         kb2 = nambas(bkv2)                                             
         j1 = inam(name1,kb1)                                           
         j2 = inam(name2,kb2)                                           
         if (j1.eq. 0 .or. j2 .eq.0) then                                 
            write(errbuf(1),200)buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
          endif                                                         
         kb3 = nambas(bkv3)                                             
         kb4 = nambas(bkv4)                                             
         ivymb1(iznmax) = inam(name3,kb3)                               
         ivymb2(iznmax) = inam(name4,kb4)                               
         if (ivymb1(iznmax) .eq. 0 .or. ivymb2(iznmax).eq.0) then         
            write(errbuf(1),200)buf80(i)                                
            iznmax = iznmax - 1                                         
            call prterr('E',1)                                          
            go to 1000                                                  
         endif                                                          
         if (ipori .eq. 'P') then                                         
            ivymsw(iznmax) = 1                                          
         else                                                           
            ivymsw(iznmax) = 2                                          
         endif                                                          
         go to 1000                                                     
      endif                                                             
 900  format('0',1x,a80,'THIS CARD HAS AN IMPROPER SUBTYPE ')           
      call prterr('E',1)                                                
 1000 continue                                                          
      return                                                            
      end                                                               
