C    %W% %G%
      subroutine vtran                                                  
                                                                        
c     This program intercepts VERSATEC VERSAPLOT calls and translates     
C     them to PostScript.
c
c     Note:  the file is created on file name PLOT$FILE.  This logical
C     must be defined before the application runs.
c
c     $ DEFINE PLOT$FILE  filename.ext                          
c                                                                       
c     This routine originally was developed at Arzonia Public Service   
c     Company and has been more fully developed by BPA since.           
c                                                                       
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/bpapds.inc' 
      include 'tspinc/files.inc' 
      include 'tspinc/comn56.inc' 
                                                                        
      common /cvtran/ lun, flname, x_now, y_now , rad                     
C                                                                         
      character flname*(40)                                               

c     integer itext( 20 ), label( 18 ), lmask, lun                      

      integer lmask, lun                      
      integer found       ! defined, but not currently used 
      character buf*90
      character itext*(*), label*80 
      integer open_file
C                                                                         
      character *(*) pmode, msgtxt                                      
                                                                        
      dimension xd( 1000 ), yd( 1000 ), xarray( 2500 ), yarray( 2500 ), 
     1          a( 1000 )                                               
                                                                        
      logical normal, bias, found_post 
                                                                        
C     save X_NOW, Y_NOW , LUN, RAD                                        
                                                                        
      character versn*10                                                
c                    ....+....1                                         

      integer lunin                                                       
      character flnmin*(*)                                                
      logical debug                                                       
      integer ibuf, nloc, ldev
      real x,y
      integer ipen

      data lun /0/, flname /'PLOT$FILE'/                                  
      data versn / 'VT 4.0.0  ' /                                      
      data debug / .true. /                                               
C                                                                         
c********************************************************************** 
c          END OF DECLARATIONS                                        * 
c********************************************************************** 
                                                                        
                                                                        
c**********************************************************************   
c   --- VTRINIT CALL ---                                                  
c**********************************************************************   
                                                                          
      save

      entry vtrinit (lunin, flnmin)                                       
C                                                                         
C     This subrtn allows a calling application to force which logical  
C     unit and filename will be used to write out the VTRAN text.      
C     If the filename is blank, then the FILE option will not be used  
C     when the file is opened.  If LUNIN is zero, then FLNMIN will     
C     be the name if the file, but the unit number will be assigned    
C     by the VAX-VMS specific subrtn LIB$GET.  If LUNIN is non-zero,   
c     VTRAN assumes that the application has already opened the        
c     file.  
C                                                                         
      lun = lunin                                                         
      flname = flnmin                                                     
      mstpost = 0          ! flag - tspmaster not yet copied to lun
      return                                                              
C                                                                         
C                                                                         
c**********************************************************************   
c   --- VTROPEN CALL ---                                                  
c**********************************************************************   
C                                                                         
      entry vtropen (lunin, flnmin)                                       
C                                                                         
C     -  This subrtn opens the VTRAN output file explicitly by an         
C        application rather than implicitly via PLOTS & PDFMSG.  If       
C        FLNAME is blank, the FILE option is not used.                    
C                                                                         
      lun = lunin                                                         
      flname = flnmin                                                     
      if (debug) then                                                     
        call dbgeko ('VTROPEN - incoming file name')                      
        call dbgwrc ('  FLNMIN = ',flnmin)                                
        call dbgwrc ('  FLNAME = ',flname)                                
      endif                                                               
      if (flname .lt. '0') then                                           
        open (unit=lun,status='NEW',access = 'SEQUENTIAL',                
c    1    carriagecontrol = 'LIST',                    
     2    form = 'FORMATTED',
     3    file = flname)                                                  
      else                                                                
        open (unit=lun,status='NEW',access = 'SEQUENTIAL',                
     1    form = 'FORMATTED',
c    2    carriagecontrol = 'LIST',                    
     3    file = flname)                                                  
      endif                                                               
      write(lun,'(A,A)')                                                  
     1     '** VTROPEN - Translated by VTRAN version: ',versn             
c     WRITE(LUN,'(A,A)')                                                  
c    1     '** PLOT$FILE Translated by VTRAN version: ',VERSN             
      return                                                              
C                                                                         
C                                                                         
c********************************************************************** 
c   --- PLOTS CALL ---                                                  
c********************************************************************** 
                                                                        
      entry plots( ibuf, nloc, ldev )                 
                                                                        
      kout = 6                                                          
      rad = 3.14159/180.0                                               
      if ( lun .le. 0 ) then                                            
                                                                        
c       IF LUN IS NOT CURRENTLY OPEN, we will                           
c       get an available logical unit number from the system pool       
c       of available numbers and open a plot file on it.  The numbers   
c       are assigned in decending order from 119 to 100                 
                                                                        
         open(            unit = lun,                                   
     1                    file = flname,                                  
     2                  status = 'UNKNOWN',                               
c    2                  STATUS = 'NEW',                                 
     3                  access = 'SEQUENTIAL',                          
     4                    form = 'FORMATTED',                           
c    5         carriagecontrol = 'LIST' ,                               
     6                  iostat = ios )                                  


         write(lun,100) '%% '                                           
         write(lun,'(A)')                                                  
     1     '%% OPENING lun IN PLOTS '               
         write(lun,100) '%% '                                           
                                                                        
                                                                        
         if (ios .ne. 0 ) stop 'FAILED TO OPEN VTRAN PLOT FILE'           
      endif

** is tspmaster.post open?
      inquire (l23, opened = found_post)
      if (.not. found_post) then 
         close (unit = l23) 
         if (is_it_vms() .eq. 0) then
**it must be UNIX **   
           found = open_file (l23, 
     1     postmstr,
     2     'F', 'R', iostat)
         else
** it's VMS ** 
           found = open_file (l23, 
     1     postmstr,
     2     'F', 'R', iostat)
         endif
      endif                                                             

********* copy tspmaster.post to LUN ***************
      inquire (l23, opened = found_post)
      if (found_post .and. mstpost .eq. 0) then
        mstpost = 1
  110   read (l23,'(a)',end=120) buf
        write (lun, '(a)') buf
        go to 110   
  120   close (unit = l23)
        if (plprpg .eq. ' ' .or. plprpg .eq. '1') then
          nplprpg = 1
        else
          nplprpg = 4
        endif
        write (lun, 131) nplprpg 
  131   format(i3, ' PlPrPg')  
      endif
********* END copy tspmaster.post to LUN ***************     

         write(lun,100) '%% '                                           
  100    format (a,a,a)                                                 
         write(lun,'(A,A)')                                                  
     1     '%% PLOTS - Translated by VTRAN version: ',versn               
         write(lun,100) '%% '                                           
*     write( lun, 101 ) 'PLOTS ', ibuf, nloc, ldev                      
* 101 format( a6, i6, i6, i6 )                                          
c 101 format( a6, i6, 1x, i6, 1x, i6 )                                          
      return                                                            
                                                                        
c********************************************************************** 
c   --- PDFMSG CALL ---                                                 
c********************************************************************** 
                                                                        
      entry pdfmsg( msgtxt )                                            
      if ( lun .le. 0 ) then                                            
                                                                        
c       IF LUN IS NOT CURRENTLY OPEN, we will                           
c       get an available logical unit number from the system pool       
c       of available numbers and open a plot file on it.  The numbers   
c       are assigned in decending order from 119 to 100                 
                                                                        
Cif vax                                                                   
c        call lib$get_lun(lun)                                          
Cendif vax                                                                
         open(            unit = lun,                                   
     1                    file = flname,                                  
     2                  status = 'NEW',                                 
     3                  access = 'SEQUENTIAL',                          
     4                    form = 'FORMATTED',                           
c    5         carriagecontrol = 'LIST' ,                               
     6                  iostat = ios )                                  
                                                                        
         if (ios .ne. 0 ) stop 'FAILED TO OPEN VTRAN PLOT FILE'           
                                                                        
         write(lun,100) '%% '                                           
         write(lun,'(A,A)')                                                  
     1     '%% PDFMSG - Translated by VTRAN version: ',versn              
         write(lun,100) '%% '                                           
                                                                        
      endif                                                             
                                                                        
      write( lun, 100 ) '** ',msgtxt                                    
                                                                        
      return                                                            
                                                                        
c********************************************************************** 
c   --- PLOT CALL ---                                                   
c********************************************************************** 
                                                                        
      entry plot( x, y, ipen )                                          
**************** postscript stuff ************************                                                                        
      write( lun, 201 )  x, y, ipen, ' pPlot '                            
 201  format(  2(f12.6, 1x,' inch '), i6, a6 )
*     write( lun, 201 ) 'PLOT  ', x, y, ipen                            
*201  format( a6, 2g12.6, i6 )
c201   format( a6, g12.6, 1x, g12.6, 1x, i6 )
**************** postscript stuff ************************                                                                        

      if ( ipen .eq. 999 ) then                                         
         close( unit = lun )                                            
Cif vax                                                                   
c        call lib$free_lun(lun)                                         
Cendif vax                                                                
         lun = 0                                                        
                                                                        
      else if ( ipen .eq. 2 .or. ipen .eq. 3 ) then                     
         x_now = x                                                      
         y_now = y                                                      
                                                                        
      else if ( ipen .eq. -2 .or. ipen .eq. -3 ) then                   
         x_now = 0.0                                                    
         y_now = 0.0                                                    
      end if                                                            
                                                                        
      return                                                            
                                                                        
c********************************************************************** 
c   --- NEWPEN CALL ---                                                 
c********************************************************************** 
                                                                        
      entry newpen( inp )                                               
                                                                        
      write( lun, 301 ) 'NEWPEN', inp                                   
301   format( '%** PROGRAM ERROR **', a6, i6 )                                                  
      return                                                            
                                                                        
                                                                        
c********************************************************************** 
c   --- PLTMOD CALL ---                                                 
c********************************************************************** 
                                                                        
      entry pltmod( pmode )                                             
                                                                        
      if ( lun .le. 0 ) then                                            
                                                                        
c       IF LUN IS NOT CURRENTLY OPEN, we will                           
c       get an available logical unit number from the system pool       
c       of available numbers and open a plot file on it.  The numbers   
c       are assigned in decending order from 119 to 100                 
                                                                        
Cif vax                                                                   
c        call lib$get_lun(lun)                                          
Cendif vax                                                                
         open(            unit = lun,                                   
     1                    file = flname,                                  
     2                  status = 'NEW',                                 
     3                  access = 'SEQUENTIAL',                          
     4                    form = 'FORMATTED',                           
c    5         carriagecontrol = 'LIST' ,                               
     6                  iostat = ios )                                  
                                                                        
         if (ios .ne. 0 ) stop 'FAILED TO OPEN VTRAN PLOT FILE'           
                                                                        
         write(lun,100) '%% '                                           
        write(lun,'(A,A)')                                                  
     1     '%% PLTMOD - Translated by VTRAN version: ',versn              
         write(lun,100) '%% '                                           
                                                                        
      endif                                                             
                                                                        
      write( lun, 302 ) 'PLTMOD', pmode                                 
302   format('%** PROGRAM ERROR **', a6, a )                                                   
      return                                                            
                                                                        
                                                                        
c********************************************************************** 
c   --- PBIAS CALL ---                                                  
c********************************************************************** 
                                                                        
      entry pbias( bx,by )                                              
                                                                        
      write( lun, 303 ) 'PBIAS ', bx, by                                
303   format('%** PROGRAM ERROR **', a6, 2g12.6 )                                              
      return                                                            
                                                                        
c********************************************************************** 
c   --- SCALE CALL ---                                                  
c********************************************************************** 
                                                                        
      entry scale( a, axlen, npoits, incr )                             
                                                                        
c---->Unlike the other entries in VTRAN, SCALE calculates FVALL and     
c     DVV and returns them in a( npts-1) and a( npts-2 ) respectively.  
c     Entry SCALE does not write any output to 'PLOT$FILE'.             
                                                                        
c---->entry scale computes scale factors for processing                 
c     unscaled data with subroutines line and axis                      
                                                                        
c---->     a: array of unscaled data to be examined, defined in         
c             calling program                                           
                                                                        
c----> axlen: axis length to which unscaled data                        
c             in array a is to be scaled                                
                                                                        
c---->npoits: number of data points to be scaled in array a.            
c             does not include points skipped by incr.                  
                                                                        
c---->  incr: absolute increment used in gathering data                 
c             from unscaled data in array a.                            
                                                                        
c---->determine maximum and minimum values in array a.                  
                                                                        
      biga = a( 1 )                                                     
      smalla = a( 1 )                                                   
                                                                        
      do 10 i = 1, npoits, incr                                         
         if ( a( i ) .gt. biga ) biga = a( i )                          
         if ( a( i ) .lt. smalla ) smalla = a( i )                      
   10 continue                                                          
                                                                        
C---->DETERMINE DVV, DELTA VALUE                                        
C---->BEGIN CASE IN WHICH ARRAY A HOLDS ALL VALUES OF LIKE SIGN.        
                                                                        
      if ( smalla .ge. 0. ) then                                        
           dellta = biga/axlen                                          
      else if ( biga. le. 0. ) then                                     
           dellta = -1.*smalla/axlen                                    
                                                                        
C---->BEGIN CASE IN WHICH ARRAY HOLDS POSITIVE AND NEGATIVE VALUES.     
                                                                        
      else                                                              
           dellta = ( biga - smalla )/axlen                             
      end if                                                            
                                                                        
      tenpow = 1.                                                       
   20 dvv = tenpow*dellta                                               
                                                                        
C---->ADJUST DVV SO IT FALLS BETWEEN 1. AND 10.                         
                                                                        
      if ( dvv .lt. 1. ) then                                           
           tenpow = 10.*tenpow                                          
           go to 20                                                     
      else if ( dvv .gt. 10. ) then                                     
           tenpow = tenpow/10.                                          
           go to 20                                                     
      else                                                              
           dvi = anint( dvv )                                           
      end if                                                            
                                                                        
      if ( dvi .lt. dvv ) then                                          
           dvv = dvi + 1.                                               
      else                                                              
           dvv = dvi                                                    
      end if                                                            
                                                                        
      dvv = dvv/tenpow                                                  
                                                                        
C---->DVV IS STORED AT END OF ARRAY A.                                  
                                                                        
      a( npoits + 2 ) = dvv                                             
                                                                        
C---->DETERMINE FVALL, THE FIRST VALUE                                  
                                                                        
      del = abs( smalla/dvv )                                           
                                                                        
      if ( smalla .lt. 0. ) then                                        
           fvall = -1.*anint(del)*dvv                                   
           if ( smalla .lt. fvall ) fvall = fvall - dvv                 
      else                                                              
           fvall = aint( del )*dvv                                      
      end if                                                            
                                                                        
C     IF ( 10.*DVV .LT. AXLEN ) FVALL = FVALL - DVV                     
                                                                        
C---->FVALL IS STORED AT NEXT-TO-END OF ARRAY A.                        
                                                                        
      a( npoits + 1 ) = fvall                                           
                                                                        
      return ! FROM SCALE                                               
                                                                        
c********************************************************************** 
c   --- SYMBOL CALL ---                                                 
c********************************************************************** 
                                                                        
      entry symbol( x, y, height, itext, angle, nc )                    
                                                                        
      nnc = nc                                                         
                                                                        
      if ( nnc .gt. 80 ) then                                           
         write( kout, '( A )' )                                         
     +         ('SYMBOL TEXT LONGER THAN 80 CHARACTERS - TRUNCATED.')   
         nnc = 80                                                       
      end if 

      if (nnc.ge. 0) then 
         if (nnc .eq. 0) nnc = 1
         write( lun, 401 ) x, y, height, itext(1:nnc), angle, nnc,
     1  'Symbol'              
  401   format(2(f12.6, ' inch '), f12.6/ '(', a, ')'/
     1  f12.6, 1x, i6, 1x, a6 )
c  401  format( a7, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i6 )
                                                                        
C---->FOR CASE WHEN NC = -1 AND APPLICATION WANTS ONLY TO PLOT A SINGLE 
C     'VERSATEC' CHARACTER                                              
                                                                        
*       if ( nc .eq. -1 ) then                                            
      else 
***** SUBROUTINE SYMBOL SAYS
***** draw a line from current location to x,y if nc < -2
***** This version doen no draw a line 
c       write( lun, 402 ) itext( 1 )                                   
c  402  format( a )                                                    
        write( lun, 402 ) x, y, height, itext(1:1), 'LinSym'      
  402   format(3(f12.6, ' inch '), a1, 1x, a6)
*     else                                                              
*                                                                       
*        if ( nnc .gt. 0 ) then                                         
*           iwords = ( nnc-1 )/4+1                                      
*        else                                                           
*           iwords = 1                                                  
*        end if                                                         
*                                                                       
c         write( lun, 403 ) ( itext( i ), i = 1, iwords )                
c  403    format( 20a4 )                                                 
      end if                                                            
                                                                        
      x_now = x + cos( angle*rad )*height*abs( nc )                     
      y_now = y + sin( angle*rad )*height*abs( nc )                     
      return                                                            
                                                                        
c********************************************************************** 
c   --- NUMBER CALL ---                                                 
c********************************************************************** 
                                                                        
      entry number( x, y, height, fpn, angle, ndec )                    
                                                                        
      write( lun, 501 ) 'NUMBER', x, y, height, fpn, angle, ndec        
  501 format( '%** PROGRAM ERROR **',a6, 5(f12.6,1x), i6 )                                          
      x_now = x + cos( angle*rad )*height*ndec                          
      y_now = y + sin( angle*rad )*height*ndec                          
      return                                                            
                                                                        
c********************************************************************** 
c   --- AXIS CALL ---                                                   
c********************************************************************** 
                                                                        
      entry axis( x, y, label, nchar, axlen, angle, fval, dv )          
      iabchr = iabs( nchar )
                                                                        
      if (iabchr .gt. 72 ) then                                 
         write( kout, '( A )')                                          
     +      ( 'AXIS LABEL LONGER THAN 72 CHARACTERS - TRUNCATED.' )     
         nchar = isign( 72, nchar )                                     
         iabchr = iabs( nchar )
      end if                                                            
                                                                        
      if (fval .le. 100000.0 .and. fval .gt. -10000.0) then
         write( lun, 601 ) x, y, label(1:iabchr), nchar, axlen, angle, 
     &                     fval, dv, 'Axis  '                        
  601    format( 2(f12.6, ' inch '), /, '(', a, ')', i6, /,
     &     f12.6, ' inch ', f12.6, 6x,  2(f12.6, 1x) ,a6)                    
      else
         write( lun, 10601 ) x, y, label(1:iabchr), nchar, axlen, angle, 
     &     fval, dv, 'Axis  '                        
10601 format( 2(f12.6, ' inch '), /, '(', a, ')', i6, /,
     &     f12.6, ' inch ', f12.6, 6x,  2(f12.5, 1x) ,a6)                    
      endif

c     iwords = ( iabs( nchar ) - 1 )/4+1                                
c     write( lun, 602 ) label                  
c  602 format( a )                                                    
c      write( lun, 602 ) ( label( i ), i = 1, iwords )                   
c  602 format( 18a4 )                                                    
      x_now = x + cos( angle*rad )*axlen                                
      y_now = y + sin( angle*rad )*axlen                                
      return                                                            
                                                                        
c********************************************************************** 
c   --- GRID CALL ---                                                   
c********************************************************************** 
                                                                        
      entry grid( x, y, nx, xxd, ny, yyd, smask )                         
                                                                        
      write( lun, 701 )  x, y, nx, xxd, ny, yyd, smask, 'Grid  ' 
* 701 format( 2(f12.6, ' inch '), 2(i6, 1x, f12.6, ' inch '), i3, a6 )                                    
  701 format(f12.6, ' inch ', f12.6, ' inch ',
     1  i6, 1x, f12.6, ' inch ', i6, 1x, f12.6, ' inch ',   
     2  f12.6, 1x, a6 )  
                                                                        
      if ( iabs( nx ) .gt. 1000 ) then                                  
                                                                        
         if ( iabs( nx ) .gt. 2000 ) then                               
            write( kout, '( A )' )                                      
     +         ( 'GRID VERTICAL LINE ARRAY > 1000 VALUES - TRUNCATED.' )
            nx = isign( 2000, nx )                                      
         end if                                                         
                                                                        
         nele = iabs( nx ) - 1000                                       
      else                                                              
         nele = 1                                                       
      end if                                                            
                                                                        
c XD IS AN ARRAY <<<<<<<<<<<<<<<<<                                      
c     x_now = x + (nx - 1)*xd( nele )                                   
      x_now = x
c     write( lun, 702 ) ( xd( i ), i = 1, nele )                        
c 702 format( 6g12.6 )                                                  
                                                                        
      if ( iabs( ny ) .gt. 1000 ) then                                  
                                                                        
         if ( iabs( ny ) .gt. 2000 ) then                               
            write( kout, '( A )' )                                      
     +       ( 'GRID HORIZONTAL LINE ARRAY > 1000 VALUES - TRUNCATED.' )
            ny = isign( 2000, ny )                                      
         end if                                                         
                                                                        
         nele = iabs( ny ) - 1000                                       
      else                                                              
         nele = 1                                                       
      end if                                                            
                                                                        
c  YD IS AN ARRAY  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<                        
c     y_now = y + (ny - 1)*yd( nele )                                   
      y_now = y
c     write( lun, 702 ) ( yd( i ), i = 1, nele )                        
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- CURVE CALL ---                                                  
c********************************************************************** 
c                                                                       
      entry curve( xarray, yarray, ne, delta )                          
                                                                        
      if ( iabs( ne ) .gt. 2500 ) then                                  
         write( kout, '( A )' )                                         
     +     ( 'CURVE ARRAYS HAVE MORE THAN 2500 VALUES - TRUNCATED.' )   
         ne = isign( 2500, ne )                                         
      end if                                                            
                                                                        
      write( lun, 801 ) 'CURVE ', ne, delta                             
  801 format( '%** PROGRAM ERROR **',a6, i6, f12.6 )                                           
      nele = iabs( ne )                                                 
      write( lun, 802 ) ( xarray( i ), i = 1, nele )                    
      write( lun, 802 ) ( yarray( i ), i = 1, nele )                    
      x_now = xarray( nele )                                            
      y_now = yarray( nele )                                            
  802 format('%** PROGRAM ERROR **', 6(f12.6, 1x))                                                  
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- LINE CALL ---                                                   
c********************************************************************** 
c                                                                       
      entry line( xarray, yarray, npts, inc, lintyp, inteq )            
                                                                        
      nele = inc * ( npts + 2 )                                         

***************** PostScript stuff *********************
      indfval = npts * inc + 1
      inddv = indfval + 1 
      xfval = xarray(indfval)
      xdv = xarray(inddv)   
      yfval = yarray(indfval)
      ydv = yarray(inddv)  
***************** PostScript stuff *********************
 
                                                                        
      if ( nele .gt. 2500 ) then                                        
         write( kout, '( A )' )                                         
     +     ( 'LINE ARRAYS HAVE MORE THAN 2500 VALUES - TRUNCATED.'  )   
         npts = 2500 / inc - 2                                          
         nele = inc * ( npts + 2 )                                      
      end if                                                            
                                                                        
***************** PostScript stuff *********************
      linsym = isign (inteq, lintyp)
      lininc = iabs(lintyp) 
      if (lintyp .eq. 0) then
        linsym = 0
        lininc = 10 
      endif 
****** loop for each curve in arrays ********8
      do 980 kntplt = 1, inc
        write (lun, 911) 
  911   format('[')
        indsav = kntplt
        kntseg = 0 

*********** plot each point on a curve *******
        do 970 kntpnt = 1, npts
          indary = kntplt + (kntpnt - 1) * inc
          kntseg = kntseg + 1
********** may need to calculate scaled values for plotting **** 
          xval = (xarray(indary) - xfval) / xdv
          yval = (yarray(indary) - yfval) / ydv
          write (lun, 921) xval, yval
  921     format(2(f12.6, ' inch ') )
          if (kntseg .eq. lininc .or. kntpnt .eq. npts) then
            kntseg = 0
            xval = (xarray(indsav) - xfval) / xdv
            yval = (yarray(indsav) - yfval) / ydv
            write (lun, 931) xval, yval, linsym
  931       format(']', 2(f12.6, ' inch '), i5, 1x, 'drawLinSmb') 
            indsav = indary
            if (kntpnt .ne. npts) then
              xval = (xarray(indsav) - xfval) / xdv
              yval = (yarray(indsav) - yfval) / ydv
              write(lun, 941) xval, yval
  941         format('[', 2(f12.6, ' inch ')) 
            endif
          endif

  970   continue
  980 continue
***************** End of PostScript stuff *********************
    
*     write( lun, 901 ) 'LINE  ', npts, inc, lintyp, inteq              
* 901 format( a6, 4i6 )                                                 
*     write( lun, 902 ) ( xarray( i ), i = 1, nele )                    
*     write( lun, 902 ) ( yarray( i ), i = 1, nele )                    
* 902 format( 6g12.6 )                                                  
      x_now = xarray( nele )                                            
      y_now = yarray( nele )                                            
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- OFFSET CALL ---                                                 
c********************************************************************** 
c                                                                       
      entry offset( xoff, xfac, yoff, yfac )                            
                                                                        
      write( lun, 1001 ) 'OFFSET', xoff, xfac, yoff, yfac               
 1001 format('%** PROGRAM ERROR **', a6, 4(f12.6, 1x))                                              
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- FACTOR CALL ---                                                 
c********************************************************************** 
c                                                                       
      entry factor( fact )                                              
                                                                        
      write( lun, 1101 ) 'FACTOR', fact                                 
 1101 format( '%** PROGRAM ERROR **',a6, f12.6 )                                               
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- SETMSG CALL ---                                                 
c********************************************************************** 
c                                                                       
      entry setmsg( msglvl )                                            
                                                                        
      write( lun, 1201 ) 'SETMSG', msglvl                               
 1201 format('%** PROGRAM ERROR **', a6, i6 )                                                  
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- PLTORI CALL ---                                                 
c********************************************************************** 
c                                                                       
      entry pltori( normal, bias )                                      
                                                                        
      write( lun, 1301 ) 'PLTORI', normal, bias                         
 1301 format('%**** WE MAY NEED PLTORI ****', a6, 2i2 )                                                 
      return                                                            
                                                                        
c                                                                       
c********************************************************************** 
c   --- WHERE CALL ---                                                  
c********************************************************************** 
c                                                                       
      entry where( x_cur, y_cur, dfact )                                
      x_cur = x_now                                                     
      y_cur = y_now                                                     
      return                                                            
                                                                        
      end                                                               
