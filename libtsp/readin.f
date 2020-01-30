C    %W% %G%
      subroutine readin                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE READS ONE RECORD OF THE INPUT FILE, L5,         
C * * * INTO AN EIGHTY CHARACTER BUFFER ARRAY.  IT IS CALLED BY         
C * * * INPUT1, NOUT1, SWINGM.  IT CALLS ERREND.                        
C * * *                                                                 
c     -  Jul/30/92 -  DEM
C          Use char[80] OLDBUF to store previous control card read 
      include 'tspinc/reread.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/prt.inc' 
C
      common /ateof/ ateof
      logical ateof
c      logical ateof / .false. / 
      character*1 col1
      data ateof /.false./
C     -
C     -     Begin     Begin     Begin     Begin     Begin     Begin
C     -
C     -    Notational and template lines have !, ., or < in col 1
C     -    Loop until a non-notational line used  
      if (ateof) goto 66
      ka = 100
      buffer = bufnxt                                                   !dem
      do 34 la = 1,ka
         read(l5, 10,end=55) bufnxt
   10       format (a80) 
         col1 = bufnxt
         if (col1 .eq. '<' .or. col1 .eq. '.' .or. col1 .eq. '!') then
            ka = ka + 1
         else 
            return 
         endif 
   34 continue
C     -  Immediate E-O-F processing.  BUFNXT is unuseable but BUFFER
C        still must be processed. 
   55 ateof = .true. 
      return
C     -  Post E-O-F processing 
   66 if(link .gt. 2) go to 100
      end file l8                                                       
      end file l8                                                       
  100 if(link .eq. 3)then                                               
         call plot(0.0,0.0,999)                                             
         stop                                                           
      endif                                                             
      write (errbuf(1), 200)                                            
      call prterr ('E',1)                                               
  200 format(' END OF FILE ENCOUNTERED ON INPUT FILE')                  
      nmx = 0                                                           
      call stime ('COMPLETE RUN')                                       
      rewind l8                                                         
C * * *                                                                 
C * * * CALL EREXIT TO CLOSE FILES AND STOP PROGRAM                     
C * * *                                                                 
      call erexit                                                       
      return                                                            
      end                                                               
