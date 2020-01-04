C    %W% %G%
      subroutine repet1 (ldumf,ldumk,iswt,recnam,msold,msnew)           !DEM
        integer ldumf,ldumk,iswt,msold,msnew                            !DEM
        character recnam*3                                              !DEM
C       -  old call 
C     TUBROUTINE REPET1 (LDUMF,LDUMK,ISWT,III,MSOLD,MSNEW)              
C     -  Revs:
c          Mar/06/92 - DEM:
c            Modified input var used to fetch unit 1 record numbers
c              from INT to CHAR[3]. 
c            Fetch of unit 1 record numbers MSOLD & MSNEW are by 
c              separate but same subrtn called in most other input
c              routines.  
      include 'tspinc/blkcom1.inc' 
c     -  /START/ is shared with sub SDATA
      common /start/ iswt1,iswt2,iswt3,iswt4,kswt1,kswt2                
c     -  Local variables 
      character*80 outbuf                                               
C     - 
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      call whrec1 (recnam,msold,msnew,isz)                              !DEM
C     MSOLD = MASOLD(III)                                               
C     MSNEW = MSOLD + 100                                               
      iswt1=0                                                           
        iswt2=0                                                         
        iswt3=0                                                         
        iswt4=0                                                         
        kswt1=0                                                         
        kswt2=0                                                         
      if(ldumf.eq.0.and.ldumk.eq.0)iswt=1                               
      if(ldumf.eq.0.and.ldumk.gt.0)iswt=2                               
      if(ldumf.gt.0.and.ldumk.eq.0)iswt=3                               
      if(ldumf.gt.0.and.ldumk.gt.0)iswt=4                               
      if (keybrd(20) .ne. 0) then                                       !DEM 
        write (outbuf,200) ldumf,ldumk,iswt                             !DEM  
  200     format(1h0,3i10)                                              !DEM
        call prtout (1)                                                 !DEM
      endif                                                             !DEM
  220 continue                                                          
      return                                                            
      end                                                               
