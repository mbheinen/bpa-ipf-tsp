C    @(#)chmisc.f	20.4 2/13/96
      subroutine chmisc
 
C     This subroutine performs miscellaneous "cleanup" processes       
C     following the conclusion of data changes.  Present processes     
C     are:                                                             
C 
C     1. Sort XDATA arrays when D/A/R changes are made to X-data.      
C     2. Sort AREAC/AREINC arrays.                                     
C     3. Sort AGC array.                                               
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qsdup.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/xsrt.inc'

      common /is_batch / is_batch

      character rcdimg * 120
 
      external kmpari, swpari, kmparc, swparc, kmpagc, swpagc
 
      if (numchg .eq. 0) go to 320
 
      if (kxtot .eq. 0) go to 180
      do ic = 1, numchg
         if (chgcrd(ic)(126:126) .eq. 'P') then
            if (chgcrd(ic)(1:1) .eq. 'X') then
               if (index('D R',chgcrd(ic)(2:2)) .ne. 0) go to 110
            else if (chgcrd(ic)(1:1) .eq. 'B') then
               if (index('DR',chgcrd(ic)(2:2)) .ne. 0) go to 110
            endif
         endif
      enddo
      go to 180
 
C     X-Data array requires resorting.  Flag deleted entities.         
 
  110 call sort_xdta()
C 
C     Sort area interchange "AC" records  
C 
  180 if (ntotc .gt. 0) then
 
         dupsw = .false.
         call qiksrt (1,ntotc,kmparc,swparc)
         if (dupsw) then
C                                         
C           Flag and remove duplicate records 
C                                             
            j = 1
            k = 2
  190       if (k .le. ntotc) then
               if ( kmparc(j,k) .lt. 0 ) then
                  j = j + 1
                  if (j .lt. k) call swparc(j,k)
                  k = k + 1
 
               else
 
                  write (errbuf(1),200)
  200             format ('duplicate area interchange records')
                  call bcdarc (j,rcdimg)
                  write (errbuf(2),160) rcdimg(1:40)
  160             format (' kept---- ', a)
                  call bcdarc (k,rcdimg)
                  write (errbuf(3),170) rcdimg(1:40)
  170             format (' deleted- ', a)
                  call prterx ('W',3)
                  k = k + 1
               endif
               go to 190
 
            else
 
               ntotc = j
            endif
 
         endif
      endif
C                                             
C     Sort area intertie "I" records    
C                                       
      if (ntotic .gt. 0) then
         dupsw = .false.
         call qiksrt (1,ntotic,kmpari,swpari)
         if (dupsw) then
c                                       
c           remove duplicate records    
c                                       
            ntotxx = ntotic
            do 260 i = 2, ntotxx
               if (i .gt. ntotic) go to 270
               if (arcint(1,i) .eq. arcint(1,i-1) .and. arcint(2,i)
     1            .eq. arcint (2,i-1)) then
c                                                                      
c                 Duplicate found. if low-high alpha, delete second 
c                 entiity.

c                 if high-low alpha, delete corresponding transposed   
c                 duplicate intertie.                                  
c                                                                      
                  if (kompr(arcint(1,i),arcint(2,i),junk) .lt. 0) then
                     ikeep = i
                     idelet = i - 1
                  else
                     do 210 j = 1, i - 1
                        if (arcint(1,j) .eq. arcint(2,i) .and.
     1                     arcint(2,j) .eq. arcint(1,i)) then
                           if (arcinp(j) .eq. -arcinp(i)) then
                              ikeep = i
                              idelet = i - 1
                           else
                              ikeep = i - 1
                              idelet = i
                           endif
                           go to 230
                        endif
  210                continue
                     write (errbuf(1),220)
  220                format (
     1                  'transpose of area intertie "i" record is ',
     2                  'not in system.')
                     call bcdari (i,rcdimg)
                     write (errbuf(2),160) rcdimg
                     if (is_batch .eq. 0) then
                        call prterx ('E',1)
                     else
                        call prterx ('F',1)
                     endif
                     ikeep = i - 1
                     idelet = i
                  endif
  230             write (errbuf(1),240)
  240             format ('Duplicate area intertie "I" records. ',
     &                    'Second one deleted.')
                  call bcdari (ikeep,rcdimg)
                  write (errbuf(2),160) rcdimg(1:40)
                  call bcdari(idelet,rcdimg)
                  write (errbuf(3),160) rcdimg(1:40)
                  call prterx ('W',3)
                  if (ikeep .lt. idelet) then
                     istart = ikeep
                  else
                     istart = ikeep + 2
                  endif
                  do 250 j = istart, ntotic
                     arcint(1,j-1) = arcint(1,j)
                     arcint(2,j-1) = arcint(2,j)
                     arcinp(j-1) = arcinp(j)
  250             continue
                  ntotic = ntotic - 1
               endif
  260       continue
         endif
      endif
c                                
c     Sort AGC records           
c                                
  270 if (numagc .gt. 0) then
 
         do i = 1, numagc
            if (kagc(1,i) .le. 0) kagc(1,i) = 99999
         enddo
         dupsw = .false.
         call qiksrt (1,numagc,kmpagc,swpagc)
         if (dupsw) then
c                                
c           Flag and remove duplicate records  
c                                
            j = 1
            k = 2
  290       if (k .le. numagc) then
               if ( kmpagc(j,k) .lt. 0 ) then
                  j = j + 1
                  if (j .lt. k) call swpagc(j,k)
                  k = k + 1
 
               else if (kagc(1,j) .ne. 99999) then
 
                  write (errbuf(1),300 )
  300             format ('Duplicate AGC records')
                  call bcdbus (kagc(1,j),rcdimg)
                  write (errbuf(2),160) rcdimg(1:40)
                  call prterx ('w',2)
                  k = k + 1
               else
                  numagc = j - 1
                  go to 310
               endif
               go to 290
            else
               numagc = j
            endif
 
         endif
      endif
  310 continue
  320 return
      end
