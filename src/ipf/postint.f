C    @(#)postint.f	20.4 1/7/99
      subroutine postint(record)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/bscords.inc'
      include 'ipfinc/pasprm.inc'

      character record*(*), type*1
********************* debug stuff **********************
*     print 9901
*9901 format(' entering postint')
********************* debug stuff **********************
      read(record,101) type, bubpf2, schflo, actflo, crcflo 
  101 format(bz, a1, t14, a10, t25, 3(e15.7))

*********** discard lines that are not of interest **************
      if ( bubpf1 .gt. bubpf2 ) go to 1000
      indcor2 = intchcor(bubpf2)
********************* debug stuff **********************      
*     print 9931, bubpf2, indcor2
*9931 format(' bubpf2,  indcor2 :', a8,i5)
********************* debug stuff **********************      
      if (indcor2 .le. 0) go to 1000
*********** lines that are not of interest have been discarded ********
****** define values in case there is no coordinate file *****
      karosg = 1       ! section number that arrow is in

******* build the coordinate track for a line ******************    
      indcor1 = intchcor(bubpf1)
      if (indcor1 .le. 0) go to 1000
********************* debug stuff **********************      
*     print 9951, bubpf1 indcor1
*9951 format(' bubpf1, indcor1', a8,i5)
********************* debug stuff **********************      
      write (lpost, 201)
  201 format ('[ ')
      if (kprflg(indcor1) .eq. 0) 
     1 write(lpost,211) xcord(indcor1), ycord(indcor1)
  211 format(2(f6.2, ' cmtr '))

******* look for branch coordinate data

****** get next coordinate record *******************
  220 continue
********************* debug stuff **********************
*     print 9941, kcorflg
*9941 format('kcorflg = :', i2)
********************* debug stuff **********************

      if (kcorflg .eq. 0) then
          read (corfil, 221, end=238) nxtcord
  221     format(a)
          kcorflg = 1
      elseif (kcorflg .eq. 1) then
****** information is already in nxtcord *******************
          continue
      elseif (kcorflg .eq. 2) then
          go to 240
      endif
********************* debug stuff **********************   
*     print 9961, nxtcord
*9961 format('nxtcord = :', a)
********************* debug stuff **********************   

      if ( nxtcord(1:1) .eq. 'I' ) then
         read (nxtcord, 231) bubcd1, bubcd2
  231    format(bz, t3, a10, 1x, a10 ) 

         if     (bubpf1 .gt. bubcd1) then  
             kcorflg = 0
             go to 220
         elseif (bubpf1 .eq. bubcd1  .and. bubpf2 .gt. bubcd2) then
              kcorflg = 0
              go to 220
         elseif (bubpf1 .eq. bubcd1 .and. bubpf2  .eq. bubcd2) then

            read (nxtcord, 233) karosg
  233       format(bz, t29, i2)
            if (karosg .eq. 0) karosg = 1
            kcorflg = 0
            ind1 = 31
            max = 90

            do while (ind1 .lt. max) 
            if (nxtcord(ind1:ind1+11) .eq. ' ') go to 240
            read (nxtcord(ind1:ind1+11), 235) xcorda, ycorda
  235       format(bz, 2f6.2)
            write(lpost,211) xcorda, ycorda 
            ind1 = ind1 + 12
            end do 
         else
            kcorflg = 1
         endif

      endif
      go to 240
  238 kcorflg = 2
  240 continue
      if (kprflg(indcor2) .eq. 0)
     1 write(lpost,211) xcord(indcor2), ycord(indcor2)

      write (lpost, 251)
  251 format (' ]')

****** build remainder of parameters for call to acLine *********
      ischflo = sign ((abs(schflo) + .5), schflo)
      iactflo = sign ((abs(actflo) + .5), actflo)
      icrcflo = sign ((abs(crcflo) + .5), crcflo)
      write (lpost, 351) karosg, ischflo, iactflo, icrcflo 
  351 format(i5, 3i10, ' areaInt') 
 1000 continue
********************* debug stuff **********************
*     print 9999
*9999 format(' exiting postint')
********************* debug stuff **********************
      return
      end
