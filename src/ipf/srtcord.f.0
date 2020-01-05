C    %W% %G%
      subroutine srtcord

      include 'ipfinc/pasprm.inc'
      include 'ipfinc/srtdta.inc'
      include 'ipfinc/bscords.inc'
      include 'ipfinc/postdta.inc'
      include 'ipfinc/apcrfil.inc'
      include 'ipfinc/prt.inc'

      character rectyp*1, cordset(5)*12
      character cordfmt*8, bufsrt1*90
      character cktid*1 
      character longfil*80 
      character busnosho(100)*8
      dimension basnosho(100) 
      external kmpcord, swpcord
      integer error, status, last, open_file
      integer filread    ! 1 - reading a coordinate file
c                        ! 2 - reading options from usropts() 

************************* debug stuff ********************
      character junk*1
************************* debug stuff ********************


      indrec = 0  
      inddcp = 0
      indopt = 0
      inddef = 0
      kntnosho = 0
      maxnosho = 100
      cordfmt = '(a90)'
      filread = 1
      numuopt = 0


************ build tables for sorting coordinate data **
************ read coordinate header record *************
************** debug stuff **************************
*     print 9961, morcord
*9961 format(' reading cord record morcord = :',i2)
************** debug stuff **************************

      if (morcord .eq. 1) then
           read (lcord, cordfmt, end = 197) bufsrt 
      else 
           bufsrt= morbuf  
      endif
         indrec = indrec + 1

******************* initialize sort data *****************
         key  (indrec) = 0
         kntrec(indrec) = 0
         bus1 (indrec) = ' ' 
         base1(indrec) = 0.0
         bus2 (indrec) = ' ' 
         base2(indrec) = 0.0

******************* initialize sort data *****************

      if ( bufsrt(1:5) .eq. '[ID C') then
c        indrec = indrec + 1
c        key(indrec) = 0
         ascrec(indrec) = bufsrt
      else
         errbuf(1)(1:80) = 'Unrecognized Coordinate File'
         errcord = 1
         call prterx ('W',1)
         indrec = indrec - 1
         return
      endif
         
************** read remainder of coordinate file ********
  110 if (filread .eq. 1) then 
        read (lcord, cordfmt, end = 1197) bufsrt
        indrec = indrec + 1
      else
        numuopt = numuopt + 1
        indrec = indrec + 1
        if (numuopt .gt. kntuopt) go to 197
        read (usropts(numuopt), cordfmt) bufsrt
      endif


******************* initialize sort data *****************
         key  (indrec) = 0
         kntrec(indrec) = 0
         bus1 (indrec) = ' '
         base1(indrec) = 0.0
         bus2 (indrec) = ' '
         base2(indrec) = 0.0
******************* initialize sort data *****************

************************* debug stuff *********************
*      print 9901, bufsrt
* 9901 format(' srtcord at 110',a) 
************************* debug stuff *********************

************ determine record type **************
      rectyp = bufsrt(1:1)

      if     (rectyp .eq. 'O') then
**** Begin-check for 'special' types of options*********
           call uscan(bufsrt, prsmsg, nwrds, '@', ' ,=')
           if (nwrds .ge. 2) then
              msgid = capital(prsmsg(2)(1:2))
**** Begin-check for BX option to automatically generate lables 
              if (msgid .eq. 'BX') then
                 icrdfls = icrdfls + 1
                 if (icrdfls .le. icrdmax) then
                    lcord = mcrdfls(icrdfls)
c fix open 5/6
      last = lastch (postmstr)
************************* debug stuff *********************
*     print 9971, last, postmstr
*9971 format(' last, postmstr' i5,1x,a)
************************* debug stuff *********************
      status = open_file(lcord, postmstr(1:last), 'F', 'R', ios)
      if (status .ne. 0) then
        print 137, ios, postmstr(1:last), lcord
         write (errbuf(1), 137) ios, postmstr(1:last), lcord
  137    format (' Error No. ', i3,
     1           ' opening file ', a, ' on logical unit', i3)
         call prterx ('W',1)
         icrdfls = icrdfls - 1
         lcord = mcrdfls(icrdfls)
         indrec = indrec - 1
         go to 120
      endif
******* look in the first 100 records for .COR type data 
                    do i1 = 1,100
                      read (lcord, cordfmt) bufsrt1
                      if ( bufsrt1(1:2) .eq. '%[' .and.
     1                bufsrt1(3:4) .eq. msgid) then
                         key(indrec) = 04
                         cordfmt = '(1x,a70)'
                         filread = 1
                         go to 119
                      endif
                    enddo
                 endif
c print error when BX not found in pfmaster.post
  112            write (errbuf(1), 113) 
  113            format (' NO %[BX record in ',
     1           'pfmaster.post as number ', i2, ' inserted file')
                 call prterx ('W',1)
                 icrdfls = icrdfls - 1
                 close (unit=lcord)
                 lcord = mcrdfls(icrdfls)
                 indrec = indrec - 1
                 go to 120
**** Begin-check for included (nest) a coordinate file**********
              elseif (msgid .eq. 'FI') then
                 if (icrdfls .ge. icrdmax) then
  116               print 117,  prsmsg(3), icrdfls
  117               format (' File Overflow - Can not open ',a,
     1              ' as number ', i2, ' inserted file')  
                    write (errbuf(1), 117) prsmsg(3), icrdfls 
                    call prterx ('W',1)
                    go to 120
                 else
                    icrdfls = icrdfls + 1
                    lcord = mcrdfls(icrdfls)   
                    last = lastch (prsmsg(3))
                    longfil = prsmsg(3)(1:last) 
                    call opnfila(lcord, longfil, status)
                    if (status .eq. 0) go to 120 
                    print 137, ios, prsmsg(3)(1:last), lcord
                    write (errbuf(1), 137) ios, postmstr(1:last), lcord
                    call prterx ('W',1)
                    icrdfls = icrdfls - 1
                    lcord = mcrdfls(icrdfls)
                    indrec = indrec - 1
                    go to 120
                 endif
**** End---check for included (nest) a coordinate file**********
              endif
           endif              
              
**** End-check for 'special' types of options*********

           key(indrec) = 05
  119      indopt = indopt + 1
           kntrec(indrec) = indopt
           ascrec(indrec) = bufsrt
  120      continue

      elseif (rectyp .eq. 'B') then
c          indrec = indrec + 1
           key(indrec) = 20
           read(bufsrt, 121, err=400) bus1(indrec), base1(indrec) 
  121      format( 2x, a8, f4.0)
           ascrec(indrec) = bufsrt
           if (bufsrt(2:2) .eq. '1') then
             if (kntnosho .lt. maxnosho) then
               kntnosho = kntnosho + 1
               busnosho (kntnosho) =  bus1(indrec)
               basnosho (kntnosho) =  base1(indrec)
             endif
           endif

      elseif (rectyp .eq. 'L' .or. rectyp .eq. 'T') then 
c          indrec = indrec + 1
           key(indrec) = 20
           read(bufsrt, 131, err=400) bus1(indrec), base1(indrec),
     1        bus2(indrec), base2(indrec), cktid, nseg
  131      format( 2x, 2(a8, f4.0), a1, 2x, i1)
           kntrec(indrec) = ichar(cktid)
           if (nseg .eq. 0) nseg = 1
************* check for reversed order *********
           if ( (bus1(indrec)  .gt.  bus2(indrec)) .or. 
     1         ((bus1(indrec)  .eq.  bus2(indrec)) .and.
     2          (base1(indrec) .gt. base2(indrec)) )) then
************** switch names ****************
                temp( 1:12) = bufsrt ( 3:14)
                bufsrt ( 3:14) = bufsrt (15:26)
                bufsrt (15:26) = temp( 1:12)

************** switch coordinates ************
                kntseg = 2
                do 150 i1 = 1,5
                i2 = 12 * (i1 -1) + 31
                i3 = i2 + 11
                cordset(i1) =  bufsrt (i2:i3)
**************** debug stuff ****************************     
*     print 9931, cordset(i1)
*9931 format(' cordset = :', a)
**************** debug stuff ****************************     
                if (cordset(i1) .ne. ' ') kntseg = kntseg + 1
  150           continue 
                nseg = kntseg - nseg
********** check for non-printed bus and adjust nseg *******
                if (kntnosho .gt. 0) then
                  do nosho = 1, kntnosho
                    if (bus2(indrec) .eq. busnosho(nosho) .and.
     1                 base2(indrec) .eq. basnosho(nosho)) then
                      if (nseg .gt. 1) nseg = nseg - 1
                      go to 160        
                    endif
                  enddo
                endif
  160           write (bufsrt(30:30), 161) nseg
  161           format(i1)
                do 170 i1 = 1, kntseg - 2
                i2 = 12 * (i1 -1) + 31
                i3 = i2 + 11
                bufsrt (i2:i3) = cordset(kntseg - 1 - i1)
  170           continue
 
           read(bufsrt, 131, err=400) bus1(indrec), base1(indrec),
     1                    bus2(indrec), base2(indrec)

c          else
c               continue
           endif
                ascrec(indrec) = bufsrt
******************* check for area record **************
      elseif (rectyp .eq. 'A') then
           key(indrec) = 25
           read(bufsrt, 181, err=400) bus1(indrec)
  181      format( 2x, a10)
           ascrec(indrec) = bufsrt
******************* check for area record **************

******************* check for interchange record ********** 
      elseif (rectyp .eq. 'I' ) then
           key(indrec) = 25
           read(bufsrt, 191, err=400) bus1(indrec), bus2(indrec), nseg
  191      format( 2x, a10,1x,a10, t30, i1)
           if (nseg .eq. 0) nseg = 1
************* check for reversed order *********
           if (bus1(indrec)  .gt.  bus2(indrec)) then 
************** switch names ****************
                temp( 1:10) = bufsrt ( 3:12)
                bufsrt ( 3:12) = bufsrt (14:23)
                bufsrt (14:23) = temp( 1:10)
 
************** switch coordinates ************
                kntseg = 2
                do 194 i1 = 1,5
                i2 = 12 * (i1 -1) + 31
                i3 = i2 + 11
                cordset(i1) =  bufsrt (i2:i3)
**************** debug stuff ****************************
*     print 9931, cordset(i1)
**************** debug stuff ****************************
                if (cordset(i1) .ne. ' ') kntseg = kntseg + 1
  194           continue
                nseg = kntseg - nseg
                write (bufsrt(30:30), 161) nseg
                do 196 i1 = 1, kntseg - 2
                i2 = 12 * (i1 -1) + 31
                i3 = i2 + 11
                bufsrt (i2:i3) = cordset(kntseg - 1 - i1)
  196           continue
 
           read(bufsrt, 191, err=400) bus1(indrec), base2(indrec)
 
           endif
                ascrec(indrec) = bufsrt
******************* check for interchange record ********** 

******************* look for another COORD set *************
      elseif (bufsrt(1:5) .eq. '[ID C' ) then
           morbuf = bufsrt
c          indrec = indrec + 1
           key(indrec) = 90
           ascrec(indrec) = '9'
           morcord = 2

************************* debug stuff ********************
*     print 9941
*9941 format('in srtcord setting morcord = 2',
*    1' send a character to continue')
*     read 9951, junk
*9951 format(a)
************************* debug stuff ********************
           go to 200

      elseif (rectyp .eq. 'D' .or. rectyp .eq. 'C' .or.  
     1        rectyp .eq. 'P' ) then
c          indrec = indrec + 1
           key(indrec) = 30
           inddcp = inddcp + 1
           kntrec(indrec) = inddcp 
           ascrec(indrec) = bufsrt

      elseif (bufsrt(1:7) .eq. '>DEFINE') then
c          indrec = indrec + 1
           key(indrec) = 40
           inddef = inddef + 1
           kntrec(indrec) = inddef
           ascrec(indrec) = bufsrt

      elseif (rectyp .eq. '9' .or. rectyp .eq. '(' ) then
        go to 197
 
      elseif (rectyp .eq. '#' .or. rectyp .eq. '!' ) then
        indrec = indrec - 1
 
      else
*       errbuf(1)(1:32) = 'Unrecognized Coordinate Record :'
*       errbuf(1)(33:80) = bufsrt(1:47)
        errbuf(1) = 'Unrecognized Coordinate Record: ' // bufsrt(1:47)
        call prterx ('W',1)
        indrec = indrec - 1
 
      endif
      go to 110

************** required when reading at 110 ****** 
 1197 indrec = indrec + 1
  197 if (icrdfls .eq. 1 .and. iapcrfil .eq. 0
     1 .and. numuopt .ge. kntuopt) then
        key(indrec) = 90
c       ascrec(indrec) = bufsrt
        ascrec(indrec) = ' The End '
        morcord = 3

************************* debug stuff *********************
*     print 9921
*9921 format(' before go to 200') 
************************* debug stuff *********************
        go to 200

      elseif (icrdfls .gt. 1) then
        icrdfls = icrdfls - 1 
        close (unit = lcord) 
        lcord = mcrdfls(icrdfls) 
        cordfmt = '(a90)'
        filread = 1
        indrec = indrec - 1

      elseif (kntuopt .gt. numuopt) then
        cordfmt = '(a90)'
        filread = 2
        indrec = indrec - 1

      else 
        iapcrfil = 0
        close (unit = lcord) 
        last = lastch (apcrfil)
         longfil = apcrfil(1:last)
************************* debug stuff *********************
           status = 999
*     print 9921,longfil, status
*9921 format(' attempt to append: ',a,1x, i5) 
************************* debug stuff *********************
         call opnfila(lcord, longfil,  status)
************************* debug stuff *********************
*     print 9921, longfil, status
************************* debug stuff *********************
        if (status .eq. 0) then
           cordfmt = '(a90)'
           filread = 1
           indrec = indrec - 1
        else 
           print 199,  apcrfil(1:last)
  199      format(' *** WARNING ***  Failure to append a file:',a)
           write (errbuf(1), 199) apcrfil(1:last)
*          call prterx ('W',1)
*          open(unit=lcord, file=apcrfil, status='old',
*      1   form='formatted', err=198, iostat = error)
* 198      key(indrec) = 90
           key(indrec) = 90
           ascrec(indrec) = bufsrt
           morcord = 3
************************* debug stuff *********************
*     print 9921,apcrfil
************************* debug stuff *********************
           go to 200
        endif
      endif

      go to 110

  400 write (errbuf(1), 401) bufsrt 
  401 format (' Incomplete processing of record:',/,a)
      call prterx ('W',2)
      indrec = indrec - 1
      go to 110



**************** sort the data ***************

  200 continue
************************* debug stuff *********************
*     print 9911
*9911 format (' at 200')
************************* debug stuff *********************

****** build record to indicate end of OPTION records
      indrec = indrec + 1
      key(indrec) = 10
      bus1 (indrec) = ' '
      base1(indrec) = 0.0
      bus2 (indrec) = ' '
      base2(indrec) = 0.0
      ascrec(indrec) = 'O END = 0'

      call qiksrt(1, indrec, kmpcord, swpcord)
      kntbus = 0
      kntarea = 0
      rewind corfil
      do 300 i1 = 1, indrec
************************* debug stuff *********************
*      print 8011, ascrec(i1)
* 8011 format('processing sorted records', / a)
************************* debug stuff *********************
      write(corfil,211) ascrec(i1)
  211 format(a)
****** build table of names and coordinates for buses in coordinate file **
      if (ascrec(i1)(1:1) .eq. 'B') then
         kntbus = kntbus + 1
         read(ascrec(i1), 221, err=400) kprflg(kntbus), bus(kntbus),
     1   base(kntbus), xcord(kntbus), ycord(kntbus)
  221    format(bz, 1x, i1, a8, f4.0, 9x, 2f6.2)
************************* debug stuff *********************
*     print 8021, kprflg(kntbus), bus(kntbus),
*    1   base(kntbus), xcord(kntbus), ycord(kntbus)
*8021    format(bz, 1x, i1,1x, a8,1x, f5.0, 9x, 2(f6.2,1x))
************************* debug stuff *********************

****** save only one non-printing bus in tables ************* 
         if ( kprflg(kntbus) .eq. 1 .and. kntbus .gt. 1) then
            if (bus (kntbus) .eq. bus (kntbus-1) .and.
     1          base(kntbus) .eq. base(kntbus-1))
     2          kntbus = kntbus - 1 
         endif  
****** save only one non-printing bus in tables ************* 
      elseif  (ascrec(i1)(1:1) .eq. 'A') then
         kntarea = kntarea + 1
         knttmp = kntbus+kntarea
         read(ascrec(i1), 231, err=400) kprflg(knttmp), bus(knttmp),
     1   xcord(knttmp), ycord(knttmp)
  231    format(bz, 1x, i1, a10, t24,  2f6.2)
 
****** save only one non-printing bus in tables *************
         if ( kprflg(knttmp) .eq. 1 .and. kntarea .gt. 1) then
            if (bus (knttmp) .eq. bus (knttmp-1)) 
     1          kntarea = kntarea - 1
         endif



      endif
****** build table of names and coordinates for buses in coordinate file **

  300 continue
************************* debug stuff *********************

      return 
      end
