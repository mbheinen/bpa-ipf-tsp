C    @(#)kmpcord.f	20.4 8/20/98
      function kmpcord(m,n)

      include 'ipfinc/srtdta.inc'


*********************** debug stuff ******************
*     print 9901
*9901 format(' entering kmpcord')
*********************** debug stuff ******************

      kmpcord = key(m) -  key(n)
      if (kmpcord .eq. 0) then

************ header or trailer record **************
        if (key(m) .eq. 0 .or. key(m) .eq. 90) then
          continue

***** End of Options record - For purposes documentation only **********
***** This code is never reached because there is only one 'key(m) = 10'
*       elseif (key(m) .eq. 10) then
*         continue
************* bus and branch now sorted together *****

************* bus, line or tx record *******************
        elseif (key(m) .eq. 20) then
          kmpcord = kompr(bus1(m), bus1(n), junk)
          if (kmpcord .eq. 0) then
            kmpcord = (base1(m) - base1(n))*100.0
            if (kmpcord .eq. 0) then
              kmpcord = kompr(bus2(m), bus2(n), junk)
              if (kmpcord .eq. 0) then
                kmpcord = (base2(m) - base2(n))*100.0
                if (kmpcord .eq. 0) then
                  kmpcord = kntrec(m) - kntrec(n)
                endif
              endif
            endif 
          endif
         
**************** Area of Interchange record ****************
        elseif (key(m) .eq. 25) then
          kmpcord = kompr(bus1(m), bus1(n), junk)
          if (kmpcord .eq. 0) then
            kmpcord = kompr(bus2(m), bus2(n), junk)
          endif


***** Draw, Comment, Postscript or *DEFINE record **********
        elseif (key(m) .eq. 30 .or. key(m) .eq. 40) then
          kmpcord = kntrec(m) - kntrec(n)

***** Options record **********************************
        elseif (key(m) .ge. 01 .and. key(m) .le. 10) then
          kmpcord = kntrec(m) - kntrec(n)

        endif

      endif

*********************** debug stuff ******************
*     print 9911
*9911 format(' leaving kmpcord')
*********************** debug stuff ******************

      return
      end      
