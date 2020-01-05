C    @(#)getdif.f	20.3 2/13/96
        character *(*) function getdif (otext, ntext)
        character otext *(*), ntext *(*)
c
c       Obtain differences (modifications) between otext and ntext
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        character bus_dif * 120, cbus_dif * 120, xbus_dif * 120, 
     &            qpd_dif * 120, brn_dif * 120, area_dif * 120, 
     &            int_dif * 120
        integer type1, type2

        type1 = index ('AIB+XQRTEL', otext(1:1))
        type2 = index ('AIB+XQRTEL', ntext(1:1))
c        if (otext(1:1) .eq. ntext(1:1)) then
        if ( type1 .ne. 0  .and.  type1 .eq. type2 ) then
           if (otext(1:1) .eq. 'A') then
c              getdif = area_dif(otext, ntext)
               getdif = ntext
               getdif(3:3) = 'M'
           else if (otext(1:1) .eq. 'I') then
              getdif = int_dif(otext, ntext)
           else if (otext(1:1) .eq. 'B') then
              getdif = bus_dif(otext, ntext)
           else if (otext(1:1) .eq. '+') then
              getdif = cbus_dif(otext, ntext)
           else if (otext(1:1) .eq. 'X') then
              getdif = xbus_dif(otext, ntext)
           else if (otext(1:1) .eq. 'Q') then
              getdif = qpd_dif(otext, ntext)
           else 
              getdif = brn_dif(otext, ntext)
           endif
        else
           write (errbuf(1), 100) otext(1:33)
  100      format ('Improper call to "getdif" : ', a)
           write (errbuf(2), 110) ntext(1:33)
  110      format ('                          : ', a)
           call prterx ('W', 2)
        endif
        return
        end
