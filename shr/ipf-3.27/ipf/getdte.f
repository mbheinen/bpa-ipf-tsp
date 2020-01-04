C    @(#)getdte.f	20.3 2/13/96
        function getdte (date)
C
C       This function computes the decimal energization and deenergizati
C       dates as a funcition of DATE (:myy)
C
C       GETDTE = 1900.0 + yy + m/13
C
C       Note the 13 months because no month is less than January.
C
        character date*3, date1*3, date2*3
        integer year
        save

        getdte = 0.0
        return
C
C       Compute energization date.  Note that no date implies zero
C       energization date.
C
        entry energd (date1)
        month = index ('123456789OND',date1(1:1))
        read (date1(2:3),100,err=110) year
  100   format (i2)
        if (index('456789',date1(2:2)) .gt. 0) then
           year = year + 1900
        else if (index('0123',date1(2:2)) .gt. 0) then
           year = year + 2000
        endif
        energd = float(year) + float(month)/13.0
        return
C
C       Flag illegal-data-in-date-field error with -9999.0
C
  110   energd = -9999.0
        return
C
C       Compute de-energization date.  Note that no date implies infinit
C       de-energization date.
C
        entry denerg (date2)
        month = index ('123456789OND',date2(1:1))
        read (date2(2:3),100, err=120) year
        if (index('456789',date2(2:2)) .gt. 0) then
           year = year + 1900
        else if (index('0123',date2(2:2)) .gt. 0) then
           year = year + 2000
        else
           year = 9999
        endif
        denerg = float(year) + float(month)/13.0
        return
C
C       Flag illegal-data-in-date-field error with -9999.0
C
  120   denerg = -9999.0
        return
 
        end
