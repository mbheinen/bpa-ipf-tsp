C    @(#)xdate.f	20.3 2/13/96
      subroutine xdate(date)
      character * (*) date

C   THIS ROUTINE GETS THE CURRENT DATE FROM THE SYSTEM
C   AS INTEGER VALUES:
C        M = MONTH
C        D = DAY
C        Y = YEAR
C   AND REFORMATS IT AS THE CHARACTER STRING:
C        'MM/DD/YY'
C   WHICH IS RETURNED IN "DATE"

      integer m, d, y

      call n_date( m, d, y )
      write (date,11) m, d, y
11    format( i2, '/', i2, '/', i2 )
      return
      end
