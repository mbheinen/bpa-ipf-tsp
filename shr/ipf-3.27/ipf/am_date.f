C    @(#)am_date.f	20.5 2/28/00
      subroutine am_date( date )
      character * (*) date

C     THIS ROUTINE GETS THE CURRENT DATE FROM THE SYSTEM
C     AS INTEGER VALUES:
C        M = MONTH
C        D = DAY
C        Y = YEAR
C     AND REFORMATS IT AS THE CHARACTER STRING:
C        'DD-MMM-YY'
C     WHICH IS RETURNED IN "DATE"

      integer m, d, y
      character month(12)*3
      data month / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
 
      call n_date( m, d, y )
      write (date,11) d, month(m), y
11    format( i2, '-', a3, '-', i2.2 )
      return
      end
