C    @(#)ftn_atoi.f	20.1 10/10/96
C****************************************************************
C
C     File: ftn_atoi.f
C
C     Purpose: Function to obtain integer value of alphameric argument
C              (emulates popular C-function of same name)
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: 
C
C****************************************************************
      integer function ftn_atoi (xbuf)
      character *(*) xbuf

      character fmt*6

      l = lastch(xbuf)
      write (fmt, 100) l
  100 format ('(i', i2, ')')
   
      read (xbuf, fmt=fmt, err=110) ftn_atoi
      go to 120

  110 ftn_atoi = 0
  120 return
      end
