C    @(#)atof.f	20.1 7/18/96
C****************************************************************
C
C     File: atof.f
C
C     Purpose: Function to obtain floating value of alphameric argument

C              (emulates popular C-function of same name)
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: 
C
C****************************************************************
      real function atof (xbuf)
      character *(*) xbuf

      character fmt*8

      l = lastch(xbuf)
      write (fmt, 100) l
  100 format ('(f', i2, '.0)')
   
      read (xbuf, fmt=fmt, err=110) atof
      go to 120

  110 atof = 0.0
  120 return
      end
