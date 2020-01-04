C    %W% %G%
      subroutine putout (crgctl,text)
        character crgctl*1, text*(*)
c     
c     -  Sticks incoming text into a general output buffer.  Using 
c        this routine removes need for calling routine to use the
c        printer package common blocks.
c  Aug/03/92  -  DEM
c     -  The first argument is the printer carriage control.
c
      include 'tspinc/prt.inc'
c 
      write (outbuf,'(a1,a)') crgctl,text
      return
      end
