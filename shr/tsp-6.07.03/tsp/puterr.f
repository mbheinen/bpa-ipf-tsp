C    %W% %G%
      subroutine puterr (linnum,text)
        character text*(*)
c     
c     -  Sticks incoming text into a numbered error buffer.  Using 
c        this routine removes need for calling routine to use the
c        printer package common blocks.
c
      include 'tspinc/prt.inc'
c 
      write (errbuf(linnum),'(a)') text
      return
      end
