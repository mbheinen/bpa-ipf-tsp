C    %W% %G%
      subroutine getids
c
c     Picks up the case ID's for the STAB, PF and SDI/O files.
c     These come from file names established in GETFNAM.  Names
c     on CASE card no longer used.  

      include 'tspinc/titles.inc' 
      include 'tspinc/files.inc' 
      include 'tspinc/blkcom1.inc'
      include 'tspinc/reread.inc'
c
      call getflcs (swcase,ctrlfl)
      call getflcs (pfcase,bsefl)
      call getflcs (savin,savifl)
      if (savin .eq. '0') savout = ' '
      if (savin .eq. 'NONE') savout = ' '
      call getflcs (savout,savofl)
      if (savout .eq. '0') savout = ' '
      if (savout .eq. 'NONE') savout = ' '
c
      return
      end
