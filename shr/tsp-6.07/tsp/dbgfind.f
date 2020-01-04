C    %W% %G%
      logical function dbgfind (infile,outfile)
      implicit none
      character infile*(*),outfile*(*)

      include 'tspinc/blkcom1.inc'

c     Indicates whether any debugging is to be done.  
c     It attempts to open the DBGIN file; if not found returns a
c     false, but if found, reads the list of routines to be debugged.
c     
      common /dbgset/ anydbg, sublist(20)
      logical anydbg
      character sublist*8 
c
      integer la, lb, kbeg, kend, status, open_file, iostat
      character ch32*32
      logical debug 

      data debug /.true./

      if (debug) write (*, '(a)') 'DBGFIND - at start'
      dbgfind = .false.
      anydbg  = .false.
      do la = 1,20
        sublist(la) = ' '
      enddo 
      if (infile .eq. '0')  return
      if (outfile .eq. '0') return
      if (debug) then
         write (*, '(2a)') '  Debug infile:  ',infile
         write (*, '(2a)') '  Debug outfile: ',outfile
      endif

      status = open_file (l12, infile, 'F', 'R', iostat)
      if (status .ne. 0) go to 911

      lb = 1
      do 741 la = 1,60
        read (l12,'(a32)',end=743) ch32
        call nxtwrd (ch32,0,kbeg,kend)
        if (kbeg .eq. 0) goto 741
        if (ch32(kbeg:kbeg) .eq. '!') goto 741

c       here if line has a subroutine name

        sublist(lb) = ch32(kbeg:kend) 
        write (*,'(2a)') '   Will debug routine ',sublist(lb)
        lb = lb + 1
        anydbg  = .true.
        dbgfind = .true.
 741  continue 

c     If doing any debugging, open debug file

 743  continue
      if (anydbg) then 
        status = open_file (l13, outfile, 'F', 'W', iostat)
        if (status .ne. 0) then
          call puterr (1,'Couldn''t open debug output file. Debug off.')
          call prterr ('W',1)
          anydbg  = .false.
          dbgfind = .false.
        endif
      endif
c
 911  return
      end
