C    @(#)initlz.f	20.6 7/18/96
      subroutine initlz(istat)

      include 'ipfinc/arcvfile.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/coment.inc'

      common /is_batch / is_batch

      character buff*80, tempc*10
      character usernm*10, debugf*60, printf*60, fichef*60
      integer ofstat, open_file
      logical arc_file_opened

      save

      data arc_file_opened / .false. /

      call init_bd_all

      is_batch = 0   ! 0 = interactive, 1 = batch
c
c     Caution: the following logical assignment must be coordinated
c     with pfinit.f
c
      batch = .false.
      inp = 13
      lprt = 14
      mfich = 15
      dbug  = 19 
      arcvfile = 51

      usernm = ' '
      call get_user( usernm )
      user = usernm

      lenunm = lastch( usernm )
 
c***      if ( lenunm .gt. 8 ) lenunm = 8  ! DOS only
      debugf = usernm(1:lenunm) // '.pfd'
      printf = usernm(1:lenunm) // '.pfo'
      fichef = usernm(1:lenunm) // '.pff'

c**********************************************************************
c***  do not use "open_file" on "dbug" since "open_file" writes to
c***  "dbug" (before it is open)
      tempc = 'unknown'
      if ( is_it_vms() .eq. 1 ) tempc = 'new'
      open(unit=dbug, file=debugf, status=tempc, form='formatted')
c***        ofstat = open_file(dbug,debugf,'FF','W',iostat)
c**********************************************************************

c     open(unit=lprt, file=printf, status='unknown', form='formatted')
c     open(unit=mfich, file=fichef, status='unknown', form='formatted')
      ofstat = open_file(lprt,printf,'F','W',iostat)
      ofstat = open_file(mfich,fichef,'F','W',iostat)

c### arcvfile ###      disable archive logging
c      if (.not. arc_file_opened) then
c         arcvname = 'archive.dat'
c         ofstat = open_file (arcvfile, arcvname, 'F', 'W', iostat)
c         arc_file_opened = .true.
c      endif



c************************************************************************
c
c     The following code translates free-field commands. It is currently
c     commented out, since no commands requiring translation have
c     been implemented.
c
c************************************************************************
c
c      maxtok = 1000
c      open (unit=inp, file='/all/ipf/dat/guitrns.dat',
c     1      iostat=ios, status='old', form='formatted')
c      if (ios .ne. 0) go to 210 
c      rewind inp
c      indset = 0
c      indtok = 0
c  100 if (indtok .ge. maxtok)then
c         write (*, 111)
c  111    format('program error - table overflow in function initlz')
c         istat = 1
c         go to 1000
c      endif
c
c      read(inp,121,end=200) buff 
c  121 format(a)
c      if (buff(1:1) .eq. '#')then
c         continue 
c      else if (buff(1:2) .eq. 'RE') then
c         indtok = indtok + 1
c         indset = indtok
c         tokno(indset) = 0
c         toksiz(indset) = 0
c 
c      else 
c         indtok = indtok + 1
c         read (buff,131) tokid(indtok),tokno(indtok),
c     1     toksiz(indtok), toktxt(indtok)
c  131    format(a7, 1x,i2,1x,i2,1x,a30)
c
c*c        print 9901, buff
c*c9901    format('after 131, buff = :',(a))
c
c         if (tokid(indtok)(1:2) .eq. 'ID') then
c            tokno(indset) = tokno(indset) + 1
c
c         else if (tokid(indtok)(1:2) .eq. 'TE' .or.
c     1            tokid(indtok)(1:2) .eq. 'CO') then
c            toksiz(indset) = toksiz(indset) + 1
c         else
c            print 141     
c  141       format('program error - bad data in GUI_TRANSLATION table')
c            istat = 1
c            go to 1000
c         endif
c      endif
c      go to 100 
c
c  200 tokmx = indtok
c      close (unit = inp)
c
c************************************************************************

  210 continue
      open (unit=inp,status='scratch',form='formatted')
      rewind inp
      istat = 0
 1000 continue
      return
      end
