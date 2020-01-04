C    @(#)prtime.f	20.3 2/13/96
      subroutine prtime(a)
      character*(*) a
c
c       Routine to time power flow functions...
c
 
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/timmsg.inc'
 
C
      character*8 timed
      character*10 user

      logical batch,chkbch

      integer savcrt, svlprt, svfich

      save batch, ln
      save savcrt, svfich, svlprt, del_time, timed, tstart, tot_time
c
      savcrt=crtsw
      if(.not.chkbch(batch))crtsw=1
      svlprt = lprtsw
      svfich = fichsw
      lprtsw=0
      fichsw=0
      ln = len (a)
      ln = min0 (ln,20)
C
      if(a.ne.'START') then
C
         del_time = cpu_secs(tstart)      ! Delta time for this process
         tot_time = tot_time + del_time   ! Total time for this Job
         tstart = tot_time
         call timoda( timed )
c         write (outbuf,1) a(1:ln), del_time, tot_time, timed
c 1       FORMAT( '0 Module "', a, '" processed : ELAPSED CP TIME =',
c     1           F8.2, '   Accumulated process time =', F8.2,
c     2           '   Time of day = ',A )
c         call prtout(1)
         outbuf(1:1)='0'
c         write (dbug,2) outbuf
c 2       format(a)
      else
         iws = 6000
c***         itfn = 0
         del_time = 0.0
         tot_time = 0.0
         tstart = cpu_secs(0.0)
         call get_user( user )
c         write (outbuf,201) iws, user
c 201     FORMAT(' START RUN WORKING SIZE = ', I5, '   BUSSES',
c     &          '     USER = ', A )
c         call prtout(1)
c         write (dbug,2) outbuf
      endif
c
      if(msgnum.lt.100) then
         msgnum=msgnum+1
c***         write (tmsg(msgnum),133) a(1:ln),del_time,tot_time,timed,itfn
c***133      format(1x,a20,10x,f8.2,13x,f8.2,11x,a8,14x,i6)
         write (tmsg(msgnum),133) a(1:ln),del_time,tot_time,timed
133      format(1x,a20,12x,f8.2,14x,f8.2,11x,a8)
      endif
c
      lprtsw = svlprt
      crtsw=savcrt
      fichsw=svfich
c
      return
      end
