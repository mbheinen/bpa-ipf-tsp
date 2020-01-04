C    @(#)eordr2.f	20.3 2/13/96
      subroutine eordr2(ktot, kdebug)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		km, kmlen, ikmu
      include 'ipfinc/blank.inc'
c	Global variables used:
c		nbslck
      include 'ipfinc/elim2.inc'
c	Global variables used:
c		next, last
      include 'ipfinc/ikk.inc'
c	Global variables used:
c		ikk
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/norder.inc'
c	Global variables used:
c		norder
      include 'ipfinc/red2.inc'
c	Global variables used:
c		ix0,loc,korder,kolum,kownt,nsize
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None

c     Note: This variable can be changed with a symbolic debugger
      common /term_debug/ iterm

C     Define the connection Matrix

      ix0 = 200
      next = 0
      kerrsw = 0
      do kt = 1, ktot
        lf = km(kt)
        ls = lf + kmlen(kt) - 1
        nt = 0
        if (kmlen(kt) .eq. 0) then
          loc(kt) = 0
        else
          loc(kt) = next + 1
          do l = lf, ls
            mt = ikmu(l)
            next = next + 1
            kolum(next) = mt
            korder(next) = next + 1
            if (nt .ge. mt) then
              write (dbug, 10000) kt, nt, mt
10000         format (' DUPLICATE OR IMPROPERLY SORTED BRANCH ', 3i6)
              kerrsw = 1
            endif
            nt = mt
          enddo
          korder(next) = 0
        endif
        kownt(1, kt) =  - kmlen(kt)
      enddo
      next = next + 1
      nsize = next + next/4
      if (nsize .gt. MAXYE) call erexit()
      last = nsize
      do i = next, last
        kolum(i) = 100000
        korder(i) = i + 1
      enddo
      korder(last) = 0

C     Check connection matrix symmetry

      write (dbug, 10002) 
      write (*, 10002) 
10002 format (' (EORDR2) Initial topological check')

      call ck_topol (ktot, kerrsw)

      if (kerrsw .ne. 0) then
        call debug(ktot)
        kdebug = max0(1, kdebug)
      endif
      call reord2(ktot, kdebug)

C     PRESERVE ORIGINAL ORDER OF SLACK NODE

      do kt = 1, nbslck
        kslack = ikk(5, kt)
        if (kslack .ne. kt) then
          do i = kt, ktot
            j = ikk(5, i)
            if (j .eq. kslack) then

              ikk(5, i) = kt
            elseif (j .le. kslack) then
              ikk(5, i) = j + 1
            endif

            norder(i) = ikk(5, i)
          enddo
        endif
      enddo
      return
      end
