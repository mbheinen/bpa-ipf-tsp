C    @(#)doanlp.f	20.3 2/13/96
        subroutine doanlp
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/smallp.inc'
      include 'ipfinc/trmdbg.inc'
 
        dimension pivs(MAXN), ipivs(MAXN)
 
        if (istate .eq. 0) then
 
           call firstb
 
           if (iopton(15) .gt. 1) then
              write (dbug,8) size, numslk, xbasis(size), ybasis(size),
     1           xr(size)
    8         format (t10,'*** FIRSTB *** SIZE ',i3,' NUMSLK ',i3,
     1           ' XBASIS(*) ',i3,' YBASIS(*) ',i3,' XR(*) ',e12.5)
           endif
           if (iterm .gt. 1) then
              write (*,8) size, numslk, xbasis(size), ybasis(size),
     1           xr(size)
           endif
 
        endif
        if (istate .eq. 11) go to 20
        if (istate .eq. 12) go to 50
 
   10   continue
 
        call chslck
 
        if (iopton(15) .gt. 1) then
           write (dbug,12) itr, size, negrow, howneg
   12      format (' ITR ',i3,t10,'*** CHSLCK *** SIZE ',i3,' NEGROW '
     1         ,i3,' HOWNEG ',e12.5)
        endif
        if (iterm .gt. 1) then
           write (*,12) itr, size, negrow, howneg
        endif
 
        call chacc
 
        if (itr .le. itrmax) go to 20
        istate = 5
        go to 80
 
   20   if (negrow.eq.0 .and. neginv.eq.0) go to 40
 
        if (neginv.ne.0) go to 30
        newy = negrow + size
        driver = 1.0
        if (slack(negrow).gt.0.0) driver = -1.0
        neginv = size1
        call addcon
 
        if (iopton(15) .gt. 1) then
           write (dbug,22) size, numslk, xbasis(size), ybasis(size),
     1        xr(size)
   22      format (t10,'*** ADDCON *** SIZE ',i3,' NUMSLK ',i3,
     1        ' XBASIS(*) ',i3,' YBASIS(*) ',i3,' XR(*) ',e12.5)
        endif
        if (iterm .gt. 1) then
           write (*,22) size, numslk, xbasis(size), ybasis(size),
     1     xr(size)
        endif
 
        if (istate.eq.4) go to 80
 
   30   call seekx
 
        i = 0
        do 32 j = 1,n
        if (piv(j) .ne. 0.0) then
           i = i + 1
           ipivs(i) = j
           pivs(i) = piv(j)
        endif
   32   continue
 
        if (iopton(15) .gt. 1) then
           write (dbug,34) newx, r, yaminc, (ipivs(j),pivs(j),j=1,i)
   34      format (t10,'*** SEEKX  *** NEWX ',i3,' R ',e12.5,' YAMINC ',
     1        e12.5 / (t25,' PIV* ',5(i3,1x,e12.5)))
        endif
        if (iterm .gt. 1) then
           write (*,34) newx, r, yaminc, (ipivs(j),pivs(j),j=1,i)
        endif
 
        if (newx.ne.0) go to 50
        istate = 2
        go to 80

   40   call isopt
        if (iopton(15) .gt. 1) then
           write (dbug,42) newx, yaminc
   42      format (t10,'*** ISOPT  *** NEWX ',i3,' YAMINC ',e12.5)
        endif
        if (iterm .gt. 1) then
           write (*,42) newx, yaminc
        endif
 
        if (newx.ne.0) go to 50
        istate = 1
        go to 80

   50   call newvec
        i = 0
        do 52 j = 1,n
        if (gr(j) .ne. 0.0) then
           i = i + 1
           ipivs(i) = j
           pivs(i) = gr(j)
        endif
   52   continue
 
        if (iopton(15) .gt. 1) then
           write (dbug,54) (ipivs(j),pivs(j),j=1,i)
   54      format (t10,'*** NEWVEC *** GR(*) ',5(i3,1x,e12.5) /
     1         (t25,' GR(*) ',5(i3,1x,e12.5)))
        endif
        if (iterm .gt. 1) then
           write (*,54) (ipivs(j),pivs(j),j=1,i)
        endif
 
        call seeky
 
        i = 0
        do 56 j = 1,n
        if (gr(j) .ne. 0.0) then
           i = i + 1
           ipivs(i) = j
           pivs(i) = gr(j)
        endif
   56   continue
 
        if (iopton(15) .gt. 1) then
           write (dbug,58) newy, r, (ipivs(j),pivs(j),j=1,i)
   58      format (t10,'*** SEEKY  *** NEWY ',i3,' R ',e12.5 /
     1         (t25,' GR(*) ',5(i3,1x,e12.5)))
        endif
        if (iterm .gt. 1) then
           write (*,58) newy, r, (ipivs(j),pivs(j),j=1,i)
        endif
 
        if (newy.ne.0) go to 60
        istate = 3
        go to 80
 
   60   if (newy.le.size) go to 70
        call addcon
 
        if (iopton(15) .gt. 1) then
           write (dbug,22) size, numslk, xbasis(size), ybasis(size),
     1        xr(size)
        endif
        if (iterm .gt. 1) then
           write (*,22) size, numslk, xbasis(size), ybasis(size),
     1     xr(size)
        endif
 
        if (istate.eq.4) go to 80
 
   70   call chbsis
 
        if (iopton(15) .gt. 1) then
           write (dbug,72) numslk, newx, newy, neginv, driver, obj
   72      format (t10,'*** CHBSIS *** NUMSLK ',i3,' NEWX ',i3,
     1        ' NEWY ',i3,' NEGINV ',i3,' DRIVER ',e12.5,' OBJ ',
     2        e12.5)
        endif
        if (iterm .gt. 1) then
           write (*,72) numslk, newx, newy, neginv, driver, obj
        endif
 
        if (iopton(15) .gt. 0) call chkinv
 
        call reduce
 
        if (iopton(15) .gt. 1) then
           write (dbug,74) numslk, neginv, size, marki, markk
   74      format (t10,'*** REDUCE *** NUMSLK ',i3,' NEGINV ',i3,
     1        ' SIZE ',i3,' MARKI ',i3,' MARKK ',i3)
        endif
        if (iterm .gt. 1) then
           write (*,74) numslk, neginv, size, marki, markk
        endif
 
        if (iopton(15) .gt. 0) call chkinv
        go to 10

   80   return
        end
