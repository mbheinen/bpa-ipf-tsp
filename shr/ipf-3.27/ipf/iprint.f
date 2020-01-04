C    @(#)iprint.f	20.3 2/13/96
      subroutine iprint
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/aref.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/smallp.inc'
 
 
 8000 format ('0 THE SIGN(I) VECTOR INDICATES THE SIGN OF THE I-TH ',
     1        'CONSTRAINT, 0 FOR EQ, 1 FOR LE, -1 FOR GE.')
 9000 format ('1 NON-ZERO ELEMENTS OF THE A MATRIX, FOLLOWED BY ',
     1        'THEIR COLUMN LABELS....')
 9001 format (1h0,12(4x,i6))
 9002 format (1h ,12(1pe10.2))
 9003 format (1h ,12(4x,i6))
 9004 format ('0 THE FOLLOWING VECTORS SHOW THE STARTING POINTS OF ',
     1        'THE SUCCESSIVE ROWS OF A IN THE ABOVE LIST OF THE ',
     2        'NON-ZERO ELEMENTS...')
 9005 format (1h0,24(i5))
 9006 format (1h ,24(i5))
 9200 format (1h0,'OBJECTIVE ',1pe15.8)
 9204 format(1h0,38x,'J ',8(3x,i3,4x))
 9205 format(1h0,38x,'I ',8(3x,i3,4x))
 9208 format (1h0,31x,'C VECTOR ',8(1pe9.2,1x))
 9212 format (1h0,27x,'BOUND VECTOR ',8(1pe9.2,1x))
 9220 format (1h0,31x,'X VECTOR ',8(1pe9.2,1x))
 9228 format (1h0,34x,5hy'a-c,1x,8(1pe9.2,1x))
 9232 format (////)
 9234 format (1h0,12(6x,i3,1x))
 9236 format (1h0,12(1pe9.2,1x))
 9238 format (1h0,12(1pe9.2,1x))
 9240 format (1h0,12(1pe9.2,1x))
 9244 format (1h0,12(1pe9.2,1x))
 9300 format (1h0,31x,'B VECTOR ',8(1pe9.2,1x))
 9304 format (1h0,35x,'SIGN ',8(1pe9.2,1x))
 9308 format (1h0,31x,'Y VECTOR ',8(1pe9.2,1x))
 9312 format (1h0,34x,' B-AX ',8(1pe9.2,1x))
 9404 format (1h0,12x,'COLUMN ',7(6x,i2,6x))
 9408 format (1h0,12x,'YBASIS ',7(5x,i3,6x))
 9412 format (1h0,16x,'YR ',2x,7(1pe12.4,2x))
 9416 format (1h0,'ROW XBS ',4x,'XR',5x,'INVERSE MATRIX'/)
 9420 format (1x,i3,1x,i3,8(1pe12.4,2x))
 9424 format (1h1,5x,8(5x,i3,6x))
 9428 format (1h0,5x,8(5x,i3,6x))
 9432 format (1h0,5x,8(1pe12.4,2x))
 9436 format (1h0,'ROW',5x,'INVERSE MATRIX CONTINUES'/)
 9438 format (1h ,i3,2x,8(1pe12.4,2x))
 9500 format (1h1)
 9504 format ('0    BIG', 1pe12.4, ', DRIVER', e12.1, ',  INREV', i12,
     1        ',     IR', i12,     ',  IRMAX', i12,   ',  ISBND', i12
     2      / '  ISDONE', i12,     ', ISTATE', i12,   ',    ITR', i12,
     3        ', ITRMAX', i12,     ',      M', i12,   ',  MARKI', i12
     3      / '   MARKK', i12,     ',   MAXA', i12,   ',   MAXM', i12,
     4        ',   MAXN', i12,     ',   MORE', i12,   ', MXSIZE', i12
     5      / '       N', i12,     ', NEGINV', i12,   ', NEGROW', i12,
     6        ',   NEWX', i12,     ',   NEWY', i12,   ', NUMSLK', i12
     7      / '       R', e12.5,   ',   SIZE', i12,   ',  SMALL', e12.4,
     8        ', TOL(1)', e12.4,   ', TOL(2)', e12.4, ', TOL(3)', e12.4
     9      / '  TOL(4)', e12.4,   ', TOL(5)', e12.4, ', TOL(6)', e12.4,
     *        ', TOL(7)', e12.4,   ', TOL(8)', e12.4, ',  XKPOS', e12.1
     *      / '  YAMINC', e12.5)
 9516 format ('0ISEFF' / 1x, 40i3)
 9520 format ('0INBASE'/ 1x, 40i3)
 9600 format ('0',i5,' SIMPLEX ITERATIONS.')
 9604 format ('0(N.B., THE MAXIMUM SIZE OF THE INVERSE DURING ',
     1        'THE CALCULATION WAS ',i4,')')
 
      write (dbug,9000)
      last = irow(mnow+1) - 1
      istart = 1
  100 iend = istart + 11
      if (iend.gt.last) iend = last
      write (dbug,9001) (ij,ij = istart,iend)
      write (dbug,9002) (aa(ij),ij = istart,iend)
      write (dbug,9003) (jcol(ij),ij = istart,iend)
      if (iend.eq.last) go to 105
      istart = iend + 1
      go to 100

  105 write (dbug,9004)
      istart = 1
  110 iend = istart + 23
      if (iend.gt.mnow) iend = mnow
      write (dbug,9005) (i,i = istart,iend)
      write (dbug,9006) (irow(i),i = istart,iend)
      if (iend.eq.mnow) go to 200
      istart = iend + 1
      go to 110

  200 write (dbug,9200) obj
      iend = 8
      if (n.le.iend) iend = n
      write (dbug,9204) (j,j = 1,iend)
      write (dbug,9208) (c(j),j = 1,iend)
      if (isbnd.eq.0) go to 210
      write (dbug,9212) (bound(j),j = 1,iend)
  210 write (dbug,9220) (x(j),j = 1,iend)
      write (dbug,9228) (yac(j),j = 1,iend)
  230 if (n.le.iend) go to 300
      write (dbug,9232)
      istart = iend + 1
      iend = iend + 12
      if (n.le.iend) iend = n
      write (dbug,9234) (j,j = istart,iend)
      write (dbug,9236) (c(j),j = istart,iend)
      if (isbnd.eq.0) go to 235
      write (dbug,9240) (bound(j), j = istart,iend)
  235 write (dbug,9240) (x(j), j = istart,iend)
      write (dbug,9244) (yac(j), j = istart,iend)
      go to 230

  300 iend = 8
      if (mnow.le.iend) iend = mnow
      write (dbug,9232)
      write (dbug,8000)
      write (dbug,9205)(i,i=1,iend)
      write (dbug,9300) (b(i), i = 1,iend)
      write (dbug,9304) (s(i), i = 1,iend)
      write (dbug,9308) (y(i), i = 1,iend)
      write (dbug,9312) (slack(i), i = 1,iend)
  310 if (mnow.le.iend) go to 400
      write (dbug,9232)
      istart = iend + 1
      iend = iend + 12
      if (mnow.le.iend) iend = mnow
      write (dbug,9234) (i, i = istart,iend)
      write (dbug,9236) (b(i), i = istart,iend)
      write (dbug,9238) (s(i), i = istart,iend)
      write (dbug,9240) (y(i), i = istart,iend)
      write (dbug,9240) (slack(i), i = istart,iend)
      go to 310

  400 if (morepr.lt.1.or.morepr.gt.2) go to 600
      iend = 7
      if (size.le.iend) iend = size
      write (dbug,9404) (l, l = 1,iend)
  410 write (dbug,9408) (ybasis(l), l = 1,iend)
      write (dbug,9412) (yr(l), l = 1,iend)
      write (dbug,9416)
      do 430 k = 1,size
         write (dbug,9420) k,xbasis(k),xr(k), (inv(k,l),l = 1,iend)
  430 continue
  440 if (size.le.iend) go to 500
      istart = iend + 1
      iend = iend + 8
      if (size.le.iend) iend = size
      write (dbug,9424) (l, l = istart,iend)
      write (dbug,9428) (ybasis(l), l = istart,iend)
      write (dbug,9432) (yr(l), l = istart, iend)
      write (dbug,9436)
      do 445 k = 1,size
         write (dbug,9438) k, (inv(k,l), l = istart,iend)
  445 continue
      go to 440

  500 if (size.lt.9) go to 510
      write (dbug,9500)
  510 write (dbug,9232)
      write (dbug,9504) big,driver,inrev,ir,irmax,isbnd,isdone,istate,
     1  itr,itrmax,m,marki,markk,MAXA,MAXM,MAXN,more,MXSIZE,n,neginv,
     2  negrow,newx,newy,numslk,r,size,small,(tol(k),k=1,8),xkpos,yaminc

  600 write (dbug,9516) (iseff(i), i = 1,mnow)
      write (dbug,9520) (inbase(j), j = 1,n)
      write (dbug,9600) itr
      write (dbug,9604) isbig
      return
      end
