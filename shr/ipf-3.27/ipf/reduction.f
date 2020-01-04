C    @(#)reduction.f	20.3 2/13/96
      subroutine  reduction ( a, b, n, m, ierr, iprout, kunit6 )
      implicit real*8 (a-h, o-z),  integer*4 (i-n)
      dimension  a(1), b(1)

      include 'ipfinc/prt.inc'

      unity = 1.d0 
      epsiln = 1.d-8

      j = n + 1
      w = unity
      if ( m .gt. 0 ) w = -w
      ij = n * j / 2
      if ( iprout .ge. 4 )
     1 write (kunit6, 2692)  n, m, ij,  ( a(k), k=1, ij )
 2692 format ( ' Top  "REDUCTION".  N, M, IJ =',  3i6,
     1         '     A(1:IJ) follow ....'  ,/,  ( 1x,  5e25.16 )  )
 4233 j = j - 1
      if ( j .eq. m ) go to 9800
      h1 = a(ij)
      if ( dabs ( h1 ) .gt. epsiln ) go to 4238
      
      ierr = 1
      write (errbuf(1), 111) j
  111 format ('Singularity encountered on row ', i4, ' during Gaussian e
     1limination.')
      write (errbuf(2), 112) n, m
  112 format ('Other relevant local variables N: ', i4, ' and M: ',i4)
      write (errbuf(3), 113) '111', 'REDUCTION'
  113 format ('Error called from statemnt ', a, ' in subroutine ', a)
      call prterx('W', 3)      
      go to 9800
 4238 h1 = -unity / h1
      b(j) = h1
      ij = ij - j
      k = 0
      ik = 0
 4244 ik = ik + k
      i1 = ik + 1
      k = k + 1
      if ( k .gt. n )    go to 4233
      if ( k .lt. j ) go to 4299
      if ( w .lt. 0.0 )  go to 4233
      if ( k .eq. j ) go to 4277
      i = ik + j
 4255 h2 = a(i)
      b(k) = h2 * h1
      i2 = ik + k
      l = 0
      do 4266  i=i1, i2
      l = l + 1
 4266 a(i) = a(i) + b(l) * h2
      if ( k .lt. j )  go to 4244
      i = ik + j
      a(i) = b(k)
      go to 4244
 4277 i = ij
      do 4288  l=1, j
      i = i + 1
 4288 a(i) = b(l)
      go to 4244
 4299 i = ij + k
      go to 4255
 9800 return
      end

