C    @(#)cxredu.f	20.3 2/13/96
      subroutine cxredu ( a, c, b, d, n, m )
      implicit real * 8 (a-h, o-z), integer * 4 (i-n)
      dimension a(1), c(1), b(1), d(1)
      j = n + 1
      w = 1.0
      if ( m .gt. 0 ) w = -w
      ij = n * j / 2
  100 j = j - 1
      if ( j .eq. m ) go to 170
      h1 = a(ij)
      g1 = c(ij)
      x = 1.0 / ( h1 * h1 + g1 * g1 )
      h1 = -h1 * x
      g1 = g1 * x
      b(j) = h1
      d(j) = g1
      ij = ij - j
      k = 0
      ik = 0
  110 ik = ik + k
      i1 = ik + 1
      k = k + 1
      if ( k .gt. n ) go to 100
      if ( k .lt. j ) go to 160
      if ( w .lt. 0.0 ) go to 100
      if ( k .eq. j ) go to 140
      i = ik + j
  120 h2 = a(i)
      g2 = c(i)
      b(k) = h2 * h1 - g2 * g1
      d(k) = h2 * g1 + g2 * h1
      i2 = ik + k
      l = 0
      do 130 i = i1, i2
      l = l + 1
      x = b(l)
      y = d(l)
      a(i) = a(i) + x * h2 - y * g2
  130 c(i) = c(i) + x * g2 + y * h2
      if ( k .lt. j ) go to 110
      i = ik + j
      a(i) = b(k)
      c(i) = d(k)
      go to 110
  140 i = ij
      do 150 l = 1, j
      i = i + 1
      c(i) = d(l)
  150 a(i) = b(l)
      go to 110
  160 i = ij + k
      go to 120
  170 return
      end
 
