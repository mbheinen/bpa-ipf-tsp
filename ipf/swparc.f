C    @(#)swparc.f	20.4 11/11/97
      subroutine swparc (m,n)
c
c     This subroutine exchanges ARCNAM(M) with ARCNAM(N)
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
 
      character tempc * 10
 
      tempc = arcnam(m)
      arcnam(m) = arcnam(n)
      arcnam(n) = tempc
 
      temp = arcnet(m)
      arcnet(m) = arcnet(n)
      arcnet(n) = temp
 
      tempc = arcbus(m)
      arcbus(m) = arcbus(n)
      arcbus(n) = tempc
 
      temp = arcbas(m)
      arcbas(m) = arcbas(n)
      arcbas(n) = temp

      do 100 i = 1, MAXCAZ
         tempc = arczns(i,m)
         arczns(i,m) = arczns(i,n)
         arczns(i,n) = tempc
  100 continue
 
      itemp = area_number(m)
      area_number(m) = area_number(n)
      area_number(n) = itemp

      return
      end
