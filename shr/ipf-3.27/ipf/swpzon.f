C    @(#)swpzon.f	20.4 11/11/97
      subroutine swpzon(m,n)
c
      include 'ipfinc/parametr.inc'
      include 'ipfinc/arcntl.inc'
C
      character ztemp*2

        ztemp = acznam(m)
        acznam(m) = acznam(n)
        acznam(n) = ztemp

        itemp = acznum(m)
        acznum(m) = acznum(n)
        acznum(n) = itemp

        itemp = zone_number(m)
        zone_number(m) = zone_number(n)
        zone_number(n) = itemp

        return
        end
