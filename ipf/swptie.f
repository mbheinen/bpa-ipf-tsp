C    @(#)swptie.f	20.5 10/13/99
        subroutine swptie (m,n)
C
C       TIESRT IS SORTED WITH THE FOLLOWING FIELDS:
C
C       1. AREA-1
C       2. AREA-2
C       3. BUS-1
C       4. BUS-2
C
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'
 
        integer MAXTIESORT
        parameter (MAXTIESORT = 200)
        common /tiesrt/ tiesrt(MAXTIESORT)
        integer tiesrt

        i = tiesrt(m)
        tiesrt(m) = tiesrt(n)
        tiesrt(n) = i
        return
        end
