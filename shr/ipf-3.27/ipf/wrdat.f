C    @(#)wrdat.f	20.3 2/13/96
        subroutine wrdat (iu,a,n)
 
        dimension a(n)
C
        write(iu) a
        return
        end
