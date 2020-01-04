C    @(#)rddat.f	20.3 2/13/96
        subroutine rddat(iu,a,n)
 
        dimension a(n)
C
        read(iu) a
        return
        end
