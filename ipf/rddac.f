C    @(#)rddac.f	20.3 2/13/96
        subroutine rddac(iu,c,n)
 
        dimension c(n)
        character c *(*)
C
        read(iu) c
        return
        end
