C    @(#)wrdac.f	20.3 2/13/96
        subroutine wrdac (iu,c,n)
C
        dimension c(n)
        character c *(*)
 
        write(iu) c
        return
        end
