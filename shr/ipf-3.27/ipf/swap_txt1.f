C    @(#)swap_txt1.f	20.3 2/13/96
        subroutine swap_txt1 (ix, jx) 
        implicit none
        integer ix, jx
c
c       This function sorts text1 by blocks and by network
c       data: 
c
c       "A", "I", and "B+XQR$LET"
c
        common /difference/ numtxt1, array1, text1, numtxt2, array2, 
     &                      text2, numchg, change

        integer MAXTEXT, MAXCHG
        parameter (MAXTEXT = 10000)
        parameter (MAXCHG = 4000)

        character text1(MAXTEXT)*120, text2(MAXTEXT)*120,
     &            change(MAXCHG)*120
        integer array1(MAXTEXT), numtxt1, array2(MAXTEXT), numtxt2, 
     &          numchg

        integer temp

        temp = array1(ix)
        array1(ix) = array1(jx)
        array1(jx) = temp

        return
        end
