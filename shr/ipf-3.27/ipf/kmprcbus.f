C    @(#)kmprcbus.f	20.3 2/13/96
        integer function kmprcbus (i,j)

        common /scratch/ ncbs, array(100), array_c(100)
        character * 8 array_c
        integer array

        kmprcbus = kompr (array_c(i), array_c(j), kmprcbus)
        return
        end
