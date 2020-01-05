C    @(#)init_bd_all.f	20.3 2/13/96

        subroutine init_bd_all

        call init_bd0001
        call init_bd0002
        call init_bd0003
        call init_bd0004
        call init_bd0005
        call init_bd0006
        call init_bd0007
        call init_bd0008
        call init_bd0009
        call init_bd0011
        call init_bd0012
        call init_bd0013

        call init_blkdta
        call init_bdpfdt
        call init_bdtrdb
        call init_bdloderr

c        external bd0001
c        external bd0002
c        external bd0003
c        external bd0004
c        external bd0005
c        external bd0006
c        external bd0007
c        external bd0008
c        external bd0009
c        external bd0011
c        external bd0012
c        external bd0013
c        external bd0014
c        external blkdta
c        external bdpfdt
c        external bdtrdb
c        external bdloderr

        return
        end

