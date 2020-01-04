C    @(#)bd0012.f	20.3 2/13/96
        subroutine init_bd0012
 
        include 'ipfinc/com012.inc'

        lmrg = 11
        mrgdic(1)  = 'INTERFACEP'
        mrgdic(2)  = 'SAVEAR*'
        mrgdic(3)  = 'SAVEZO*'
        mrgdic(4)  = 'SAVEBA*'
        mrgdic(5)  = 'SAVEBU*'
        mrgdic(6)  = 'INCLUDEBU*'
        mrgdic(7)  = 'EXCLUDEBU*'
        mrgdic(8)  = 'INTERFACEB'
        mrgdic(9)  = 'RENAMEBU*'
        mrgdic(10) = 'EXCLUDEBR*'
        mrgdic(11) = 'USEAIC'

        return
        end

c        BLOCK DATA BD0012
 
c        include 'ipfinc:com012.inc'

c        DATA LMRG/ 11 /, MRGDIC/ 'INTERFACEP', 'SAVEAR*','SAVEZO*',
c     1   'SAVEBA*', 'SAVEBU*', 'INCLUDEBU*', 'EXCLUDEBU*',
c     2   'INTERFACEB', 'RENAMEBU*', 'EXCLUDEBR*', 'USEAIC' /

c        END
