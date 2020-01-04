C    @(#)bd0011.f	20.4 2/13/96
        subroutine init_bd0011
 
        include 'ipfinc/com011.inc'

        lctl = 19
        dict(1)  = 'DEBUG'
        dict(2)  = 'ELI*'
        dict(3)  = 'EXCLUDEBUS'
        dict(4)  = 'INCLUDEBUS'
        dict(5)  = 'INCLUDECON'
        dict(6)  = 'KEEPAI*'
        dict(7)  = 'MINEQ*'
        dict(8)  = 'OPTIMALRED'
        dict(9)  = 'RETAINGEN*'
        dict(10) = 'REI*'
        dict(11) = 'SAVEBUS*'
        dict(12) = 'SAVEBASE*'
        dict(13) = 'SAVEZONE*'
        dict(14) = 'ULT*'
        dict(15) = 'COH*'
        dict(16) = 'SAVEARE*'
        dict(17) = 'ENVEL*'
        dict(18) = 'START*'
        dict(19) = 'CHANGE*'
 
        return
        end

c        block data bd0011
 
c        include 'ipfinc:com011.inc'

c        data lctl / 18 /
c     &       dict / 'DEBUG',        'ELI*',       'EXCLUDEBUS',
c     &              'INCLUDEBUS',   'INCLUDECON', 'KEEPAI*',
c     &              'MINEQ*',       'OPTIMALRED', 'RETAINGEN*',
c     &              'REI*',         'SAVEBUS*',   'SAVEBASE*',
c     &              'SAVEZONE*',    'ULT*',       'COH*',
c     &              'SAVEARE*',     'ENVEL*',     'START*' /
 
c        end
