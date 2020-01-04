C    @(#)bd0013.f	20.3 2/13/96
        subroutine init_bd0013
 
        include 'ipfinc/com013.inc'

        commnt(1)  = 'SELECT - ASSIGNED INTERFACE PREFERENCE  '
        commnt(2)  = 'SELECT - BUS-BRANCH OWNERSHIPS MATCH    '
        commnt(3)  = 'SELECT - SUBSYSTEM PREFERENCE           '
        commnt(4)  = 'SELECT - SOLITARY CANDIDATE             '
        commnt(5)  = 'SELECT -                                '
        commnt(6)  = 'REJECT - TERMINAL BUS IS NOT IN SYSTEM  '
        commnt(7)  = 'REJECT - NO ASSIGNED INTERFACE PREFRENCE'
        commnt(8)  = 'REJECT - BUS-BRANCH OWNERSHIPS DIFERENCE'
        commnt(9)  = 'REJECT - NOT IN PREFERRED SUBSYSTEM     '
        commnt(10) = 'REJECT - DUPLICATE BRANCH RECORD        '
        commnt(11) = 'REJECT -                                '
        commnt(12) = 'REJECT - TERMINAL BUS1 NOT IN SYSTEM    '
        commnt(13) = 'REJECT - TERMINAL BUS2 NOT IN SYSTEM    '
 
        return
        end

c        BLOCK DATA BD0013
 
c        include 'ipfinc:com013.inc'

c        DATA COMMNT /
c     1   'SELECT - ASSIGNED INTERFACE PREFERENCE  ',
c     2   'SELECT - BUS-BRANCH OWNERSHIPS MATCH    ',
c     3   'SELECT - SUBSYSTEM PREFERENCE           ',
c     4   'SELECT - SOLITARY CANDIDATE             ',
c     5   'SELECT -                                ',
c     6   'REJECT - TERMINAL BUS IS NOT IN SYSTEM  ',
c     7   'REJECT - NO ASSIGNED INTERFACE PREFRENCE',
c     8   'REJECT - BUS-BRANCH OWNERSHIPS DIFERENCE',
c     9   'REJECT - NOT IN PREFERRED SUBSYSTEM     ',
c     A   'REJECT - DUPLICATE BRANCH RECORD        ',
c     B   'REJECT -                                ',
c     C   'REJECT - TERMINAL BUS1 NOT IN SYSTEM    ',
c     D   'REJECT - TERMINAL BUS2 NOT IN SYSTEM    '/
 
c        END
