C    @(#)buf_load_alt.f	20.3 2/13/96

        subroutine buf_load_alt( text )
        character * (*)  text

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'

        save

        buf = text

        return
        end
