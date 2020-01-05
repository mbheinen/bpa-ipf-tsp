
        parameter   ( MAXCHG = 5000   )   ! changes permitted
        parameter   ( MAXADT = MAXCHG )   ! add/restores w/transposes

        common /chgcrd/ numchg, nadd, nres, ndel, nrename
        common /chgcrdc/ chgcrd(MAXCHG)
        character chgcrd*126

