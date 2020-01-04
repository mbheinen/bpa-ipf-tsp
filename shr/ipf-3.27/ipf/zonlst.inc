
        common /zonlst/ nfzdta, npzdta, nfzout, npzout, nfzanl, npzanl,
     1                  nfoanl, npoanl, leffan, txefan, npaanl, nfaanl,
     2                  aplist(23), aflist(23), apsort(23),apsize(23)
 
        parameter   ( MXALST = 50  )     !   zones in anlysis listing
        parameter   ( MXPLST = 50  )     !   zones in input/output listing
        parameter   ( MXFLST = 50  )     !   zones in microfiche output
        parameter   ( MXOLST = 50  )     !   ownerships in anlysis listing

        common /zonlsc/ fzdlst(MXFLST),pzdlst(MXPLST),fzolst(MXFLST),
     1                  pzolst(MXPLST), fzalst(MXALST), pzalst(MXALST),
     2                  foalst (MXOLST), poalst (MXOLST),
     3                  effan(MXOLST), xefan(MXOLST), faalst(MXALST),
     4                  paalst(MXALST), apfile(23)
C
        character fzdlst*2, pzdlst*2, fzolst*2, pzolst*2, fzalst*2,
     1            pzalst*2, foalst*3, poalst*3, effan*3, xefan*3,
     2            faalst*10, paalst*10, apfile*30
        integer txefan, aplist, aflist, apsort, apsize
 
