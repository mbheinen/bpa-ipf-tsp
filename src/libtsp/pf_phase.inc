C phase.inc
C
       common /phase/ pf_jphid(8,PF_MAXPHS)
       dimension pf_phid(8,PF_MAXPHS)
       equivalence (pf_phid,pf_jphid)
       integer pf_jphid
       real pf_phid
C
