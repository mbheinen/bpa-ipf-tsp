C    @(#)pofrpt.f	20.3 2/13/96
      subroutine pofrpt (paprfl,fichfl,zonecd,ownercd)
      character paprfl * 1, fichfl * 1
      character ownercd* 3, zonecd * 2

c     Check the zone and ownership codes against the print and fiche
c     options and set the paper_file_list and fiche_file_list switches

      include 'ipfinc/zonlst.inc'

c     Local declarations
c
      character ownfl * 1, zonfl * 1

      character csrchn * 1
      external  csrchn

c
c     Test eligibility for paper report
c        If no paper_owner_restriction and no paper_zone_restriction
c           then set paper_flag on
c           else 
c              if tfrmr_side1_owner is in paper_owner_select_list
c                 then Set paper_owner_flag on;
c                 else Set paper_owner_flag off
c
c              if tfrmr_side1_zone is in paper_zone_select_list
c                 then set paper_zone_flag on;
c                 else Set paper_zone_flag off
c
c              if tfrmr is excluded by either list,
c                 then set paper_flag off;
c                 else Set paper_flag on
c
c              ( paper_flag is the union of the zone_select_list
c                and the owner_select_list )

      if (npzanl .eq. 0 .and. npoanl .eq. 0) then
         paprfl = 'Y'
c
      else
         if (npoanl .le. 0) then
            ownfl = 'Y'
         else
            ownfl = csrchn (ownercd,poalst,3,npoanl)
         endif
c
         if (npzanl .le. 0) then
            zonfl = 'Y'
         else
            zonfl = csrchn (zonecd,pzalst,2,npzanl)
         endif
c
         if (ownfl .eq. 'N' .or. zonfl .eq. 'N') then
            paprfl = 'N'
         else
            paprfl = 'Y'
         endif
      endif
c
c     Done testing for paper output report
c
c     Test elibibility for mfiche report.
c      ( same logic as for paper list above, except using the
c        fiche_owner_restrictions and fiche_zone_restrictiions )
c
      if (nfzanl .eq. 0 .and. nfoanl .eq. 0) then
         fichfl = 'Y'
c
      else
         if (nfoanl .le. 0) then
            ownfl = 'Y'
         else
            ownfl = csrchn (ownercd,foalst,3,nfoanl)
         endif
c
         if (nfzanl .le. 0) then
            zonfl = 'Y'
         else
            zonfl = csrchn (zonecd,fzalst,2,nfzanl)
         endif
c
         if (ownfl .eq. 'N' .or. zonfl .eq. 'N') then
            fichfl = 'N'
         else
            fichfl = 'Y'
         endif
      endif
c
c              Done testing for mfiche output
c
      return
      end
