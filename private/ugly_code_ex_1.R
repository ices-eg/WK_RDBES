





matchs<-distinct(dat_7,match)
matchs_ran<-mutate(matchs[sample(1:nrow(matchs)),],ft_id=row.names(matchs))

dat_8=left_join(dat_7,matchs_ran)

hauls<-select(mutate(distinct(dat_8,match,ft_id,haul_id),fo_id=str_replace(haul_id,match,ft_id)),haul_id,fo_id)

dat_8<-left_join(dat_8,hauls)

dat_9<-select(dat_8,-match,-match_alle,-haul_id,-fopera_id)


dat_4<-select(dat_3,-fvd,-afrfvd,-square,-start_br_pos,-start_lg_pos,-slut_fngtid,-slut_br_pos,-slut_lg_pos,-dfadfvd,-dfadfvd_mrk,-square_ret,-zone_vms,-dfadfvd_ret,-fao_area,-zone)
