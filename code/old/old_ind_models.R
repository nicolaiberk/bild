
# old individual models

# #############################################################################
# ###################### OLD BELOW HERE #######################################
# #############################################################################
# 
# ## unweighted independent
# imm_mod_unw_all_nosal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1130_clean` ~ 
#        `1130_lag` + 
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# mod_tbl <- summary(imm_mod_unw_all_nosal) 
# 
# 
# longnames <- c('(Intercept)', 'Lagged DV', 'Petty Crime', 'Capital Crime', 'Refugee Numbers', 'Camps', 'Mediterranean Rescue', 'Labour Market', 'Deportations')
# arm::coefplot(imm_mod_unw_all_nosal, longnames) %>% 
#   ggsave(filename = here('paper/vis/individual_effects_immigration.png'),
#                          width = 8, height = 6)
# 
# 
# # integration
# int_mod_unw_all_nosal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1210_clean` ~ 
#        `1210_lag` + 
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# summary(int_mod_unw_all_nosal) # capcrime negative (expected), deport positive (also expected), but surprisingly labmar no effect
# 
# stargazer(imm_mod_unw_all_nosal, 
#           int_mod_unw_all_nosal,
#           title = 'Individual-level Model', 
#           out = here('paper/imm_unw.tex'))
# 
# arm::coefplot(int_mod_unw_all_nosal) %>% 
#   ggsave(filename = here('paper/vis/individual_effects_immigration.png'),
#          width = 8, height = 6)
# 
# 
# ## holds controlling for salience:
# imm_mod_unw_all_sal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1130_clean` ~ 
#        `1130_lag` + 
#        n_mig_tot_7d_unw +
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# summary(imm_mod_unw_all_sal)
# arm::coefplot(imm_mod_unw_all_sal)
# 
# 
# int_mod_unw_all_sal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1210_clean` ~ 
#        `1210_lag` + 
#        n_mig_tot_7d_unw +
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# summary(int_mod_unw_all_sal) # capcrime negative (expected), labmar positive (unclear)
# arm::coefplot(int_mod_unw_all_sal)
# 
# 
# ## interact salience with share
# 
# imm_mod_inter <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1130_clean` ~ 
#        `1130_lag` + 
#        n_mig_tot_7d_unw +
#        n_mig_tot_7d_unw * crime_share_7d_unw + 
#        n_mig_tot_7d_unw * capcrime_share_7d_unw + 
#        n_mig_tot_7d_unw * refnums_share_7d_unw + 
#        n_mig_tot_7d_unw * camps_share_7d_unw + 
#        n_mig_tot_7d_unw * medit_share_7d_unw + 
#        n_mig_tot_7d_unw * labmar_share_7d_unw + 
#        n_mig_tot_7d_unw * deport_share_7d_unw)
# 
# summary(imm_mod_inter) # refnums, medit, camps pos, salience only slightly but interaction hard to interpret (not sure if useful but might plot marginal effects)
# 
# 
# int_mod_inter <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1210_clean` ~ 
#        `1210_lag` + 
#        n_mig_tot_7d_unw +
#        n_mig_tot_7d_unw * crime_share_7d_unw + 
#        n_mig_tot_7d_unw * capcrime_share_7d_unw + 
#        n_mig_tot_7d_unw * refnums_share_7d_unw + 
#        n_mig_tot_7d_unw * camps_share_7d_unw + 
#        n_mig_tot_7d_unw * medit_share_7d_unw + 
#        n_mig_tot_7d_unw * labmar_share_7d_unw + 
#        n_mig_tot_7d_unw * deport_share_7d_unw)
# 
# summary(int_mod_inter) # no effect salience, 
# 
# 
# ## weighted media consumption estimates
# imm_mod_wtd_all <- 
#   lm(`1130_clean` ~ 
#      `1130_lag` + 
#        # n_mig_tot_7d + 
#        crime_tot_7d + 
#        capcrime_tot_7d + 
#        refnums_tot_7d + 
#        camps_tot_7d + 
#        medit_tot_7d + 
#        labmar_tot_7d + 
#        deport_tot_7d,
#      data = merged_data)
# 
# summary(imm_mod_wtd_all) # only effect camps - but actually MORE restrictive!
# 
# 
# int_mod_wtd_all <- 
#   lm(`1210_clean` ~ 
#        `1210_lag` + 
#        # n_mig_tot_7d +
#        crime_tot_7d + 
#        capcrime_tot_7d + 
#        refnums_tot_7d + 
#        camps_tot_7d + 
#        medit_tot_7d + 
#        labmar_tot_7d + 
#        deport_tot_7d,
#      data = merged_data)
# 
# summary(int_mod_wtd_all) # no effects
# 
# 
# # control for paper readership
# imm_mod_unw_paperctrl <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1130_clean` ~ 
#        `1130_lag` + 
#        `1661a_bin` +
#        `1661c_bin` +
#        `1661d_bin` +
#        `1661e_bin` +
#        `1661f_bin` +
#        scale(crime_tot_7d_unw,    center = T, scale = T) + 
#        scale(capcrime_tot_7d_unw, center = T, scale = T) + 
#        scale(refnums_tot_7d_unw,  center = T, scale = T) + 
#        scale(camps_tot_7d_unw,    center = T, scale = T) + 
#        scale(medit_tot_7d_unw,    center = T, scale = T) + 
#        scale(labmar_tot_7d_unw,   center = T, scale = T) + 
#        scale(deport_tot_7d_unw,   center = T, scale = T))
# 
# summary(imm_mod_unw_paperctrl)
# 
# 
# ## 3.5 explore individual-level moderators ####
# 
# 
# ### 3.5.1 existing migration opinion ####
# 
# ## immigration
# imm_mod_inter_opinion <- 
#   merged_data %>% 
#   mutate(
#     imm_op = `1130_clean`,
#     imm_op_lag = `1130_lag`
#   ) %>% 
#   lm(data = .,
#      formula = 
#        imm_op ~ 
#        imm_op_lag + 
#        crime_tot_7d_unw * imm_op_lag + 
#        capcrime_tot_7d_unw * imm_op_lag + 
#        refnums_tot_7d_unw * imm_op_lag + 
#        camps_tot_7d_unw * imm_op_lag + 
#        medit_tot_7d_unw * imm_op_lag + 
#        labmar_tot_7d_unw * imm_op_lag + 
#        deport_tot_7d_unw * imm_op_lag )
# 
# plot_model(imm_mod_inter_opinion) # camps, medit, labmar more restrictive, capcrime, refnums less - no effing sense. But capcrime most polarising, labmar de-polarising
# 
# ### marginal effect plot
# interplot(imm_mod_inter_opinion, var1 = 'crime_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # negative effect among pro-imm, positive effect among anti-imm (anti-polarisation?)
# interplot(imm_mod_inter_opinion, var1 = 'capcrime_tot_7d_unw', var2 = 'imm_op_lag', hist = T) # capcrime makes those with less restrictive attitudes even less restrictive (and vice versa, as expected for more restrictive attitudes) - Polarisation&Reaktanz?
# interplot(imm_mod_inter_opinion, var1 = 'refnums_tot_7d_unw',  var2 = 'imm_op_lag', hist = T) # always positive, less positive with more negative attitudes
# interplot(imm_mod_inter_opinion, var1 = 'camps_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # always negative, but less so among more restrictive resps
# interplot(imm_mod_inter_opinion, var1 = 'medit_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # always negative
# interplot(imm_mod_inter_opinion, var1 = 'labmar_tot_7d_unw',   var2 = 'imm_op_lag', hist = T) # strongly anti-polarising
# interplot(imm_mod_inter_opinion, var1 = 'deport_tot_7d_unw',   var2 = 'imm_op_lag', hist = T) # strongly anti-polarising
# 
# 
# 
# ## integration
# 
# int_mod_inter_opinion <- 
#   merged_data %>%   
#   mutate(
#     int_op = `1210_clean`,
#     int_op_lag = `1210_lag`,
#   ) %>% 
#   lm(data = .,
#      formula = 
#        int_op ~ 
#        int_op_lag + 
#        crime_tot_7d_unw * int_op_lag + 
#        capcrime_tot_7d_unw * int_op_lag + 
#        refnums_tot_7d_unw * int_op_lag + 
#        camps_tot_7d_unw * int_op_lag + 
#        medit_tot_7d_unw * int_op_lag + 
#        labmar_tot_7d_unw * int_op_lag + 
#        deport_tot_7d_unw * int_op_lag )
# 
# plot_model(int_mod_inter_opinion) # capcrime more restrictive, camps more lenient, capcrime polarising, camps de-polarising
# 
# interplot(int_mod_inter_opinion, var1 = 'crime_tot_7d_unw',    var2 = 'int_op_lag', hist = T) #  not significant
# interplot(int_mod_inter_opinion, var1 = 'capcrime_tot_7d_unw', var2 = 'int_op_lag', hist = T) # strongly polarising
# interplot(int_mod_inter_opinion, var1 = 'refnums_tot_7d_unw',  var2 = 'int_op_lag', hist = T) # polarising
# interplot(int_mod_inter_opinion, var1 = 'camps_tot_7d_unw',    var2 = 'int_op_lag', hist = T) # de-polarising
# interplot(int_mod_inter_opinion, var1 = 'medit_tot_7d_unw',    var2 = 'int_op_lag', hist = T) # not significant
# interplot(int_mod_inter_opinion, var1 = 'labmar_tot_7d_unw',   var2 = 'int_op_lag', hist = T) # not significant
# interplot(int_mod_inter_opinion, var1 = 'deport_tot_7d_unw',   var2 = 'int_op_lag', hist = T) # strongly de-polarising
#   
# 
# 
# ### 3.5.2 existing issue importance ####
# 
# ## immigration
# imm_mod_inter_imp <- 
#   merged_data %>% 
#   mutate(
#     imm_op = `1130_clean`,
#     imm_op_lag = `1130_lag`,
#     imm_imp_lag = `1140_lag`
#   ) %>% 
#   lm(data = .,
#      formula = 
#        imm_op ~ 
#        imm_op_lag + 
#        imm_imp_lag + 
#        crime_tot_7d_unw * imm_imp_lag + 
#        capcrime_tot_7d_unw * imm_imp_lag + 
#        refnums_tot_7d_unw * imm_imp_lag + 
#        camps_tot_7d_unw * imm_imp_lag + 
#        medit_tot_7d_unw * imm_imp_lag + 
#        labmar_tot_7d_unw * imm_imp_lag + 
#        deport_tot_7d_unw * imm_imp_lag )
# 
# plot_model(imm_mod_inter_imp) # only labmar decreases restrictiveness and depolarises, camps increase restrictiveness
# 
# ### marginal effect plot
# interplot(imm_mod_inter_imp, var1 = 'crime_tot_7d_unw',    var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'capcrime_tot_7d_unw', var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'refnums_tot_7d_unw',  var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'camps_tot_7d_unw',    var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'medit_tot_7d_unw',    var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'labmar_tot_7d_unw',   var2 = 'imm_imp_lag')
# interplot(imm_mod_inter_imp, var1 = 'deport_tot_7d_unw',   var2 = 'imm_imp_lag')
# 
# 
# ## integration
# 
# int_mod_inter_imp <- 
#   merged_data %>%   
#   mutate(
#     int_op = `1210_clean`,
#     int_op_lag = `1210_lag`,
#     int_imp_lag = `1220_lag`
#   ) %>% 
# lm(data = .,
#    formula = 
#      int_op ~ 
#      int_op_lag + 
#      int_imp_lag + 
#      int_imp_lag *  crime_tot_7d_unw    + 
#      int_imp_lag * capcrime_tot_7d_unw + 
#      int_imp_lag * refnums_tot_7d_unw  + 
#      int_imp_lag *  camps_tot_7d_unw    + 
#      int_imp_lag *  medit_tot_7d_unw    + 
#      int_imp_lag *  labmar_tot_7d_unw   + 
#      int_imp_lag *  deport_tot_7d_unw   )
# 
# plot_model(int_mod_inter_imp) # capcrime, refnums, camps more restrictive, no interaction effects
# 
# interplot(int_mod_inter_imp, var1 = 'crime_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'capcrime_tot_7d_unw', var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'refnums_tot_7d_unw',  var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'camps_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'medit_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'labmar_tot_7d_unw',   var2 = 'int_imp_lag', hist = T)
# interplot(int_mod_inter_imp, var1 = 'deport_tot_7d_unw',   var2 = 'int_imp_lag', hist = T)
# 
# 
# 
# 
# # 3.5.3 party vote (190b) ####
# 
# merged_data$`190b_clean` <- relevel(as.factor(merged_data$`190b_clean`), ref = 'CDU/CSU')  
# 
# ## immigration
# imm_mod_inter_party <- 
#   merged_data %>% 
#   mutate(
#     imm_op = `1130_clean`,
#     party = `190b_clean`,
#     imm_op_lag = `1130_lag`
#   ) %>% 
#   filter(party != '') %>% 
#   lm(data = .,
#      formula = 
#        imm_op ~ 
#        imm_op_lag + 
#        party + 
#        crime_tot_7d_unw * party + 
#        capcrime_tot_7d_unw * party + 
#        refnums_tot_7d_unw * party + 
#        camps_tot_7d_unw * party + 
#        medit_tot_7d_unw * party + 
#        labmar_tot_7d_unw * party + 
#        deport_tot_7d_unw * party )
# 
# plot_model(imm_mod_inter_party, group.terms = c(1, 2:6, rep(7, 7), rep(2:6, 7))) # 
# 
# ### marginal effect plot
# interplot(imm_mod_inter_party, var1 = 'crime_tot_7d_unw',    var2 = 'party')
# interplot(imm_mod_inter_party, var1 = 'capcrime_tot_7d_unw', var2 = 'party') # baseline effect (CDU/CSU) positive(!), but becomes insignificant/neg for all other parties
# interplot(imm_mod_inter_party, var1 = 'refnums_tot_7d_unw',  var2 = 'party') # leads to more positive att among left
# interplot(imm_mod_inter_party, var1 = 'camps_tot_7d_unw',    var2 = 'party') # neg effect disappears for SPD and AfD, increases for Greens
# interplot(imm_mod_inter_party, var1 = 'medit_tot_7d_unw',    var2 = 'party') # becomes negative among left ???
# interplot(imm_mod_inter_party, var1 = 'labmar_tot_7d_unw',   var2 = 'party') # neg effect disappears for SPD & AfD, increases for Left
# interplot(imm_mod_inter_party, var1 = 'deport_tot_7d_unw',   var2 = 'party') # neg effect among CDU/CSU disappears for all, even pos among greens
# 
# 
# ## integration
# 
# int_mod_inter_party <- 
#   merged_data %>%   
#   mutate(
#     int_op = `1210_clean`,
#     party = `190b_clean`,
#     int_op_lag = `1210_lag`,
#   ) %>% 
#   lm(data = .,
#      formula = 
#        int_op ~ 
#        int_op_lag +
#        party + 
#        party *  crime_tot_7d_unw    + 
#        party * capcrime_tot_7d_unw + 
#        party * refnums_tot_7d_unw  + 
#        party *  camps_tot_7d_unw    + 
#        party *  medit_tot_7d_unw    + 
#        party *  labmar_tot_7d_unw   + 
#        party *  deport_tot_7d_unw   )
# 
# plot_model(imm_mod_inter_party, group.terms = c(1, 2:6, rep(7, 7), rep(2:6, 7))) # 
# 
# ### marginal effect plot
# interplot(imm_mod_inter_party, var1 = 'crime_tot_7d_unw',    var2 = 'party') # no effect
# interplot(imm_mod_inter_party, var1 = 'capcrime_tot_7d_unw', var2 = 'party') # neg for CDU/CSU, insig for others
# interplot(imm_mod_inter_party, var1 = 'refnums_tot_7d_unw',  var2 = 'party') # neg for Left, no eff for all others
# interplot(imm_mod_inter_party, var1 = 'camps_tot_7d_unw',    var2 = 'party') # pos for CDU/CSU, FDP, greens, Left
# interplot(imm_mod_inter_party, var1 = 'medit_tot_7d_unw',    var2 = 'party') # pos for all, esp. left
# interplot(imm_mod_inter_party, var1 = 'labmar_tot_7d_unw',   var2 = 'party') # pos for all except AfD & SPD
# interplot(imm_mod_inter_party, var1 = 'deport_tot_7d_unw',   var2 = 'party') # pos for CDU, neg for Greens
# 
# 
# # 3.5.4 ideology (1500) ####
# 
# # 3.5.5 strength of pre-existing att on migration ####
# 
# # 3.5.6 number of papers read/diversity of media diet ####
# 
# # 3.5.7 political knowledge ####
# 
# 
# 
# 
# 
# 
# 
# # 3.6 effects on issue importance ####
# 
# 
# ## unweighted independent
# imm_imp_unw_all_nosal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1140_clean` ~ 
#        `1140_lag` + 
#        n_mig_tot_7d_unw +
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# summary(imm_imp_unw_all_nosal) # not significant
# 
# 
# arm::coefplot(imm_imp_unw_all_nosal)
# 
# 
# # integration
# int_imp_unw_all_nosal <- 
#   merged_data %>% 
#   lm(data = .,
#      formula = 
#        `1220_clean` ~ 
#        `1220_lag` + 
#        n_mig_tot_7d_unw +
#        crime_tot_7d_unw + 
#        capcrime_tot_7d_unw + 
#        refnums_tot_7d_unw + 
#        camps_tot_7d_unw + 
#        medit_tot_7d_unw + 
#        labmar_tot_7d_unw + 
#        deport_tot_7d_unw )
# 
# summary(int_imp_unw_all_nosal) # again only crime
# 
# arm::coefplot(int_imp_unw_all_nosal)
