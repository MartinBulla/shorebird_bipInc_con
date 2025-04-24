let's use our model m_1 (previous mbi5zp) and animate relationship for the fixed effects across 0-1 of prop_ip

mbi5 = lmer(bout_f~bout_m*bout_pol1+bout_m*bout_pol2+(1|genus) + (1|species) + (bout_m*bout_pol1+bout_m*bout_pol2|lat_pop), data = u, 
control = lmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5)))

mbi5zp = lmer(scale(bout_f)~scale(bout_m)*bout_pol1+scale(bout_m)*bout_pol2+(1|genus) + (1|species) + (scale(bout_m)*bout_pol1+scale(bout_m)*bout_pol2|lat_pop), data = u, 
control = lmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5)))

mbi5 = lmer(scale(bout_f)~scale(bout_m)*bout_pol1+scale(bout_m)*bout_pol2+(1|genus) + (1|species) + (scale(bout_m)*bout_pol1+scale(bout_m)*bout_pol2|lat_pop), data = u, 
control = lmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5)))
