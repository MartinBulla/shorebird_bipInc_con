require(here)
source(here::here('R/tools.R')) 
female='#FCB42C' # 'deepskyblue'
male='#535F7C' # 'red' 

n=fread(here::here('Data/Bulla_et_al_2016-Supplementary Data 4 - Nests metadata.csv'), stringsAsFactors=FALSE)
load(here::here("Data/Bulla_et_al_2016-comparative_all.RData"))  

d$inc_start=as.POSIXct(n$incubation_start_datetime[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))])
d$bout_start_j = as.numeric(format(d$datetime_on ,"%j")) - as.numeric(format(d$inc_start,"%j"))+1
d$end_state=n$end_state[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))]
d$lat_a=abs(d$lat)

b = data.table(read_excel(here::here('Data/species_level_variables.xls')))
d$col_dim=b$col_dim_pics_hbw[match(tolower(d$species),tolower(b$species))]

d$time_h=as.numeric(difftime(d$datetime_on,trunc(d$datetime_on,"day"), units = "hours"))+d$bout_length/2 #bout_midpoint
d$time_h=ifelse(d$time_h>24,d$time_h-24,d$time_h)
d$rad=(2*pi*d$time_h) / 24
d=d[d$sex%in%c('f','m'),]
d = data.table(d)

d[, lat_pop := factor(paste(round(mean(lat),2),sp)), pop]
d[, lat_pop := factor(lat_pop, levels=rev(levels(lat_pop)[order(levels(lat_pop))]))]

# fix missing bouts
    di = d[pk == 15593]
    d[pk == 15593, datetime_off := as.POSIXct('1997-05-09 09:12:09')]
    d[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    #d[pk == 15593]

    di[, datetime_on := as.POSIXct('1997-05-09 09:15:09')]
    di[, datetime_off := as.POSIXct('1997-05-09 10:08:49')]
    di[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    di[, sex := 'f']
    di[, pk := 15593.5]

    d = rbind(d,di)
    #d[pk == 15593.5]
    
    di = d[pk == 15766]
    d[pk == 15766, datetime_off := as.POSIXct('1997-05-25 09:05:29')]
    d[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    #d[pk == 15593]

    di[, datetime_on := as.POSIXct('1997-05-25 09:14:09')]
    di[, datetime_off := as.POSIXct('1997-05-25 09:24:29')]
    di[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    di[, sex := 'm']
    di[, pk := 15766.5]

    d = rbind(d,di)
# merge two bouts that are not two, but single bout
    d[pk == 9752, datetime_off:=d[pk == 9753, datetime_off]]
    d[pk == 9752, bout_length:= bout_length+d[pk == 9753, bout_length]]
    d = d[pk != 9753]
    #d[pk == 15766.5]
    d = d[order(pk)]
dm = d[!is.na(bout_start_j)]

# add julian start of bout and incubation
dm[, bout_start_j_pop:=(bout_start_j-1)/max(bout_start_j-1), by = pop]
dm[, bout_start_j_sp:=(bout_start_j-1)/max(bout_start_j-1), by = sp]
dm[, inc_start_j := as.numeric(format(inc_start ,"%j"))]
dm[, inc_start_pop := inc_start_j+1-min(inc_start_j), by = pop]

# add colo
dm[,col_ := ifelse(suborder == 'Scolopaci', female, male)]

# aggregate bout per individual
dd_i=dm[,list(
  sampling=median(sampling,na.rm=TRUE), 
  med_bout=median(bout_length, na.rm=TRUE), 
  mean_bout=mean(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n_=as.numeric(difftime(max(datetime_off),min(datetime_on),days))),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop, lat_pop, year,nest, nn,pk_nest, inc_start,inc_start_pop, sex, end_state, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_) 
  ]

# aggregate bout per nest and sex
dd_n=dm[,list(
  sampling=median(sampling,na.rm=TRUE), 
  med_f=median(bout_length[sex=='f'], na.rm=TRUE), med_m=median(bout_length[sex=='m'], na.rm=TRUE),
  mean_f=mean(bout_length[sex=='f'], na.rm=TRUE), mean_m=mean(bout_length[sex=='m'], na.rm=TRUE),
  mean_bout=mean(bout_length, na.rm=TRUE), med_bout=median(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n=as.numeric(difftime(max(datetime_off),min(datetime_on),days))),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop, lat_pop, year,nest, nn,pk_nest, inc_start, inc_start_pop, end_state, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_)
  ]
  dd_n=dd_n[which(!is.na(dd_n$med_f) & !is.na(dd_n$med_m)),]
  dd_n=dd_n[which(!is.na(dd_n$med_f) & !is.na(dd_n$med_m)),]
  
  dd_n[,fm_diff_mean := mean_f-mean_m]
  dd_n[,fm_diff_med := med_f-med_m]

  dd_n[, n_by_pop := .N, pop]
  
  dd_n[!is.na(inc_start_pop) & n_by_pop>5,  slope_f := rlm(med_f ~ inc_start_pop, weights = n)  %>% coef  %>% magrittr::extract(2), by = lat_pop]   
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & slope_f<0, slope_f_neg := 'yes']
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & !slope_f_neg%in%c('yes'), slope_f_neg := 'no']

  dd_n[!is.na(inc_start_pop) & n_by_pop>5,  slope_m := rlm(med_m ~ inc_start_pop, weights = n)  %>% coef  %>% magrittr::extract(2), by = lat_pop]   
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & slope_m<0, slope_m_neg := 'yes']
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & !slope_m_neg%in%c('yes'), slope_m_neg := 'no']

  dd_n[!is.na(inc_start_pop) & n_by_pop>5,  slope := rlm(med_bout ~ inc_start_pop, weights = n)  %>% coef  %>% magrittr::extract(2), by = lat_pop]   
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & slope<0, slope_neg := 'yes']
  dd_n[!is.na(inc_start_pop) & n_by_pop>5 & !slope_neg%in%c('yes'), slope_neg := 'no']

 dd_p=dd_n[, list(
  mean_f=mean(mean_f), mean_m=mean(mean_m),sd_f=sd(mean_f, na.rm=TRUE),sd_m=sd(mean_m, na.rm=TRUE), 
  med_f=median(med_f, na.rm=TRUE), med_m=median(med_m, na.rm=TRUE),
  lwr_f=quantile(med_f,probs=0.025), upr_f=quantile(med_f,probs=0.975),
  lwr_m=quantile(med_m,0.025), upr_m=quantile(med_m,0.975),
  fm_diff_mean=mean(fm_diff_mean, na.rm=TRUE), sd_fm_diff=sd(fm_diff_mean, na.rm=TRUE),
  fm_diff_med=median(fm_diff_med, na.rm=TRUE), lwr_diff=quantile(fm_diff_med,probs=0.025), upr_diff=quantile(fm_diff_med,probs=0.975),
  lat_med=median(lat, na.rm=TRUE), lwr_lat=quantile(lat,0.025), upr_lat=quantile(lat,0.975),
  ssd = median(log(pop_wing_f) - log(pop_wing_m)), size_f = median(pop_wing_f),
  n = length(pk_nest)),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop,lat_pop, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_)  
      ]

dd_s=dd_p[,  list(
   mean_f=mean(mean_f), mean_m=mean(mean_m),sd_f=sd(mean_f, na.rm=TRUE),sd_m=sd(mean_m, na.rm=TRUE), 
   med_f=median(med_f, na.rm=TRUE), med_m=median(med_m, na.rm=TRUE),
   lwr_f=quantile(med_f,probs=0.025), upr_f=quantile(med_f,probs=0.975),
   lwr_m=quantile(med_m,0.025), upr_m=quantile(med_m,0.975),
   fm_diff_mean=mean(fm_diff_mean), sd_fm_diff=sd(fm_diff_mean, na.rm=TRUE),
   fm_diff_med=median(med_f-med_m, na.rm=TRUE), 
   lwr_diff=quantile(fm_diff_med,probs=0.025), upr_diff=quantile(fm_diff_med,probs=0.975),
   lat_med=median(lat_med, na.rm=TRUE), lwr_lat=quantile(lat_med,0.025), upr_lat=quantile(lat_med,0.975),
   ssd = median(log(pop_wing_f) - log(pop_wing_m)), size_f = median(pop_wing_f),
   n = length(pop)),
    by = list(suborder,genus,animal,sp,scinam,species, col_dim, col_)
   ]

dd_g=dd_s[,  list(
   mean_f=mean(mean_f), mean_m=mean(mean_m),sd_f=sd(mean_f, na.rm=TRUE),sd_m=sd(mean_m, na.rm=TRUE), 
   med_f=median(med_f, na.rm=TRUE), med_m=median(med_m, na.rm=TRUE),
   fm_diff_mean=mean(fm_diff_mean), sd_fm_diff=sd(fm_diff_mean, na.rm=TRUE),
   fm_diff_med=median(fm_diff_med, na.rm=TRUE), 
   lwr_diff=quantile(fm_diff_med,probs=0.025), upr_diff=quantile(fm_diff_med,probs=0.975),
   lwr_lat=quantile(lat_med,0.025), upr_lat=quantile(lat_med,0.975),
   lat_med=median(lat_med, na.rm=TRUE),
   n = length(sp)),
   by = list(suborder,genus)
   ]

# per nest and incubation period
dd_nj=dm[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  bout_cv = sd(bout_length, na.rm=TRUE) / mean(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(pk_nest)),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop,lat_pop, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_, nest, nn, pk_nest, bout_start_j, bout_start_j_pop, bout_start_j_sp)  
      ]

# per pop, sex and incubation period
dd_pj=dm[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  bout_cv = sd(bout_length, na.rm=TRUE) / mean(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(pk_nest)),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop,lat_pop, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_, sex, bout_start_j, bout_start_j_pop, bout_start_j_sp)  
      ]

# per pop & incubation period
dd_pj_=dm[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  bout_cv = sd(bout_length, na.rm=TRUE) / mean(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(pk_nest)),
  by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop,lat_pop, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_dim, col_, bout_start_j,bout_start_j_pop, bout_start_j_sp)  
      ]
  dd_pj_[!is.na(bout_cv),  slope := rlm(bout_cv ~ bout_start_j, weights = n)  %>% coef  %>% magrittr::extract(2), by = lat_pop]   
  dd_pj_[!is.na(bout_cv) & slope<0, slope_neg := 'yes']
  dd_pj_[!is.na(bout_cv)  & !slope_neg%in%c('yes'), slope_neg := 'no']

# per sp and incubation period
dd_sj=dd_pj[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(pop)),
  by = list(suborder,genus,animal,sp,scinam,species,app, sex, bout_start_j,bout_start_j_sp)  
      ]  

# per genus and incubation period
dd_gj=dd_sj[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(sp)),
  by = list(suborder,genus, sex, bout_start_j)  
      ] 

# per genus and incubation period
dd_oj=dd_gj[, list(
  sampling=median(sampling,na.rm=TRUE), 
  bout_length = median(bout_length, na.rm=TRUE),
  lat=median(lat, na.rm=TRUE),lon=median(lon),
  n = length(genus)),
  by = list(suborder,sex, bout_start_j)  
      ]                  

# END