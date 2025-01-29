#' ---
#' title: "Checking robustness of the results for 'Striking consistency in parental care of the sexes across shorebird evolutionary history'"
#' author: "Martin Bulla"
#' date: "`r Sys.time()`"
#' output: 
#'     html_document:
#'         toc: true
#'         toc_float: true
#'         toc_depth: 5
#'         code_folding: hide
#'         bibliography: shorebird_bipInc_sex.bibtex
#'         link-citations: yes
#' ---

#+ r setup, include=FALSE 
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = FALSE)

#' ##### Code to load tools & data
  # constants
    save_plot = TRUE # save_plot as PNG TRUE/FALSE
    fam = "Arial Unicode MS"# text family
    female='#FCB42C' # 'deepskyblue'
    male='#535F7C' # 'red'
    ax_lines = "grey60"
    size_l = 0.75
    tx_legend_tit = 6
    inch = 0.393701 
    nsim = 5000
    scale_size = 0.352778 # a scaling factor for the size outside ggplot theme (which is in mm) to match point size in the theme

    font_size = 2.5 # in the TREE
    ladderize_ = TRUE # ladderize TREE?

  # packages and functions
    require(here)
    source(here::here('R/tools.R'))  # TODO:in the final version bring it here within the text
 
  # prepare max cred tree
    trees =  read.nexus(here::here("Data/Bulla_et_al_2016-100_Trees_Hackett_all_species_niv.tre"))
    tree <- maxCladeCred(trees)
    tree$tip.label[tree$tip.label=="Catoptrophorus_semipalmatus"]="Tringa_semipalmata" # current name for this species  
      #plot(tree, main="Maximum Credibility Tree")  
      #plot(ladderize(tree, right = TRUE))
    tree$tip.label = gsub("_", " ", tree$tip.label)

  # load and prepare bout data
    load(here::here("Data/Bulla_et_al_2016-comparative_all.RData"))  
    d$lat_a=abs(d$lat)
    d=d[d$sex%in%c('f','m'),]
   
    # add inc_start
    n=fread(here::here('Data/Bulla_et_al_2016-Supplementary Data 4 - Nests metadata.csv'), stringsAsFactors=FALSE)
    
    d$inc_start=as.POSIXct(n$incubation_start_datetime[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))]) 

    d$bout_start_j = as.numeric(format(d$datetime_on ,"%j")) - as.numeric(format(d$inc_start,"%j"))+1
    d$end_state=n$end_state[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))]
    d$lat_a=abs(d$lat)

    d = data.table(d)
    #d[, length(unique(nest)), by = list(scinam, breeding_site)] 

    # adjust variables
      d[, lat_pop := factor(paste(round(mean(lat),2),sp)), pop]
      d[, lat_pop := factor(lat_pop, levels=rev(levels(lat_pop)[order(levels(lat_pop))]))]
      
      d[,col_ := ifelse(suborder == 'Scolopaci', female, male)] # adds color for ploting
      # adjust variaables
      d[, pop_lat :=paste(scinam, substring(lat_pop, 1,5))] #dd_n10[n_by_pop>10, length(unique(pop_lat))] #dd_n10[n_by_pop>10, unique(pop_lat)]
      d[pop_lat=="Arenaria interpres 71 RU", pop_lat:="Arenaria interpres 71.00"]
      d[pop_lat=="Tringa totanus 52.9 ", pop_lat:="Tringa totanus 52.90"]
      d[pop_lat=="Limosa haemastica 61.2 ", pop_lat:="Limosa haemastica 61.20"]
      d[pop_lat=="Charadrius alexandrinus 24.26", pop_lat:="Char. alexandrinus 24.26"]
      d[pop_lat=="Charadrius alexandrinus 41.29", pop_lat:="Char. alexandrinus 41.29"]
      d[pop_lat=="Haematopus bachmani 61.09", pop_lat:="Haem. bachmani 61.09"]
      d[pop_lat== "Numenius phaeopus 63.83", pop_lat:= "Numen. phaeopus 63.83"]  

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

    # aggregate bout per nest and sex TODO:remove unused variables
    dd_n=d[,list(
      sampling=median(sampling,na.rm=TRUE), 
      med_f=median(bout_length[sex=='f'], na.rm=TRUE), med_m=median(bout_length[sex=='m'], na.rm=TRUE),
      mean_f=mean(bout_length[sex=='f'], na.rm=TRUE), mean_m=mean(bout_length[sex=='m'], na.rm=TRUE),
      n_f= length(bout_length[sex=='f']), n_m= length(bout_length[sex=='m']),
      mean_bout=mean(bout_length, na.rm=TRUE), med_bout=median(bout_length, na.rm=TRUE),
      n= length(bout_length),
      lat=median(lat, na.rm=TRUE),lon=median(lon),
      n_days=as.numeric(difftime(max(datetime_off),min(datetime_on),days)),
      med_bout_start_j = median(bout_start_j, na.rm=TRUE)),
      by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop, lat_pop, pop_lat, year,nest, nn,pk_nest, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_)
      ]
      dd_n[, n_fm:=ifelse(n_f>n_m, n_f, n_m)]
    # limit data
      dd_n=dd_n[which(!is.na(dd_n$med_f) & !is.na(dd_n$med_m)),]
      dd_n10 = dd_n[n>=10 & n_f>=5 & n_m>=5]#; nrow(dd_n10)#; dd_n10[n_by_pop>10,length(unique(pop))]

    # N nests
      dd_n10[, n_by_pop := .N, pop]#; dd_n10[n_by_pop>10, length(unique(pop))]
      dd_n10[, n_by_sp := .N, sp]#; nrow(dd_n10[sp>10]); dd_n10[n_by_sp>10,length(unique(sp))]
    
    # estimate within population slopes
      # r pearsons
      dd_n10[n_by_pop>5,  r := cor(med_f, med_m), by = pop]
      dd_n10[n_by_pop>5 & r<0, r_neg := 'yes']
      dd_n10[n_by_pop>5 & !r_neg%in%'yes', r_neg := 'no']
      
      # rlm
      dd_n10[n_by_pop>5,  slope_pop := rlm(med_f ~ med_m, weights = n)  %>% coef  %>% magrittr::extract(2), by = pop] 
      dd_n10[n_by_pop>5 & slope_pop<0, slope_pop_neg := 'yes']
      dd_n10[n_by_pop>5 & !slope_pop_neg%in%'yes', slope_pop_neg := 'no']
      dd_n10[n_by_pop>5, slope_pop_certain := simulate_rlm(.SD), by = pop] #x = dd_n10[n_by_pop>10 & !duplicated(pop), ]; summary(factor(x$slope_pop_certain)) 

    # estimate within species slopes
      # r pearsons
      dd_n10[n_by_sp>5,  r_sp := cor(med_f, med_m), by = scinam]
      dd_n10[n_by_sp>5 & r_sp<0, r_sp_neg := 'yes']
      dd_n10[n_by_sp>5 & !r_sp_neg%in%'yes', r_sp_neg := 'no']

      # rlm
      dd_n10[n_by_sp>5,  slope_sp := rlm(med_f ~ med_m, weights = n)  %>% coef  %>% magrittr::extract(2), by = scinam] 
      dd_n10[n_by_sp>5 & slope_sp<0, slope_sp_neg := 'yes']
      dd_n10[n_by_sp>5 & !slope_sp_neg%in%'yes', slope_sp_neg := 'no']
      dd_n10[n_by_sp>5, slope_sp_certain := simulate_rlm(.SD), by = scinam]   

    # Within nest bout pair similarity
    # prepare data
    # fix issue with non-alternating sex
        fm = copy(d)
        fm = fm[!pk == 15799] # fixed for KEPL tuzl 1997 1997-D-76 1  
        fm = fm[!pk %in%c(15035,15036,15037)] # fixed for KEPL ddll 2007 AC07 1
        fm = fm[!pk%in%c(14975,14976,14977,14978,14979)] # fixed for KEPL ddll 2006 V2 1

        # other cases
        fm[ , sex_next := data.table::shift(sex, type = 'lead'), by = pk_nest]
        #nrow(fm[!is.na(sex_next) & sex == sex_next])
        fm = fm[!(!is.na(sex_next) & sex == sex_next)]
        
        #nrow(fm)

        # DONE check
            #fm[ , sex_prev := data.table::shift(sex), by = pk_nest]
            #fm[ , sex_next := data.table::shift(sex, type = 'lead'), by = pk_nest]
            #nrow(fm[!is.na(sex_next) & sex == sex_next])
            #fm[ , off_prev := data.table::shift(datetime_off), by = pk_nest]
            #fm[ , on_next := data.table::shift(datetime_on, type = 'lead'), by = pk_nest]
            #fm[ , pk_next := data.table::shift(pk, type = 'lead'), by = pk_nest]
            
        
            #fm[!is.na(sex_next) & sex == sex_next, unique(pk_nest)]
            #fm[!is.na(sex_next) & sex == sex_next, .(pk_nest, bout_length, sex, sex_prev, sex_next, off_prev, datetime_on, datetime_off, on_next, pk, pk_next)]
            #fm[!is.na(sex_next) & sex == sex_next, .(pk_nest, bout_length, sex, sex_next,  datetime_on, datetime_off, pk)]
            #print(fm[!is.na(sex_next) & pk_nest == 'BLGO frie 2013 NA-B319 2', .(pk, pk_nest, bout_length, sex, sex_prev, sex_next, off_prev, datetime_on, datetime_off, on_next)], nrow=1000)
            # some bouts for these nests were removed #fm[!is.na(sex_next) & sex == sex_next & pk_nest %in% c('BLGO frie 2013 NA-B319 2','BLOY hafj 2005 101-05A 1','BLOY hafj 2005 114-05A 1','BLOY hafj 2006 114-06B 2','BLOY hafj 2006 115-06B 2','BLOY hafj 2006 118-06A 1','KEPL tuzl 1997 1997-D-37 1','LRPL czrp 2014 LR504_CZ2014 0','SAND zack 2007 S30 1','GRPL kois 2011 KR24 1','KEPL ddll 2006 A16 1','KEPL ddll 2006 A20 1','KEPL ddll 2006 CA3 1','KEPL ddll 2007 AL07 1','KEPL ddll 2007 MA07 1','KEPL ddll 2008 AB08 1','KEPL ddll 2008 CAA08 1')]
                                                                    
    # create dataset
        fm[ , bout_m := data.table::shift(bout_length, type = 'lead'), by = pk_nest]
        fm = fm[sex=='f' & !is.na(bout_m)]
        setnames(fm, old = 'bout_length', new = 'bout_f')
        fm[ ,(c('sex', 'bird_ID', 'sex_next')) := NULL]

    # shorten names for plotting  
        fm[scinam=="Charadrius alexandrinus", scinam:= 'Char. alexandrinus']
        fm[scinam=="Charadrius semipalmatus", scinam:= 'Char. semipalmatus']
        fm[scinam=="Haematopus ostralegus", scinam:= 'Haem. ostralegus']
        fm[scinam=="Haematopus bachmani", scinam:= 'Haem. bachmani']
        fm[scinam=="Limnodromus scolopaceus" , scinam:= 'Limn. scolopaceus']
        fm[scinam=="Numenius madagascariensis" , scinam:= 'N. madagascariensis']

    # estimate within pair bout correlations
        fm[, n_by_nest := .N, pk_nest] #summary(fm$n_by_nest); fm[n_by_nest>6, length(unique(pk_nest))]
        # pearson
        fm[n_by_nest>=5,  r := cor(bout_f, bout_m), by = pk_nest]
        fm[n_by_nest>=5 & r<0, r_neg := 'yes']
        fm[n_by_nest>=5 & !r_neg%in%'yes', r_neg := 'no']

        # rlm
        fm[n_by_nest>=5,  slope_nest := rlm(bout_f ~ bout_m, maxit = 200)  %>% coef  %>% magrittr::extract(2), by = pk_nest] 
        fm[n_by_nest>=5 & slope_nest<0, slope_nest_neg := 'yes']
        fm[n_by_nest>=5 & !slope_nest_neg%in%'yes', slope_nest_neg := 'no']
        fm[n_by_nest>=5, slope_nest_certain := simulate_rlm_no_weights(.SD), by = pk_nest]

        # DONE find a group that causes the warning
        done = TRUE
        if(done!=TRUE){
        fm5 = fm[n_by_nest>=5]
        problematic_groups <- lapply(unique(fm5$pk_nest), function(g) {
            tryCatch({
            fm5[pk_nest == g, slope_nest := rlm(bout_f ~ bout_m, maxit = 200)  %>% coef  %>% magrittr::extract(2)]
            NULL  # Return NULL if no warning
            }, warning = function(w) {
            list(group = g, warning = w$message)  # Capture the group and warning message
            })
        })

        problematic_groups <- Filter(Negate(is.null), problematic_groups) # Filter out NULLs to see only problematic groups

        # Display problematic groups, if any
        if (length(problematic_groups) > 0) {
            print(problematic_groups)
        } else {
            print("No warnings found.")
        }
        }
        
        #fm = fm[!pk_nest =="AMGP chur 2013 13AMGP03W 1"] # remove the group where rlm failed to converge #ggplot(x, aes(x = bout_m, y = bout_f)) + geom_point()

        # aggregate
        fmr = fm[!is.na(r), list(
            r = unique(r),
            bout_m_min = min(bout_m),
            bout_m_max = max(bout_m),
            bout_f_min = min(bout_f),
            bout_f_max = max(bout_f),
            bout_start_j=median(bout_start_j, na.rm= TRUE)
            ),
            by = list(suborder,genus,animal,sp,scinam,species,year, breeding_site,pop,lat_pop, pop_lat, nest, nn, app, tidal, tidal_pop,pop_wing_f, pop_wing_m, pk_nest, lat, n_by_nest, r_neg,slope_nest_certain)  
        ] # same as fmr = fm[!(is.na(r) | duplicated(paste(r, pk_nest))) ]

xt = dd_n10[!duplicated(pk_nest)]
xt = xt[n_by_sp>10]

#'***
#' ## Is assortative mating confounded by data collection?
#' ### Is the median ♂/♀ bout per nest related to number of days a nest was monitored?
#+ ft_1, fig.width=20*inch,fig.height=12*inch
ggplot(data = xt) + 
geom_point(aes(x = n_days, y = med_f), fill = 'red', alpha = 0.5, col = "white", pch = 21) +
geom_point(aes(x = n_days, y = med_m), fill = 'blue', alpha = 0.5, col = "white", pch = 21) +
facet_wrap(~scinam, ncol = 6) + 
labs(x = "# days recorded", y = "Bout length [h]")
#' **!!! It is reassuring to see that no, with exception of turnstone. !!!**  
#'  
#' ### Is the median ♂/♀ bout per nest confounded by when within the incubation period the data was collected?
#' 
#' Here I used the median day of incubation period from all collected bouts for a given nest.
#+ ft_2, fig.width=20*inch,fig.height=12*inch
ggplot(data = xt) + 
geom_point(aes(x = med_bout_start_j, y = med_f), fill = 'red', alpha = 0.5, col = "white", pch = 21) +
#stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_f), col = 'red')+
geom_point(aes(x = med_bout_start_j, y = med_m), fill = 'blue', alpha = 0.5, col = "white", pch = 21) +
#stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_m), col = 'blue')+
facet_wrap(~scinam, ncol = 6) + 
labs(x = "Median incubation period of the recorded bouts", y = "Bout length [h]")

#' I see a rather similar picture to the above effect of days. Adding lines to highlight trends:
#+ ft_3, fig.width=20*inch,fig.height=12*inch
ggplot(data = xt) + 
geom_point(aes(x = med_bout_start_j, y = med_f), fill = 'red', alpha = 0.5, col = "white", pch = 21) +
stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_f), col = 'red')+
geom_point(aes(x = med_bout_start_j, y = med_m), fill = 'blue', alpha = 0.5, col = "white", pch = 21) +
stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_m), col = 'blue')+
facet_wrap(~scinam, ncol = 6) + 
labs(x = "Median incubation period of the recorded bouts", y = "Bout length [h]")

#' Perhaps in some species the incubation biases the bout lengths, but  does it influence assortative mating estimates?
#+ ft_4, fig.width=8*inch,fig.height=8*inch
require(foreach)
p = foreach(i = unique(xt$scinam), .combine = rbind) %do% {
  # i = 'Calidris pusilla'
  xti = xt[scinam==i]  
  mi = lm(med_f~med_m, xti)
  t_mi =m_out(mi, paste(i, "simple"), dep = 'med_f', save_sim = FALSE, type = "lm")
  t_mi[,scinam := i]
  t_mi[, model :='simple']
  t_mi[, N :=N[1]]
  
  mji = lm(med_f~med_bout_start_j + med_m, xti)
  t_mji =m_out(mji, paste(i, "inc_per"), dep = 'med_f', save_sim = FALSE, type = "lm")
  t_mji[,scinam := i]
  t_mji[, model :='inc_per']
  t_mji[, N :=N[1]]
  
  t_mji[, estimate_s:=c(t_mi$estimate_r[1],NA,t_mi$estimate_r[2])]
  t_mji[, lwr_s:=c(t_mi$lwr_r[1],NA,t_mi$lwr_r[2])]
  t_mji[, upr_s:=c(t_mi$upr_r[1],NA,t_mi$upr_r[2])]

  return(t_mji)
}
ggplot(p[effect%in%'med_m'], aes(x = estimate_s, y = estimate_r)) +  
  geom_point() +
  #geom_errorbar(aes(ymin = lwr_r, ymax = upr_r), width = 0.1) +  # Error bars for y-axis
  #geom_errorbarh(aes(xmin = lwr_s, xmax = upr_s), height = 0.1)  # Error bars for x-axis
  geom_abline(intercept = 0, slope = 1, lty = 3, col = "grey") +
  labs(x = "Estimate", y = "Estiamte controlled for incubation period")
#' **!!! It is reassuring to see that NO.!!!** The estimates from a simple model - lm(med_f~med_m) - are similar to those from a model controlled for median incubation period of the data lm(med_f~inc_per+med_m)   
#' 
#' I feel this is enough and we do not need to check whether assortative mating holds when only part of the incubation period is used.  
#' 
#'***
#' 
#' ## Is the strength of within nest correlations linked to nest success?
n[, pk_nest:=paste(sp, breeding_site, year, nest, nn)]
vn = merge(v,n[,.(pk_nest,end_state)], all.x = TRUE)
#' Distribution of nest end states
table(vn$end_state)

#' I have thus used d (deserted) and p (predated) as success = 0 and h (hatched) and fl (fletched) as success = 1. The rest (trampled, unknown) were not used. Fitting the following model:
vn[end_state%in%c('fl','h'), success :=1]
vn[end_state%in%c('p','d'), success :=0]
vn01 = vn[!is.na(success)]
m = glmer(success~r +(1|genus) + (1|species) + (1|lat_pop), family = binomial, data = vn01)
#m = glmer(success~scale(r)  + (scale(r)|lat_pop), family = binomial, data = vn01, control = glmerControl(optimizer = "nloptwrap", optCtrl = list(xtol_rel = 1e-6)))
summary(m)

#' **If anything, the results show an opposite trend**.
#' 
#'We have also discussed the possibility "to test for the convergence by looking at the correlation in bout length early and late in incubation to show the synchronisation and contrast that for successful and failed? If we show that pairs align their bouts as nests age and nests that do it more quickly are more likely to hatch – implication of ass mating. If not, then it is not convergence of behaviour – whether it is active/passive choice, there is pre-programmed or environmental influences". 
#' 
#' Shall I still try it? My worry is that it will be a lot of work for nothing because (a) nest success results above and (b) even if we have the whole incubation period for some nests, we will be truly limited in sample size to test anything. As of now, I use minimum 5 or 10 pairs of male-femal bouts