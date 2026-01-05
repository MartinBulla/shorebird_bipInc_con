#' ---
#' title: "Draft of 'Striking consistency in parental care of the sexes across shorebird evolutionary history'"
#' author: "Martin Bulla"
#' date: "`r Sys.time()`"
#' bibliography: ../Resources/_bib.bib
#' link-citations: true
#' output:
#'     html_document:
#'         toc: true
#'         toc_float: true
#'         toc_depth: 4
#'         code_folding: hide
#'         link-citations: yes
#'         css: ../Resources/styles.css
#' base:  href="/[shorebird_bipinc_con]/"
#' ---

#+ r setup, include=FALSE 
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)

# TODO:check whether densityscale in trees are correct in fs2 surely not and fig 1c likely also not
# TODO:check compare predictions on the original scale (i.e., back-transform from log) and  explore whether log(bout_m) or bout_m_z is more biologically interpretable?

#' ##### Code to load tools & data
  # constants
    save_plot = TRUE # save_plot as PNG TRUE/FALSE
    fam = "Arial Unicode MS"# text family
    female='#FCB42C' # 'deepskyblue'
    male='#535F7C' # 'red'
    green_ = '#4A9F4A'
    red_ = "#D43F3AFF" # ggsci::pal_locuszoom()(5)    
    blue_ =  "#46B8DAFF"
    lz_colors <- c(
      "#3B4CC0",  # blue
      "#78AADD",  # light blue
      "#79C36A",  # green
      "#E1A439",  # orange
      "#D92120"   # red
    )
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
   
    # add inc_start and end state
    n=fread(here::here('Data/Bulla_et_al_2016-Supplementary Data 4 - Nests metadata.csv'), stringsAsFactors=FALSE)
    
    d$inc_start=as.POSIXct(n$incubation_start_datetime[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))]) 

    d$bout_start_j = as.numeric(format(d$datetime_on ,"%j")) - as.numeric(format(d$inc_start,"%j"))+1
    d$end_state=n$end_state[match(tolower(paste(d$breeding_site,d$year,d$nest,d$nn)),tolower(paste(n$breeding_site,n$year,n$nest, n$nn)))]
    d$lat_a=abs(d$lat)

    d = data.table(d)
    #d[, length(unique(nest)), by = list(scinam, breeding_site)] 
    # incubation start as % of species incubation period
    ip =  fread(here::here('Data/inc_period.csv'), stringsAsFactors=FALSE)
    d = merge(d,ip[,.(scinam, inc_per)], all.x = TRUE)
    d[pk_nest%in%d[bout_start_j%in%0, pk_nest], bout_start_j := bout_start_j+1] # adjust wrongly assigned bout_start_j 
    d$prop_ip=d$bout_start_j/d$inc_per

    # add body mass
    b=fread(here::here('Data/Supplementary Data 5 - Birds metadata.csv'), stringsAsFactors=FALSE)
    b_filtered <- b[!(is.na(wing) & is.na(mass))]  # Remove rows where both wing & mass are NA
    #b_filtered[, .N, by = .(nest, bird_ID)][N > 1] # check multiple catches of a bird 
    b_both =  b[!is.na(wing) & !is.na(mass)] # non-na wing and mass
    b_both_mean <- b_both[, .( # get mean values per bird, nest and year
        wing = mean(wing, na.rm = TRUE), 
        mass = mean(mass, na.rm = TRUE)
      ), by = .(year, nest, bird_ID)]
    
    d <- merge(d, b_both_mean[, .(year, nest, bird_ID, wing, mass)], by = c("year","nest", "bird_ID"), all.x = TRUE) # merge with d while keeping all d records

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

    # merge two bouts that are not two, but a single bout
      d[pk == 9752, datetime_off:=d[pk == 9753, datetime_off]]
      d[pk == 9752, bout_length:= bout_length+d[pk == 9753, bout_length]]
      d = d[pk != 9753]
      #d[pk == 15766.5]
      
      d[pk == 3813, datetime_off:=d[pk == 3814, datetime_off]]
      d[pk == 3813, bout_length:= bout_length+d[pk == 3814, bout_length]]
      d = d[pk != 3814]


      
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
      med_bout_start_j = median(bout_start_j, na.rm=TRUE),
      wing_f=mean(wing[sex=='f'], na.rm=TRUE),
      wing_m=mean(wing[sex=='m'], na.rm=TRUE),
      mass_f=mean(mass[sex=='f'], na.rm=TRUE),
      mass_m=mean(mass[sex=='m'], na.rm=TRUE)
      ),
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

      dd_n10[n_by_pop>5,  r_wing := cor(wing_f, wing_m), by = pop]
      dd_n10[n_by_pop>5 & r_wing<0, r_wing_neg := 'yes']
      dd_n10[n_by_pop>5 & !r_wing_neg%in%'yes', r_wing_neg := 'no']
      
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

#'***
#' ## TODO:ABSTRACT
#' 
#' <br> 
#' Here, we use unprecedented comparative dataset on ~ `r as.character(round_any(nrow(d), 100))` incubation bouts from `r length(unique(d$pk_nest))` nests of `r length(unique(d$pop))` populations of `r length(unique(d$sp))` shorebird species from `r length(unique(d$genus))` genera to investigate how female bouts correlate with those of males and whether the correlations differ or are consistent across evolutionary history.  
#' <br>
#'
#' ***

#' ### Assortative mating for incubation bouts
#' #### Across and within populations
a = dd_n10[n_by_sp>10 & !duplicated(scinam)]

quantile(a$r_sp, probs = c(0.025,0.5,0.975)); mean(a$r_sp) 
wtd.quantile(a$r_sp, a$n_by_sp, probs = c(0.025,0.5,0.975));wtd.mean(a$r_sp, a$n_by_sp) # desriptive stats weighing by number of nests (note that weighing by number of m-f bouts is meaningless in the cross species context where species differ in bout lengths)

#+ f1, fig.width=20*inch,fig.height=19.5*inch
  
  f1a = 
  ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = scinam, weight=n_fm)) + 
      geom_point(aes(size = n, col = suborder), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE,  col = 'grey40', aes(lwd = slope_sp_certain))+ #linewidth = size_l,alpha = 0.2,
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = med_m, y = med_f, label = ..r.label..), inherit.aes = FALSE) +

      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Statistically clear\nregression")+ 
      scale_size(name = "# of ♀-♂ bout\npairs", breaks = c(10, 100, 200)) + 
      #scale_size(breaks = c(1,15,30), name = 'n days') +

      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      #coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      #labs(subtitle = "A")+
      labs(tag = 'A')+
      facet_wrap(~scinam, ncol = 6, scales = "free") + 
      theme_MB + 
      theme(strip.background = element_blank()
           #panel.background = element_rect(fill = "transparent",
            #                     colour = NA_character_), # necessary to avoid drawing panel outline
    
          #plot.background = element_rect(fill = "transparent",
           #                         colour = NA_character_), # necessary to avoid drawing plot outline
      #legend.background = element_rect(fill = "transparent"),
      #legend.box.background = element_rect(fill = "transparent"),
      #legend.key = element_rect(fill = "transparent")
      )

  # add inset
    adding_inset <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
      layer(data = data, stat = StatIdentity, position = PositionIdentity, 
            geom = ggplot2:::GeomCustomAnn,
            inherit.aes = TRUE, params = list(grob = grob, 
                                              xmin = xmin, xmax = xmax, 
                                              ymin = ymin, ymax = ymax))
    }
    inset_dunl =
      ggplot(dd_n10[n_by_pop>10 & scinam == 'Calidris alpina'],aes(x = med_m, y = med_f, group = pop, weight=n_fm)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))

    inset_sesa = 
      ggplot(dd_n10[n_by_pop>10 & scinam == 'Calidris pusilla'],aes(x = med_m, y = med_f, group = pop, weight=n_fm)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))

    inset_kepl = 
      ggplot(dd_n10[n_by_pop>10 & scinam == "Charadrius alexandrinus"],aes(x = med_m, y = med_f, group = pop, weight=n_fm)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))
    
    inset_amgp =
      ggplot(dd_n10[n_by_pop>10 & scinam == "Pluvialis dominica"],aes(x = med_m, y = med_f, group = pop, weight=n_fm)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))

    ann_text <- data.frame(
      med_m  = ggplot_build(f1a)$layout$panel_params[[2]]$x.range[2]-(ggplot_build(f1a)$layout$panel_params[[2]]$x.range[2]-ggplot_build(f1a)$layout$panel_params[[2]]$x.range[1])*0.4, 
      med_f = ggplot_build(f1a)$layout$panel_params[[2]]$y.range[1]+(ggplot_build(f1a)$layout$panel_params[[2]]$y.range[2]-ggplot_build(f1a)$layout$panel_params[[2]]$y.range[1])*0.4,lab = "Populations",
      scinam = factor('Calidris alpina',levels = dd_n10[n_by_pop>10, levels(factor(scinam))]),
      n_fm = 1)

    f1a_ = 
    f1a +
    adding_inset(
        grob=ggplotGrob(inset_dunl), 
        data = dd_n10[n_by_pop>10 & scinam == 'Calidris alpina'],
        ymin = ggplot_build(f1a)$layout$panel_params[[2]]$y.range[1], 
        ymax= ggplot_build(f1a)$layout$panel_params[[2]]$y.range[1]+(ggplot_build(f1a)$layout$panel_params[[2]]$y.range[2]-ggplot_build(f1a)$layout$panel_params[[2]]$y.range[1])*0.4, 
        xmin=ggplot_build(f1a)$layout$panel_params[[2]]$x.range[2]-(ggplot_build(f1a)$layout$panel_params[[2]]$x.range[2]-ggplot_build(f1a)$layout$panel_params[[2]]$x.range[1])*0.4, 
        xmax=ggplot_build(f1a)$layout$panel_params[[2]]$x.range[2])   +
    adding_inset(
        grob=ggplotGrob(inset_sesa), 
        data = dd_n10[n_by_pop>10 & scinam == 'Calidris pusilla'],
        ymin = ggplot_build(f1a)$layout$panel_params[[5]]$y.range[1], 
        ymax=ggplot_build(f1a)$layout$panel_params[[5]]$y.range[1]+(ggplot_build(f1a)$layout$panel_params[[5]]$y.range[2]-ggplot_build(f1a)$layout$panel_params[[5]]$y.range[1])*0.4, 
        xmin=ggplot_build(f1a)$layout$panel_params[[5]]$x.range[2]-(ggplot_build(f1a)$layout$panel_params[[5]]$x.range[2]-ggplot_build(f1a)$layout$panel_params[[5]]$x.range[1])*0.4, 
        xmax=ggplot_build(f1a)$layout$panel_params[[5]]$x.range[2]) +
    adding_inset(
        grob=ggplotGrob(inset_kepl), 
        data = dd_n10[n_by_pop>10 & scinam == 'Charadrius alexandrinus'],
        ymin = ggplot_build(f1a)$layout$panel_params[[6]]$y.range[1], 
        ymax=ggplot_build(f1a)$layout$panel_params[[6]]$y.range[1]+(ggplot_build(f1a)$layout$panel_params[[6]]$y.range[2]-ggplot_build(f1a)$layout$panel_params[[6]]$y.range[1])*0.4, 
        xmin=ggplot_build(f1a)$layout$panel_params[[6]]$x.range[2]-(ggplot_build(f1a)$layout$panel_params[[6]]$x.range[2]-ggplot_build(f1a)$layout$panel_params[[6]]$x.range[1])*0.4, 
        xmax=ggplot_build(f1a)$layout$panel_params[[6]]$x.range[2]) +
    adding_inset(
        grob=ggplotGrob(inset_amgp), 
        data = dd_n10[n_by_pop>10 & scinam == 'Pluvialis dominica'],
        ymin = ggplot_build(f1a)$layout$panel_params[[15]]$y.range[1], 
        ymax=ggplot_build(f1a)$layout$panel_params[[15]]$y.range[1]+(ggplot_build(f1a)$layout$panel_params[[15]]$y.range[2]-ggplot_build(f1a)$layout$panel_params[[15]]$y.range[1])*0.4, 
        xmin=ggplot_build(f1a)$layout$panel_params[[15]]$x.range[2]-(ggplot_build(f1a)$layout$panel_params[[15]]$x.range[2]-ggplot_build(f1a)$layout$panel_params[[15]]$x.range[1])*0.4, 
        xmax=ggplot_build(f1a)$layout$panel_params[[15]]$x.range[2]) +
    geom_text(data = ann_text,label = "Populations", size = 1.8, hjust = 0, vjust = 0, col = 'grey30')    

  #export ggsave(file = here::here("Output/Fig_1A_width-180mm_test.png"), f1a_, width = 20, height = 10, units = "cm")

# f1b fig.width=10*inch,fig.height=10*inch
f1b =
    ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = sp, weight=n_fm, color = suborder)) + 
      #geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE, alpha = 0.5, aes(lwd = slope_sp_certain))+ #col = 'grey40', 
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_y_continuous("♀ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
      #annotate("text", x=2.9, y=15.5, label= "Fits to nests' median bouts", col = "grey30", size = 2, hjust = 0) + 
      #geom_segment(aes(x = 0.75, y = 15.5, xend = 2.5, yend = 15.5), color = "darkgrey", linewidth = .35) +

      #annotate("text", x=3, y=14.5, label= "Slope", col = "grey30", size = 2, hjust = 0) + 
      #annotate("text", x=1.1, y=14.5, label= "+", col = male, size = 2.75) + 
      #annotate("text", x=2.2, y=14.5, label = "-", col = female, size = 2.75)+
      labs(tag = 'B', subtitle = "")+
      annotate("text", x = 0.25, y = 15.5,                # Set position manually
        label = "Regression lines from A",
        size = 7*scale_size, colour="grey30",
        hjust = 0
      )+
      theme_MB+ #yheme_bw() +theme_MB2+
      theme(legend.position="none"
            #plot.background = element_rect(fill = "transparent",
                            #  colour = NA_character_) # necessary to avoid drawing plot outline
      )
  #export ggsave(file = here::here("Output/Fig_1B_width-90mm.png"), f1b, width = 6.6, height = 6.7, units = "cm")

# f1c fig.width=10*inch,fig.height=10*inch TODO:check why images are not showing
# prepare colors
cols_f1 <- rev(c(brewer.pal(11, "Spectral")[1], brewer.pal(11, "Spectral")[4], brewer.pal(11, "Spectral")[7:11]))

# prepare data and tree
ds = dd_n10[n_by_sp>10]
ds = ds[, cor(med_f, med_m), by = list(scinam, animal)]  %>% setnames(old = 'V1', new = 'r')
ds = merge(ds, dd_n10[!duplicated(scinam), .(scinam,n_by_sp)])
    #summary(ds); summary(ds[!scinam%in%'Limosa lapponica'])
ds[, genus:=sub("\\_.*", "", animal)]

   # DELETE sp_r=data.frame(ds[,c("r","scinam")])
   # DELETE tree_r = drop.tip(tree, tree$tip.label[!tree$tip.label%in%sp_r$scinam])

# unify limits for color plotting
lims_tip_f1 <- range(ds$r, na.rm = TRUE)   

# ladderize
if (ladderize_ == FALSE) {
    treei <- drop.tip(tree, setdiff(tree$tip.label, ds$scinam))
} else {
   treei <- drop.tip(tree, setdiff(tree$tip.label, ds$scinam)) %>% ladderize(right =TRUE)
}

# reconstrunct ancestral state using phytools
colelab <- ds$r
names(colelab) <- ds$scinam
fit <- phytools::fastAnc(treei, colelab, vars = FALSE, CI = FALSE)
nd <- data.table(node = names(fit), trait = as.numeric(fit)) 
td <- data.table(node = ggtree::nodeid(treei, names(colelab)), trait = colelab)
ptr <- rbind(td, nd)
ptr[, node := as.numeric(node)]
treei_c <- dplyr::full_join(treei, ptr, by = "node")

# prepare phylogenetic contrasts
r_pear=ds$r
names(r_pear)=ds$scinam
yourPics <- pic(x=r_pear, phy=treei)

contrast_data <- data.table(
  node = (Ntip(treei) + 1):(Ntip(treei) + Nnode(treei)),
  pic = yourPics
)

treei_c <- treei_c %>%
  left_join(contrast_data, by = "node")

# prepare genera images
images = data.table(image = list.files(
    path = here::here("Illustrations/for_tree/"), 
    pattern = "\\.png$", full.names = TRUE),
    genus = sub("\\_.*", "", list.files(path = "Illustrations/for_tree/", pattern = "\\.png$", 
    full.names = FALSE)),
    genus_y = c(4.5,2.5,13, 15.5,9,11,17.5, 6.5),
    genus_x = 105,
    col = c("lightgrey","lightgrey","darkgrey", "lightgrey", "darkgrey", "lightgrey", "darkgrey", "lightgrey"),
    width_tree = c(0.9, 0.88, 0.83, 1.2, 1.1, 1.12, 0.75,0.87),
    #width_tree = c(0.9, 0.85, 0.8, 1.2, 1.1, 1.08,0.9,0.91),
    #width_tree = c(0.9, 0.72, 0.6, 1.67, 1.52, 1.6,0.8,1.07),
    bird_size = c(23.5, 19, 15.5,43.75, 39.75, 42, 26, 28)
    )
images$width = image_info(image_read(images$image))$width 
images$height = image_info(image_read(images$image))$height 


# add node indentifiers for vertical genus bars 
treeid <- data.table(as_tibble(tidytree::as.treedata(treei)))
treeid[, genus:=sub("\\ .*", "", label)]
nod = treeid[, min(parent), genus] %>% setnames(c('V1'),c('node'))
images = merge(images, nod)
images[, name := NA]
images[genus=='Numenius', node := 1]
images[genus=='Arenaria', node := 7]

#default_size <- ggplot2:::check_subclass("point", "Geom")$default_aes$size

# plot tree
p <- ggtree::ggtree(treei_c, ladderize = ladderize_, right = TRUE) + #layout = "circular", 
    geom_tree(aes(color = trait), continuous = "colour", size = 1) +
    geom_tiplab(offset = 0.5, fontface = "italic", colour = "grey30", size = 2.35)+
    scale_color_gradientn(colours = cols_f1, limits = lims_tip_f1, oob = scales::squish, name = "Assortative mating") 
    geom_image(data = images, 
             aes(
                x = genus_x, y = genus_y, image = image, size = I(width_tree/10)), by='width')+#, size = 0.1) +#inherit.aes = FALSE) +  # Adjust x and size as needed #, by = "width" 
    geom_point(data = data.frame(x = 97.5, y = c(5)), aes(x =x, y = y), color = "darkgrey", shape = 15, size = 1) +
    geom_point(data = data.frame(x = 97.5, y = c(11)), aes(x =x, y = y), color = "lightgrey", shape = 15, size = 1) +
    #geom_point(
    #        aes(x = x, y = y, size = abs(pic)), #sqrt(abs(pic/pi))),
    #        fill = "grey90", color = "grey50", pch = 21) +
    #scale_size_identity()+
    #scale_size(range = c(0.1,2.5))+
    coord_cartesian(xlim = c(0,110))+
    guides(size = "none") + 
    labs(tag = 'C') +
    #theme_tree2()+
    theme_MB + 
    theme(  legend.position="none",
            plot.tag.position=c(0.06,.97),
            panel.border = element_blank()
            #plot.background = element_rect(fill = "transparent",
                                  #  colour = NA_character_)
        #legend.title = element_text(face = "bold", hjust = 0.5),
        #plot.margin = unit(c(0,0, 0, 0), "cm"),
        #plot.tag = element_text(size = 9)    
    )

p_g = p

# add genus lines
for (j in images$genus) {
    # j = 'Arenaria'
    ij <- images[genus == j]
    # p_l <- p_l + geom_cladelabel(node = cj$Node, label = cj$Label, color = c(cj$col, "black"), align = TRUE, barsize = 1.5)
    p_g <-
        p_g +
        ggtree::geom_cladelabel(node = ij$node, label = ij$name, color = c(ij$col), barsize = 1, offset = 34.5,fontsize = font_size) # angle = "auto")#
    # ggtree::geom_cladelab(node = c_s$Node, label = c_s$Label, barcolor = c_s$col, textcolor = sub_t, align = TRUE, barsize = 2, hjust = "left", offset.text = 6)
    # ggsave(here::here(glue('Output/temp_phylo_lader_{j}.png')))
    # print(j)
}

#p_g

# use this to add horizontal lines
#p_g = p_g + geom_tiplab(aes(subset = (node %in% c(1)), label = ""), offset = 27, color = "lightgrey", align = TRUE, linetype = 1, vjust = 1, linesize = font_size) # treeid[treeid$label == 'xenicus_gilviventris','label']


# add scale 
#qn <- scales::rescale(quantile(ds$r), probs = seq(0, 1, length.out = length(cols_f1)))

dens <- density(ds$r, n = 2^12)
den <- data.table(x = dens$x, y = dens$y)
#den <- den[x > log10(0.99) & x < log10(50.01)]

f1c_l <-
    ggplot() +
    geom_segment(data = den, aes(x, y, xend = x, yend = 0, colour = x)) +
    scale_color_gradientn(
        colours = cols_f1, # viridis(10),
        limits = lims_tip_f1, oob = scales::squish #values = qn # c(0, seq(qn01[1], qn01[2], length.out = 18), 1)
    ) +
    geom_vline(xintercept = median(ds$r), lty =3, linewidth = 0.5, color = green_)+
    #geom_line(data = den_o, aes(x = x, y = y), color = osc) +
    #geom_line(data = den_s, aes(x = x, y = y), color = sub) +
    # geom_segment(data = den_s, aes(x, y, xend = x, yend = 0)) +
    # geom_segment(data = den_s, aes(x, y, xend = x, yend = 0)) +
    # ggplot() +
    # geom_density(data = d, aes(x = log10(element_types_extrapol_mean), col = clade))+
    # geom_density(data = d, aes(x = log10(element_types_extrapol_mean)))
    # geom_line(data = den_o, aes(x =x, y = y), color = osc) +
    # geom_line(data = den_s, aes(x =x, y = y), color = sub) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c('0','0.5','1')) +
    scale_y_continuous(expand = c(0,0)) +
    ylab("") +
    xlab("Pearson's r\n [for ♀ & ♂ median nest bout]") +
    theme_bw() +
    theme(
        text = element_text(family = fam),
        legend.position = "none",
        axis.line.x = element_line(color = ax_lines, linewidth = 0.25),
        panel.grid.major = element_blank(), # panel.grid.major = element_line(size = 0.25),
        panel.grid.minor = element_blank(), # element_line(size = 0.25),
        # panel.border = element_rect(size=0.25),
        panel.border = element_blank(),
        # axis.line.x.bottom = element_line(color = ax_lines, size = 0.25),
        # axis.line.y.left   = element_line(color = ax_lines, size = 0.25),
        axis.ticks.length = unit(1, "pt"), # axis.ticks.length=unit(.05, "cm"),
        axis.ticks = element_line(linewidth = 0.25, color = ax_lines),
        # plot.tag.position = c(0.96, 0.96),
        # plot.tag = element_text(size = 7.5), # size = 10
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7, colour="grey30"),
        axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA)
    )

# merge
f1c = p_g + theme(legend.position = "none") + inset_element(f1c_l, 
left = 0.10, right = 0.30,
bottom = 0.03, top = 0.308, 
on_top = TRUE, align_to = "full")

# export ggsave(here::here("Output/Fig_1c.png"), f1c, width = 11, height = 10, units ='cm')

# combine f1a, f1b, f1c with grid arrange
blank = ggplot() + theme_void() 
f1b_b = ggarrange(
    blank, f1b,blank,
    nrow=3, heights=c(0.4,6.7,2.9), align = 'v'
    )
f1bc = ggarrange(
    f1b_b,blank, f1c, blank,
    ncol=4, widths=c(6.6,1.4,11, 1), align = 'h'
    )
f1abc = ggarrange(
    f1a_,f1bc,
    nrow=2, heights=c(9.5,10), align = 'v'
    )

if (save_plot == TRUE) {
ggsave(here::here("Output/Fig_1_width-180mm.png"), f1abc, width = 20, height = 19.7, units ='cm', bg='white')
x <- image_read(here::here("Output/Fig_1_width-180mm.png"), density=300)
y <- image_trim(x) # width = 94.5cm, height = 88 (conversion factor 1181/100 )
image_write(y, path = "Output/Fig_1_width-178mm_trimmed.png", format = "png", density = 300)
} 

# prepare for Fig. 1 legend 
  # get residuals
   m = lm(r_sp~1, a) #summary(m)
   a[, res := resid(m)]

  # brms
   dont_run = TRUE # to save time, loads the model outputs and skips the model output generating scripts
   if(dont_run){load(here::here('Data/Fig_1_brms.Rdata'))}else{
    treei_a <- treei
    A_matrix = vcv.phylo(treei_a, corr = TRUE)

    # without phylo
    priors <- get_prior(res ~ 0 + Intercept, data = a) #
    m_no = brm(
      form = res ~ 0 + Intercept,
      data = a,
      cores = 2,
      chains = 5,
      control = list(adapt_delta = 0.999),
      iter = 5000,
      thin = 5,
      sample_prior = "yes",
      save_pars = save_pars(all = TRUE),
      prior = priors,
      seed = 5
    )
    plot(m_no, ask = FALSE)
    pp_check(m_no, ndraws = 100)
    mcmc_plot(m_no, type = "acf")
    summary(m_no)

    # with phylo
    priors_yes <- get_prior(res ~ 0 + Intercept  + (1 | gr(scinam, cov = A)), data = a, data2 = list(A = A_matrix))
    m_yes = brm(
      form = res ~ 0 + Intercept + (1 | gr(scinam, cov = A)),
      data = a,
      data2 = list(A = A_matrix),
      cores = 2,
      chains = 5,
      control = list(adapt_delta = 0.9995),
      iter = 5000,
      thin = 5,
      sample_prior = "yes",
      save_pars = save_pars(all = TRUE),
      prior = priors_yes,
      seed = 5
    )

    plot(m_yes, ask = FALSE)
    pp_check(m_yes, ndraws = 100)
    pairs(m_yes)
    mcmc_plot(m_yes, type = "acf")
    summary(m_yes)
  }

  # lambda
  # hypothesis(m_yes, "sd_scinam__Intercept^2 / (sd_scinam__Intercept^2 + sigma^2) = 0", class = NULL)

  v_sc <- (VarCorr(m_yes, summary = FALSE)$scinam$sd)^2
  v_r <- (VarCorr(m_yes, summary = FALSE)$residual$sd)^2
  #summary(as.mcmc(v_sc / (v_sc  + v_r)))

  # compare  models
  # 1. LOOic
  loo_no <- loo(m_no, moment_match = TRUE)
  loo_yes <- loo(m_yes, moment_match = TRUE)
  #loo_compare(loo_no, loo_yes)

  # 2. Bayes factor
  invisible(capture.output({bf <- as.numeric(bayes_factor(m_no, m_yes))}))
  #round(as.numeric(bayes_factor(m_no, m_yes, quiet = TRUE)),1)

  # 3. Poterior probability
  invisible(capture.output({post_prob <- as.numeric(post_prob(m_yes, m_no)[2])})) 

f1abc

#' <a name="F_1">
#' **Figure 1</a> | Assortative mating for incubation bout lengths across shorebirds.** **A**,**B**. Correlations between median male and female incubation bouts for `r length(a$scinam)` species with >10 nests (n = `r dd_n10[n_by_sp>10, length(unique(pk_nest))]` nest) each with ≥5 female-male incubation bout pairs (n = `r sum(a$n)` bouts). Circles represent female-male median bouts for each nest, their size number of female-male bout pairs. Lines represent robust regressions weighted by sample size (65), i.e. number of female-male bout pairs. Thick lines indicate statistically clear regressions, thin lines the unclear ones (45). Colour indicates suborder (blue Charadrii, orange Scolopaci). Red dotted lines indicate the same median incubation bout of the sexes. **A**. r represents Pearson’s correlation coefficient. Panels represent species, the small insets populations specific regressions for the three populations with >10 nests. For an alternative plot with species-specific axis ranges see Fig. [S1](#f_s1). Note that **B** summarises the species regressions from **A**. **C**. Observed and reconstructed Pearson’s correlations in female and male incubation bouts (assortative mating) visualised by colour on the evolutionary tree (Revell [2013](https://doi.org/10.1111/2041-210X.12066)). The legend depicts the density of the correlation coefficients, its dotted line the median. Note, results were consistent when weighting by number of nests per species (`r round(wtd.quantile(a$r_sp, a$n_by_sp, probs = 0.5),2)`, 95% percentiles: `r round(wtd.quantile(a$r_sp, a$n_by_sp, probs = 0.025),2)`-`r round(wtd.quantile(a$r_sp, a$n_by_sp, probs = 0.975),2)`; mean `r round(wtd.mean(a$r_sp, a$n_by_sp),2)`). A linear model estimated a similar mean (0.64), and its residuals showed no strong phylogenetic signal; the model without phylogeny fitted the residuals better than the model with phylogeny (Bayes factor of `r round(bf,1)[1]` in favour of the non-phylogenetic model; posterior probability of the non-phylogenetic model = `r round(post_prob,2)`).
#' 
#' 
#' <br> 
#' <br> 
#' 
#' ### Drivers
#' #### Is assortative mating confounded by data collection?
#' In some species, icubation bout length tend to change over the incubation period, e.g. increasing in length. Thus, if some nests are monitored only at the begining of the incubation period and others toward the end or if nests are monitored for varying number of days, this in itself could create spurious assortment.
#' 
#' #####  Is the median ♂/♀ bout per nest related to number of days a nest was monitored?  
#' 
#+ f_s1, fig.width=20*inch,fig.height=10*inch
xt = dd_n10[!duplicated(pk_nest)]
xt = xt[n_by_sp>10]

# prepare labels
label_xt <- data.table(
  scinam = levels(factor(xt$scinam))[1],
  x = c(27, 29),  # adjust x as needed
  y = c(15.5,15.5),  # adjust y based on your data range
  label = c("♂", "♀"),
  color = c(blue_, red_)
)

f_s1=
ggplot(data = xt) + 
geom_point(aes(x = n_days, y = med_f), fill = red_, alpha = 0.8, col = "white", pch = 21) +
geom_point(aes(x = n_days, y = med_m), fill = blue_, alpha = 0.8, col = "white", pch = 21) +
facet_wrap(~scinam, ncol = 6) + 
geom_text(data = label_xt, aes(x = x, y = y, label = label, color = color),
            show.legend = FALSE, fontface = "bold", size = 4) +
scale_color_identity() + # use actual color values from 'color' column
labs(x = "Days recorded", y = "Bout length [h]")+
theme_MB
ggsave(file = here::here("Output/Fig_S1_width-180mm.png"), f_s1, width = 20, height = 10, units = "cm")

f_s1

#' <a name="F_S1">
#' **Figure S1</a> | Median incubation bout length in relation to the number of recorded days.** Panels represent species, dots median incubation bout lengths per nest and their colour the sex (female in red and male in blue).  **<span style="color:red">Reassuringly, with exception of *Arenaria interpres*, median ♀/♂ bout per nest are unrelated to number of days a nest was monitored.</span>**  
#' 
#' ### Is the median ♂/♀ bout per nest confounded by when within the incubation period the data were collected?
#' 
#+ f_s2, fig.width=20*inch,fig.height=10*inch
xt = dd_n10[!duplicated(pk_nest)]
xt = xt[n_by_sp>10]

label_xt2 <- data.table(
  scinam = levels(factor(xt$scinam))[1],
  x = c(25, 27),  # adjust x as needed
  y = c(15.5,15.5),  # adjust y based on your data range
  label = c("♂", "♀"),
  color = c(blue_, red_)
)

f_s2 = 
ggplot(data = xt) + 
geom_point(aes(x = med_bout_start_j, y = med_f), fill = red_, alpha = 0.9, col = "white", pch = 21) +
#stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_f), col = 'red')+
geom_point(aes(x = med_bout_start_j, y = med_m), fill = blue_, alpha = 0.9, col = "white", pch = 21) +
#stat_smooth(method = "lm", se = FALSE, aes(x = med_bout_start_j, y = med_m), col = 'blue')+
facet_wrap(~scinam, ncol = 6) + 
geom_text(data = label_xt2, aes(x = x, y = y, label = label, color = color),
            show.legend = FALSE, fontface = "bold", size = 4) +
scale_color_identity() + # use actual color values from 'color' column
labs(x = "Median incubation period of the recorded bouts", y = "Bout length [h]") +
theme_MB

ggsave(file = here::here("Output/Fig_S2_width-180mm.png"), f_s2, width = 20, height = 10, units = "cm")

f_s2

#' <a name="F_S2">
#' **Figure S2</a> | Median incubation bout length in relation to the median incubation period within which the incubation bouts were recorded.** Panels represent species, dots individual data points, their colour highlights sex: female in red and male in blue. **<span style="color:red">In some species the time within the incubation, when bouts were collected might bias the bout lengths, however such bias has only small effect on assortative mating (Fig. [S3](#F_S3)).</span>**  
#' 
#' 
#+ f_s3, fig.width=8*inch,fig.height=8*inch
xt = dd_n10[!duplicated(pk_nest)]
xt = xt[n_by_sp>10]

p = foreach(i = unique(xt$scinam), .combine = rbind) %do% {
  # i = 'Calidris pusilla'
  xti = xt[scinam==i]  
  mi = lm(scale(med_f)~scale(med_m), xti)
  t_mi =m_out(mi, paste(i, "simple"), dep = 'med_f', save_sim = FALSE, type = "lm")
  t_mi[,scinam := i]
  t_mi[, model :='simple']
  t_mi[, N :=N[1]]
  
  mji = lm(scale(med_f)~scale(med_bout_start_j) + scale(med_m), xti)
  t_mji =m_out(mji, paste(i, "inc_per"), dep = 'med_f', save_sim = FALSE, type = "lm")
  t_mji[,scinam := i]
  t_mji[, model :='inc_per']
  t_mji[, N :=N[1]]
  
  t_mji[, estimate_s:=c(t_mi$estimate_r[1],NA,t_mi$estimate_r[2])]
  t_mji[, lwr_s:=c(t_mi$lwr_r[1],NA,t_mi$lwr_r[2])]
  t_mji[, upr_s:=c(t_mi$upr_r[1],NA,t_mi$upr_r[2])]

  return(t_mji)
}

p_xt = p[effect%in%'scale(med_m)']
#as.numeric(round(quantile(p_xt$estimate_s,probs = c(0.025,0.5,.975)),2)); mean(p_xt$estimate_s) 
#as.numeric(round(quantile(p_xt$estimate_r,probs = c(0.025,0.5,.975)),2)); mean(p_xt$estimate_r) 

f_s3 = 
ggplot(p_xt, aes(x = estimate_s, y = estimate_r)) +  
  geom_point(aes(size = as.numeric(N)), pch = 21) +
  #geom_errorbar(aes(ymin = lwr_r, ymax = upr_r), width = 0.1) +  # Error bars for y-axis
  #geom_errorbarh(aes(xmin = lwr_s, xmax = upr_s), height = 0.1)  # Error bars for x-axis
  geom_abline(intercept = 0, slope = 1, lty = 3, col = "red") +
  scale_size(name = "# of nests", limits = c(10, 124), breaks = c(10, 60, 120)) +
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Assortative mating\n", y = "Assortative mating\ncontrolled for incubation period")+
  theme_MB
  ggsave(file = here::here("Output/Fig_S3_width-72mm_test.png"), f_s3, width = 8, height = 6.5, units = "cm")

f_s3

#'  
#' <a name="F_S3">
#' **Figure S3</a> | Comparison of raw assortative mating estimates with those controlled for incubation period.** Circles represent individual species, circle size indicates number of nests. Assortative mating estimates on x-axis come species-specific linear models with z-transformed female incubation bout as a response and z-transformed male incubation bout as a predictor, on y-axis the estimates come from models controlled for median incubation period within which were the bouts of a given nest recorded. Red dotted line marks equal assortative mating estimates. **<span style="color:red">Reassuringly, the estimates are similar</span>** (from simple model: median = `r as.numeric(round(quantile(p_xt$estimate_s,probs = 0.5),2))`, mean = `r round(mean(p_xt$estimate_s),2)`, 95% percentile: `r as.numeric(round(quantile(p_xt$estimate_s,probs = 0.025),2))`-`r as.numeric(round(quantile(p_xt$estimate_s,probs = 0.975),2))`; from controlled model: median = `r as.numeric(round(quantile(p_xt$estimate_r,probs = 0.5),2))`, mean = `r round(mean(p_xt$estimate_r),2)`, 95% percentile: `r as.numeric(round(quantile(p_xt$estimate_r,probs = 0.025),2))`-`r as.numeric(round(quantile(p_xt$estimate_r,probs = 0.975),2))` )  
#' 
#' 
#'***
#' 

#' #### Not body size
#' # prepare data
    ws = dd_n[!(is.na(wing_f) | is.na(wing_m))]
    ws = ws[!duplicated(pk_nest)]
    ws[, n_by_sp:= .N, scinam]
    ws_sp5 = ws[n_by_sp>5]

    ws_sp5[,  r_sp := cor(wing_f, wing_m), by = scinam]
    ws_sp5[,  r_sp_p := cor.test(wing_f, wing_m)$p.value, by = scinam]
    ws_sp5[r_sp_p<0.01, r_sp_p_certain := 'Certain']
    ws_sp5[is.na(r_sp_p_certain), r_sp_p_certain := 'Uncertain']

     #round(quantile(ws_sp5[!duplicated(scinam), r_sp], probs = c(0.025, 0.5, 0.975)),2); ws_sp5[!duplicated(scinam), mean(r_sp)]

    ws_sp5[, r_bout := cor(med_f, med_m), by = scinam]

    ws_sp5[, slope_sp := rlm(wing_f ~ wing_m, maxit = 200)  %>% coef  %>% magrittr::extract(2), by = scinam] 
    ws_sp5[, slope_sp_certain := simulate_rlm_no_weights_w(.SD), by = scinam]

    #ws[, n_by_pop := .N, pop]#; ws[n_by_pop>10, length(unique(pop))]
    #ws_pop5 = ws[n_by_pop>5]#

    #ws_pop5[, r_bout := cor(med_f, med_m), by = pop]
    #ws_pop5[, r_pop := cor(wing_f, wing_m), by = pop]
    
    #ggplot(ws_pop5[!duplicated(pop)], aes(x = r_pop)) + geom_histogram()
    #ggplot(ws_sp5[!duplicated(scinam)], aes(x = r_sp)) + geom_histogram()
    #ggplot(ws_pop5[!duplicated(pop)], aes(x = r_pop , y = r)) + geom_point() + stat_smooth(method = 'lm')

#+ f_s4, fig.width=20*inch,fig.height=7*inch   
  # prepare histogram data
    r_table <- ws_sp5[, .(r = cor(wing_m, wing_f, method = "pearson")), by = scinam]
    r_hist_data <- data.table(scinam = "Summary of Pearson's r", r = r_table$r)

  # Compute per-facet max ranges
    xy_lims = rbind(
    ws_sp5[,list(x = min(c(wing_m, wing_f)), y = min(c(wing_m, wing_f))), by = scinam],
    ws_sp5[,list(x = max(c(wing_m, wing_f)), y = max(c(wing_m, wing_f))), by = scinam]
    )
    xy_lims[, x := round(x)]
    xy_lims[, y := round(y)]
  
  p_main =
  ggplot(ws_sp5, aes(x = wing_m, y = wing_f)) +
      # regular data
      geom_blank(data = xy_lims, aes(x = x, y = y)) +
      geom_point(data = ws_sp5, aes(col = suborder), alpha = 0.5) +
      geom_smooth(data = ws_sp5, method = 'rlm', se = FALSE, col = 'grey40',
                  aes(lwd = slope_sp_certain), method.args = list(maxit = 200)) +
      geom_abline(intercept = 0, slope = 1, lty = 3, col = 'red') +
      ggpubr::stat_cor(method="pearson", size = 2, cor.coef.name = 'r',
                      aes(x = wing_m, y = wing_f, label = after_stat(r.label)),
                      inherit.aes = FALSE) +

      facet_wrap(~scinam, ncol = 6, scales = "free") +

      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Statistically clear\nslope")+ 

      scale_x_continuous("♂ wing length [mm]", breaks = function(x) {
        brks <- scales::pretty_breaks(n = 5)(x); brks[brks %% 1 == 0]
      }) +
      scale_y_continuous("♀ wing length [mm]", breaks = function(x) {
        brks <- scales::pretty_breaks(n = 5)(x); brks[brks %% 1 == 0]
      }) +
      #labs(tag = 'A') +
      theme_MB +
      theme(strip.background = element_blank())
  
    p_hist <- ggplot(r_hist_data, aes(x = r)) +
          geom_histogram(bins = 20, fill = "gray70", color = "gray50") + 
          scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1), expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0))+
            geom_vline(xintercept = median(r_hist_data$r), col = green_) +  
            annotate("text", x=0, y=2.5, label= "median", col = green_, size = 2, hjust = 1)+
    labs(x = "Pearson's r", y = "# of nests", subtitle = "Summary") +
    theme_MB +
    theme(plot.subtitle = element_text(size=6, margin = margin(b = 0)),
          axis.title.y = element_text(margin = margin(r = 0.2)) )

  f_body = p_main + 
    theme(
      legend.position = "right",               # keep it on the right
      legend.justification = c(0.5, 1),        # align legend box to the top
      legend.box.just = "top",                 # align content to the top of the box
      legend.margin = margin(t = 0, b = 0),    # reduce vertical padding
      legend.box.margin = margin(t = 5, b = 5) # spacing between legend and plot
    ) + 
    inset_element(p_hist, 
    left = 0.775, right = 0.91,
    bottom = 0.08, top = 0.5, 
    on_top = TRUE, align_to = "full")

  ggsave(file = here::here("Output/Fig_S4_width-180mm.png"), f_body, width = 20, height = 7, units = "cm")

  f_body

#' <a name="F_S4">
#' **Figure S4</a> | Assortative mating for wing length.** Correlations between  male and female wing length for `r length(unique(ws_sp5$scinam))` species with > 5 pairs where wing length of a pair was known (n = `r ws_sp5[, length(unique(pk_nest))]` nests). Circles represent individual observations. Lines represent robust regressions (65). Thick lines indicate statistically clear regressions, thin lines the unclear ones (45). Colour indicates suborder: blue = Charadrii, orange = Scolopaci. Red dotted lines imark equal wing length between sexes. *r* represents Pearson’s correlation coefficient. 
#' 
#+ f_s5, fig.width=7*inch,fig.height=6.8*inch  
 ws_sp5_s = ws_sp5[!duplicated(scinam)]
 #cor.test(ws_sp5_s$r_sp, ws_sp5_s$r_bout)
 f_s5 = 
 ggplot(ws_sp5_s, aes(x = r_sp , y = r_bout)) + 
  stat_smooth(method = 'rlm', col = 'red', lwd = 0.5, bg='grey75') + 
  geom_point(pch=21, bg='grey45') +
  labs(x = 'Assortative mating for wing length', y = 'Assortative mating for incubation bout length') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(xlim = c(-1,1), ylim = c(0,1))+
  theme_MB

 ggsave(file = here::here("Output/Fig_S5_width-72mm.png"), f_s5, width = 7, height = 6.8, units = "cm") 

 f_s5

#' <a name="F_S5">
#' **Figure S5</a> | Assortative mating for incubation bout lengths in relation to assortative mating for wing length.** Each dot represents a species. Red line with grey area depicts robust regression with 95%CI (65).
#' 
#'
#' <br> 
#' 
#' ***
#' ### Within nest bout pair similarity
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
      fm[n_by_nest>=5,  r_p := cor.test(bout_f, bout_m)$p.value, by = pk_nest]
      fm[n_by_nest>=5 & r<0, r_neg := 'yes']
      fm[n_by_nest>=5 & !r_neg%in%'yes', r_neg := 'no']
      fm[r_p<0.01, r_p_certain := 'Certain']
      fm[is.na(r_p_certain), r_p_certain := 'Uncertain']

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
        r_p = unique(r_p),
        r_p_certain = unique(r_p_certain),
        bout_m_min = min(bout_m),
        bout_m_max = max(bout_m),
        bout_f_min = min(bout_f),
        bout_f_max = max(bout_f),
        bout_start_j=median(bout_start_j, na.rm= TRUE)
        ),
        by = list(suborder,genus,animal,sp,scinam,species,year, breeding_site,pop,lat_pop, pop_lat, nest, nn, app, tidal, tidal_pop,pop_wing_f, pop_wing_m, pk_nest, lat, n_by_nest, r_neg,slope_nest_certain)  
      ] # same as fmr = fm[!(is.na(r) | duplicated(paste(r, pk_nest))) ]

    # Offset the text in plots slightly to avoid overlaps
        x_offset <- 0.025  # Adjust as needed
        y_offset <- 0.05 # Adjust as needed


#+ f2,fig.height=20*inch, fig.width=18*inch
  # prepare   
    fm10=fm[n_by_nest>10]

    # descriptive stats 
      #length(unique(fm10$pk_nest)) # n nests in f2a
      #nrow(fm10) # n bouts in f2a
      #length(unique(fm10$scinam)) # n nests in f2a
      #nrow(fm)

    fmr10=fmr[n_by_nest>10] # #nrow(fmr10)
    fmrm = fmr10[r_p_certain%in%'Certain', list(
        r = median(r, na.rm = TRUE),
        bout_m_pos =  min(bout_m_min) + 0.05 * (max(bout_m_max) - min(bout_m_min)),
        bout_f_pos =  max(bout_f_max) + 0.05 * (max(bout_f_max) - min(bout_f_min))),
        by = list(suborder,genus,animal,sp,scinam,species)  
      ] 
    # align the  datasets to be plotted
     fmrm <- fmrm %>%
          mutate(scinam = factor(scinam, levels = unique(fm10$scinam)))


# f2a  
  f2a = 
  ggplot() + 
      geom_smooth(data = fm10, aes(x = bout_m, y = bout_f, group = pk_nest, lwd = slope_nest_certain), method = 'rlm', se = FALSE,  col = 'grey40', alpha = 0.8, method.args = list(maxit = 200))+ #linewidth = size_l,alpha = 0.2, col = slope_nest_neg
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      geom_text(
        data = fmrm, 
        aes(x = 0, y =30, label = paste("italic(r) == ", round(r,2))), 
        hjust = 0, vjust =1, size = 2,  parse = TRUE
          ) +
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope statistically clear")+ 
    
      #scale_size(breaks = c(1,15,30), name = 'n days') +

      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      coord_cartesian(xlim = c(0, 30),ylim = c(0, 30)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      #labs(subtitle = "A")+
      labs(tag = 'A')+
      facet_wrap(~scinam, ncol = 8)+#, scales = "free") + 
      theme_MB + 
      theme(strip.background = element_blank(),
            legend.position = c(0.875, 0.08)
           #panel.background = element_rect(fill = "transparent",
            #                     colour = NA_character_), # necessary to avoid drawing panel outline
    
          #plot.background = element_rect(fill = "transparent",
           #                         colour = NA_character_), # necessary to avoid drawing plot outline
      #legend.background = element_rect(fill = "transparent"),
      #legend.box.background = element_rect(fill = "transparent"),
      #legend.key = element_rect(fill = "transparent")
      )
  #ggsave(file = here::here("Output/Fig_2a_width-180mm_fixed-axis-limits30_v2.png"), f2a_lim, width = 20, height = 11, units = "cm")


# f2b
  fmr10_c = fmr10[r_p_certain %in% 'Certain']
  # in r and suborder specific
  give.n <- function(x){
    return(c(y = 1.1, label = length(x))) 
    # experiment with the multiplier to find the perfect position
  }
  
  # define color for genus
  go=data.frame(genus=c("Limosa","Tringa","Numenius","Calidris","Arenaria",  "Pluvialis","Charadrius",  "Haematopus"),
  cols=c(brewer.pal(11,"Spectral")[1:5],brewer.pal(11,"Spectral")[9:11]), stringsAsFactors=FALSE)
  #scales::show_col(brewer.pal(11,"Spectral"))
  #go=data.frame(genus=c("Arenaria","Calidris","Tringa","Limnodromus", "Limosa","Numenius", "Charadrius", "Vanellus", "Pluvialis","Haematopus"),
  #cols=c(brewer.pal(11,"Spectral")[1:6],brewer.pal(11,"Spectral")[7:8],brewer.pal(11,"Spectral")[10:11]), stringsAsFactors=FALSE)
           
  f2b =   
  ggplot(fmr10_c, aes(y = fct_reorder(scinam, r, .fun = median, .desc =TRUE), x = r, fill = genus)) + 
    geom_boxplot(
      lwd = 0.25,
      outlier.size = 0.25,
      outlier.color = "grey40")+
    stat_summary(fun.data = give.n, geom = "text", size = 1.5, col = "grey30") +
    #scale_x_continuous(lim = c(-1,1))+
    scale_fill_manual(values = go$cols, breaks = go$genus, name = 'Genus')+
    facet_grid(rows = vars(forcats::fct_relevel(suborder, "Scolopaci", "Charadrii")), scales = "free_y",space = "free_y") +
    geom_vline(xintercept = 0, lty = 3)+
    scale_x_continuous("Pearson's correlation coefficient for ♀ & ♂ incubation bout length\n[for nests with statistically clear correlation]", expand = c(0, 0),  breaks = c(-1,-0.5,0,0.5,1)) + 
    coord_cartesian(xlim = c(-1, 1)) + 
    labs(tag = 'B')+
    theme_MB +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      strip.text.y.right = element_text(angle = 90),
      strip.background = element_blank(),
      axis.title.y=element_blank()
        )
    #ggsave(here::here('Output/Fig_2b_89mm_v2_all.png'), f2b, height = 10, width = 8.8, units = 'cm')

# f2c
  fmr10_c$genus <- factor(fmr10_c$genus, levels = go$genus)
  ann_text_f2c <- data.frame(
    r = c(-0.5, 0.5),lab = c("Negative", "Positive"),
    genus = factor('Limosa',levels = go$genus))

  f2c =
   ggplot(fmr10_c, aes(x=r))+#, fill = r_neg)) + 
      geom_rect(xmin = -2, xmax = 0, ymin = -Inf, ymax = Inf,fill = 'grey80',inherit.aes = FALSE)+
      geom_histogram(aes(fill = genus), col = "grey40") + geom_vline(xintercept = 0, lty =3, col = 'black')+
      facet_wrap(~genus, nrow = 4) + 
      geom_text(data = ann_text_f2c,y = 14, label = c("Negative", "Positive"), size = 0.7/scale_size, col = 'grey30')+
      scale_fill_manual(values = go$cols, name = 'Genus')+
      scale_x_continuous("Pearson's correlation coefficient for ♀ & ♂ incubation bout length\n[for nests with statistically clear correlation]", expand = c(0, 0),  breaks = c(-1,-0.5,0,0.5,1)) + #, lim = c(-1,1)
      coord_cartesian(xlim = c(-1, 1)) + 
      scale_y_continuous("Nests [count]", expand = c(0, 0)) +
      #scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      #labs(subtitle = "Based on individual bouts")  +
      labs(tag = 'C')+
      theme_MB + 
      theme(
       strip.background = element_blank(),
       #panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
       panel.spacing.x = unit(1, "lines"),
       plot.margin = margin(l = 30),
       legend.position = "none")

    #ggsave('Output/Fig_2c_v2.png', f2c, height = 10, width = 6.5, units = 'cm')

  # combine and export
    bottom_row <- f2b + f2c + plot_layout(ncol = 2, widths = c(0.9, 0.9), axis_title = "collect")

    fig2 <- free(f2a, side = "l") / bottom_row + 
      plot_layout(heights = c(1, 0.91)) 

    ggsave('Output/Fig_2_width-162mm_v5.png', fig2, height = 20, width = 18, units = 'cm')

    fig2

#' <a name="F_2">
#' **Figure 2</a> | Within-pair correlations in incubation bout lengths across shorebirds.** **A**. Correlations between male and female incubation bouts for `r length(unique(fm10$pk_nest))` nests with >10 female-male incubation bout pairs (n = `r nrow(fm10)` from `r length(unique(fm10$scinam))` species. Lines represent robust regressions (Venables & Ripley [2002](https://link.springer.com/book/10.1007/978-0-387-21706-2)); thick lines indicate statistically clear regressions, thin lines the unclear ones (Dushoff et al. [2019]( https://doi.org/10.1111/2041-210X.13159)). *r* represents the median Pearson’s correlation coefficient per species based only on statistically clear *r* values; *r* is omitted in panels with only unclear *r* coefficients. **B**, **C**. Distribution of statistically clear *r* coefficients from **A** (n = `r nrow(fmr10_c)` nests). Dotted lines indicate no correlation. Box plots are ordered within each suborder from the most negative to the most positive median species correlation, and depict the genus (colour), median (vertical line inside the box), the 25th and 75th percentiles (box), whiskers extending to 1.5 × the interquartile range from the box or to the minimum/maximum value (whichever is smaller), and outliers (circles). See Fig S5, for A with species-specific axis ranges, and B and C including uncertain *r* coefficients.
#' 
#' 
#' <br> 
#'  
#+ f_s6, fig.width=20*inch,fig.height=20*inch
  # f_s6a
  f_s6a= 
  ggplot() + 
      geom_smooth(data = fm10, aes(x = bout_m, y = bout_f, group = pk_nest, lwd = slope_nest_certain), method = 'rlm', se = FALSE,  col = 'grey40', alpha = 0.8, method.args = list(maxit = 200))+ #linewidth = size_l,alpha = 0.2, col = slope_nest_neg
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      facet_wrap(~scinam, ncol = 6, scales = "free") + 
      #v1: geom_text(data = fmrm, aes(x = bout_m_pos, y =bout_f_pos, 
      #label = paste('r =', round(r,2))),
      #hjust = 0, vjust =1, size = 2) +
      geom_text(data = fmrm, aes(x = bout_m_pos-x_offset*abs(bout_m_pos), y =bout_f_pos+y_offset*abs(bout_f_pos), 
      label = paste("italic(r) == ", round(r,2))),
      hjust = 0, vjust =1, size = 2, parse = TRUE) + 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope statistically clear")+ 
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      #labs(subtitle = "A")+
      labs(tag = 'A')+
      theme_MB + 
      theme(
        strip.background = element_blank(),
        legend.position = c(0.93, 0.08)
      )
  #ggsave(file = here::here("Output/Fig_S6a_free.png"), fs1, width = 20*0.9, height = 16*0.9, units = "cm")       
  
  # f_s6b
   # Compute median 'r' for scinams with 'Certain' values 
    med_r_certain <- fmr10 %>%
    filter(r_p_certain == "Certain") %>%
    group_by(scinam) %>%
    summarize(med_r = median(r, na.rm = TRUE)) %>%
    ungroup()

  # Join the medians back into full data
   fmr10_f_s6b <- fmr10 %>%
    left_join(med_r_certain, by = "scinam") %>%
    mutate(
      med_r = ifelse(is.na(med_r), Inf, med_r),  # Put "uncertain-only" at the bottom
      scinam = fct_reorder(scinam, med_r, .desc = TRUE)
    )
  
  go2=data.frame(genus=c("Limosa","Tringa","Numenius","Calidris","Arenaria","Limnodromus",  "Pluvialis", "Charadrius","Haematopus", "Vanellus"),
  cols=c(brewer.pal(11,"Spectral")[1:6],brewer.pal(11,"Spectral")[7:8],brewer.pal(11,"Spectral")[10:11]), stringsAsFactors=FALSE)

  f_s6b =   
  ggplot(data = fmr10_f_s6b, aes(y = scinam, x = r, fill = genus)) +
    geom_boxplot(
      aes(alpha = r_p_certain),     
      lwd = 0.25,
      outlier.size = 0.25,
      outlier.color = "grey40")+
    stat_summary(aes(group = r_p_certain), fun.data = give.n, geom = "text", size = 1.5, col = "grey30") +
    #scale_x_continuous(lim = c(-1,1))+
    scale_fill_manual(values = go2$cols, breaks = go2$genus, name = 'Genus')+
    scale_alpha_manual(values = c(0.5,1), breaks = c('Uncertain', 'Certain'), guide = "none")+
    facet_grid(rows = vars(forcats::fct_relevel(suborder, "Scolopaci", "Charadrii")), 
    cols = vars(r_p_certain), scales = "free_y",space = "free_y") +
    geom_vline(xintercept = 0, lty = 3)+
    xlab("Pearson's correlation coefficient for ♀ & ♂ incubation bout length") +
    labs(tag = 'B')+
    theme_MB +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      strip.text.y.right = element_text(angle = 90),
      strip.background = element_blank(),
      axis.title.y=element_blank()
        )
  #ggsave(here::here('Output/Fig_S6b_width-118mm_v2_all_facets.png'), f_s4a, height = 10, width = 8.8*1.5, units = 'cm')

  fmr10_f_s6b$genus <- factor(fmr10_f_s6b$genus, levels = go2$genus)
  ann_text_f_s6c <- data.frame(
    r = c(-0.5, 0.5),lab = c("Negative", "Positive"),
    genus = factor('Limosa',levels = go2$genus))

  f_s6c =
   ggplot(fmr10_f_s6b, aes(x=r))+#, fill = r_neg)) + 
      geom_rect(xmin = -2, xmax = 0, ymin = -Inf, ymax = Inf,fill = 'grey80',inherit.aes = FALSE)+
      #geom_histogram(aes(fill = genus), col = "grey40") + 
      geom_histogram(
        aes(fill = genus, alpha = r_p_certain),
        col = "grey40",
        position = "identity"  # to overlay the two groups!
      ) +
      scale_alpha_manual(
        values = c(Uncertain = 0.5, Certain = 1),
        name = "Correlation statistically"
        ) +

      geom_vline(xintercept = 0, lty =3, col = 'black')+
      facet_wrap(~genus, nrow = 5) + 
      geom_text(data = ann_text_f_s6c,y = 14, label = c("Negative", "Positive"), size = 0.7/scale_size, col = 'grey30')+
      scale_fill_manual(values = go2$cols, name = 'Genus')+
      scale_x_continuous("Pearson's correlation coefficient for ♀ & ♂ incubation bout length", expand = c(0, 0)) +
      scale_y_continuous("Nests [count]", expand = c(0, 0)) +
      #scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      #labs(subtitle = "Based on individual bouts")  +
      labs(tag = 'C')+
      theme(
      panel.spacing.x = unit(1, "lines"),
      plot.margin = margin(l = 15),
      legend.position = "none") +
      theme_MB

  # combine
    bottom_row_2 <- f_s6b + f_s6c + plot_layout(ncol = 2, widths = c(1, 0.8), axis_title = "collect")

    fig_s6 <- free(f_s6a, side = "l") / bottom_row_2 + 
      plot_layout(heights = c(1, 0.91)) 

    ggsave('Output/Fig_S6_width-162mm_v5.png', fig_s6, height = 20, width = 18, units = 'cm')

    fig_s6

#' <a name="F_S6">
#' **Figure S6</a> | Within-pair correlations in incubation bout lengths across shorebirds.** **A**. Correlations between male and female incubation bouts for `r length(unique(fm10$pk_nest))` nests with >10 female-male incubation bout pairs (n = `r nrow(fm10)` from `r length(unique(fm10$scinam))` species. Lines represent robust regressions (Venables & Ripley [2002](https://link.springer.com/book/10.1007/978-0-387-21706-2)); thick lines indicate statistically clear regressions, thin lines the unclear ones (Dushoff et al. [2019]( https://doi.org/10.1111/2041-210X.13159)). *r* represents the median Pearson’s correlation coefficient per species based only on statistically clear *r* values; *r* is omitted in panels with only unclear *r* coefficients. **B**, **C**. Distribution of *r* coefficients from **A** (n = `r nrow(fmr10_c)` nests). Dotted lines indicate no correlation. Color depicts genus and color intensity statistically clear (darker) and unclear (lighter) *r* coefficients. Box plots are ordered within each suborder from the most negative to the most positive median species correlation, and depict the genus (colour), median (vertical line inside the box), the 25th and 75th percentiles (box), whiskers extending to 1.5 × the interquartile range from the box or to the minimum/maximum value (whichever is smaller), and outliers (circles).
#' 
#' 
#+ f_s7, fig.width=10*inch,fig.height=10*inch
# TODO:check the densityplot
# prepare colors
cols_f1 <- rev(c(brewer.pal(11, "Spectral")[1], brewer.pal(11, "Spectral")[4], brewer.pal(11, "Spectral")[7:11]))

# prepare data and tree
ds2 =  fmr10[,list(r = median(r, na.rm = TRUE)), by = list(suborder,genus,animal,sp,scinam,species)] # TODO:delete this comment: initially was fmr12 dataset
ds2 = ds2[!is.na(r)]
ds2[, scinam:=sub("_", " ", animal)]

# unify limits for color plotting
lims_tip <- range(ds2$r, na.rm = TRUE)

# ladderize
if (ladderize_ == FALSE) {
    treei2 <- drop.tip(tree, setdiff(tree$tip.label, ds2$scinam))
} else {
   treei2 <- drop.tip(tree, setdiff(tree$tip.label, ds2$scinam)) %>% ladderize(right =TRUE)
}

# reconstrunct ancestral state using phytools
colelab2 <- ds2$r
names(colelab2) <- ds2$scinam
fit2 <- phytools::fastAnc(treei2, colelab2, vars = FALSE, CI = FALSE)
nd2 <- data.table(node = names(fit2), trait = as.numeric(fit2)) 
td2<- data.table(node = ggtree::nodeid(treei2, names(colelab2)), trait = colelab2)
ptr2 <- rbind(td2, nd2)
ptr2[, node := as.numeric(node)]
treei_c2 <- dplyr::full_join(treei2, ptr2, by = "node")

# prepare phylogenetic contrasts
r_pear2=ds2$r
names(r_pear2)=ds2$scinam
yourPics2 <- pic(x=r_pear2, phy=treei2)

contrast_data2 <- data.table(
  node = (Ntip(treei2) + 1):(Ntip(treei2) + Nnode(treei2)),
  pic = yourPics2
)

treei_c2 <- treei_c2 %>%
  left_join(contrast_data2, by = "node")

# prepare genera images
images2 = data.table(image = list.files(
    path = here::here("Illustrations/for_tree2/"), 
    pattern = "\\.png$", full.names = TRUE),
    genus = sub("\\_.*", "", list.files(path = "Illustrations/for_tree2/", pattern = "\\.png$", 
    full.names = FALSE)),
    genus_y = c(6.5, 3.5, 20, 24.6, 11.5, 14,17,27.5, 9, 22.8),
    genus_x = c(105,105,105,110,110,105,105,105,105,105),
    col = c("lightgrey","lightgrey","lightgrey", "lightgrey", "darkgrey", "lightgrey", "darkgrey", "darkgrey", "lightgrey", "darkgrey"),
    width_tree = c(0.9, 0.88, 0.83, 1.2, 0.9, 1.1, 1.12, 0.75,0.87, 0.88),
    #width_tree = c(0.9, 0.85, 0.8, 1.2, 1.1, 1.08,0.9,0.91),
    #width_tree = c(0.9, 0.72, 0.6, 1.67, 1.52, 1.6,0.8,1.07),
    bird_size = c(23.5, 19, 15.5,43.75, 29, 39.75, 42, 26, 28, 30)
    )

images2$width = image_info(image_read(images2$image))$width 
images2$height = image_info(image_read(images2$image))$height 

# add node indentifiers for vertical genus bars 
treeid2 <- data.table(as_tibble(tidytree::as.treedata(treei2)))
treeid2[, genus:=sub("\\ .*", "", label)]
nod2 = treeid2[, min(parent), genus] %>% setnames(c('V1'),c('node'))
images2 = merge(images2, nod2)
images2[, name := NA]
images2[genus=='Vanellus', node := 25]
images2[genus=='Arenaria', node := 7]

#default_size <- ggplot2:::check_subclass("point", "Geom")$default_aes$size

# plot tree
p2 <- ggtree::ggtree(treei_c2, ladderize = ladderize_, right = TRUE) + #layout = "circular", 
    geom_tree(aes(color = trait), continuous = "colour", size = 1) +
    geom_tiplab(offset = 0.5, fontface = "italic", colour = "grey30", size = 2.35)+
    scale_color_gradientn(colours = (cols_f1), limits = lims_tip, oob = scales::squish, name = "Assortative mating") +
    geom_image(data = images2, 
             aes(
                x = genus_x, y = genus_y, image = image, size = I(width_tree/10)), by='width')+#, size = 0.1) +#inherit.aes = FALSE) +  # Adjust x and size as needed #, by = "width" 
    geom_point(data = data.frame(x = 97.5, y = c(7)), aes(x =x, y = y), color = "darkgrey", shape = 15, size = 1) +
    geom_point(data = data.frame(x = 97.5, y = c(23)), aes(x =x, y = y), color = "darkgrey", shape = 15, size = 1) +
    #geom_point(
    #        aes(x = x, y = y, size = abs(pic)), #sqrt(abs(pic/pi))),
    #        fill = "grey90", color = "grey50", pch = 21) +
    #scale_size_identity()+
    #scale_size(range = c(0.1,2.5))+
    coord_cartesian(xlim = c(0,110))+
    guides(size = "none") + 
    #labs(tag = 'D') +
    #theme_tree2()+
    theme_MB + 
    theme(  #legend.position="none",
            plot.tag.position=c(0.06,.97),
            panel.border = element_blank()
            #plot.background = element_rect(fill = "transparent",
                                  #  colour = NA_character_)
        #legend.title = element_text(face = "bold", hjust = 0.5),
        #plot.margin = unit(c(0,0, 0, 0), "cm"),
        #plot.tag = element_text(size = 9)    
    )

p_g2 = p2

# add genus lines
for (j in images2$genus) {
    # j = 'Charadrius'
    ij2 <- images2[genus == j]
    # p_l <- p_l + geom_cladelabel(node = cj$Node, label = cj$Label, color = c(cj$col, "black"), align = TRUE, barsize = 1.5)
    p_g2 <-
        p_g2 +
        ggtree::geom_cladelabel(node = ij2$node, label = ij2$name, color = c(ij2$col), barsize = 1, offset = 34.5,fontsize = font_size) # angle = "auto")#
    # ggtree::geom_cladelab(node = c_s$Node, label = c_s$Label, barcolor = c_s$col, textcolor = sub_t, align = TRUE, barsize = 2, hjust = "left", offset.text = 6)
    # ggsave(here::here(glue('Output/temp_phylo_lader_{j}.png')))
    # print(j)
}

#p_g

# use this to add horizontal lines
#p_g = p_g + geom_tiplab(aes(subset = (node %in% c(1)), label = ""), offset = 27, color = "lightgrey", align = TRUE, linetype = 1, vjust = 1, linesize = font_size) # treeid[treeid$label == 'xenicus_gilviventris','label']

# add scale 
#qn2 <- scales::rescale(quantile(ds2$r), probs = seq(0, 1, length.out = length(cols_f1)))

dens2 <- density(ds2$r, n = 2^12)
den2 <- data.table(x = dens2$x, y = dens2$y)
#den <- den[x > log10(0.99) & x < log10(50.01)]

f_den <-
    ggplot() +
    geom_segment(data = den2, aes(x, y, xend = x, yend = 0, colour = x)) +
    scale_color_gradientn(
        colours = cols_f1, # viridis(10),
        limits = lims_tip, oob = scales::squish # c(0, seq(qn01[1], qn01[2], length.out = 18), 1)
    ) +
    geom_vline(xintercept = median(ds2$r), lty =3, linewidth = 0.5, color = 'red')+
    #geom_line(data = den_o, aes(x = x, y = y), color = osc) +
    #geom_line(data = den_s, aes(x = x, y = y), color = sub) +
    # geom_segment(data = den_s, aes(x, y, xend = x, yend = 0)) +
    # geom_segment(data = den_s, aes(x, y, xend = x, yend = 0)) +
    # ggplot() +
    # geom_density(data = d, aes(x = log10(element_types_extrapol_mean), col = clade))+
    # geom_density(data = d, aes(x = log10(element_types_extrapol_mean)))
    # geom_line(data = den_o, aes(x =x, y = y), color = osc) +
    # geom_line(data = den_s, aes(x =x, y = y), color = sub) +
    scale_x_continuous(breaks = seq(-1,1, by = 0.5), labels = seq(-1,1, by = 0.5)) +
    scale_y_continuous(expand = c(0,0)) +
    ylab("") +
    xlab("Median Pearson's r\n [for within nest ♀ & ♂ bouts]") +
    theme_bw() +
    theme(
        text = element_text(family = fam),
        legend.position = "none",
        axis.line.x = element_line(color = ax_lines, linewidth = 0.25),
        panel.grid.major = element_blank(), # panel.grid.major = element_line(size = 0.25),
        panel.grid.minor = element_blank(), # element_line(size = 0.25),
        # panel.border = element_rect(size=0.25),
        panel.border = element_blank(),
        # axis.line.x.bottom = element_line(color = ax_lines, size = 0.25),
        # axis.line.y.left   = element_line(color = ax_lines, size = 0.25),
        axis.ticks.length = unit(1, "pt"), # axis.ticks.length=unit(.05, "cm"),
        axis.ticks = element_line(linewidth = 0.25, color = ax_lines),
        # plot.tag.position = c(0.96, 0.96),
        # plot.tag = element_text(size = 7.5), # size = 10
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7, colour="grey30"),
        axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA)
    )

# merge and export
fx = p_g2 + theme(legend.position = "none") + inset_element(f_den, 
left = 0.10, right = 0.30,
bottom = 0.03, top = 0.308, 
on_top = TRUE, align_to = "full")

fx; #ggsave(here::here("Output/Fig_S7_v2.png"), fx, width = 11, height = 10, units ='cm')


#' <a name="F_S7">
#' **Figure S7</a> | Observed and reconstructed within-pair Pearson’s correlations in female and male incubation bouts visualised on the evolutionary tree (66).** Node and tip colours indicate correlation strength; the density plot shows the distribution of the median correlation coefficients (median’s per species based on nests with >10 female-male incubation bout pairs), with the dotted line highlighting their median.

#' ### Are within-nests correlations confounded by cahnging bout lengths over incubation period?#' 
#'
#' #### Does mean r change if controlled for incubation period?
#' <a name="T_S1a">
#' **Table S1a | Within-pair correlations in incubation bout lengths**</a>
# use only statistically clear r values (p<0.01)
v = fmr[n_by_nest>10 & r_p_certain%in%'Certain'] #nrow(fmr[n_by_nest>15 & slope_nest_certain%in%'yes'])
#nrow(v)
#summary(v$r)

# r vs r controlled for mean/median incubation period - no difference
mi_a = lmer(r~1+(1|genus) + (1|species) + (1|lat_pop), data = v)
table_s1a =m_out(mi_a, "a", dep = 'r', save_sim = FALSE, R2 = FALSE)

mi_b = lmer(r~1+ scale(bout_start_j) + (1|genus) + (1|species) + (1|lat_pop), data = v)
table_s1b =m_out(mi_b, "b", dep = 'r', save_sim = FALSE, R2 = FALSE)

mi_c = lmer(r~1+ scale(bout_start_j) + (1|genus) + (scale(bout_start_j)|species) + (1|lat_pop), data = v)
table_s1c =m_out(mi_c, "c", dep = 'r', save_sim = FALSE, R2 = FALSE)

table_s1=rbind(table_s1a, table_s1b, table_s1c)

table_s1[, `95%CI` := paste(lwr_r, upr_r, sep ="-")]
setnames(table_s1, old = c("estimate_r"), new = c("estimate"))
#table_s1[is.na(table_s1)] <- ""
table_s1$lwr_r = table_s1$upr_r = NULL
table_s1 %>%
  kbl() %>%
  kable_paper("hover", full_width = F)  %>%
  column_spec(ncol(table_s1), extra_css = "text-align: center;") %>%
  #column_spec(ncol(table_s1), extra_css = "font-family: monospace; text-align: center;") %>%
  scroll_box(width = "100%", height = "650px")

#' <span style="color: grey;">Mean assortative mating (*r*) is same in the intecept only model (a), model controlled for median incubation period of the recorded bouts (b) and model with random slope of  median incubation period of the recorded bouts (c). The estimates and 95% credible intervals come from the joint posterior distribution of 5000 simulated values generated by the *sim* function from the *arm* R-package [@gelman2022a].</span>
#' 

# end