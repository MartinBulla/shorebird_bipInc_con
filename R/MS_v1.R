#' ---
#' title: "Draft of "Striking consistency in parental care of the sexes across shorebird evolutionary history""
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
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)

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
    d = data.table(d)

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
      n_days=as.numeric(difftime(max(datetime_off),min(datetime_on),days))),
      by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop, lat_pop, pop_lat, year,nest, nn,pk_nest, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_)
      ]
  
    # limit data
      dd_n=dd_n[which(!is.na(dd_n$med_f) & !is.na(dd_n$med_m)),]
      dd_n10 = dd_n[n>=10 & n_f>=5 & n_m>=5]#; nrow(dd_n10)#; dd_n10[n_by_pop>10,length(unique(pop))]

    # N nests
      dd_n10[, n_by_pop := .N, pop]#; dd_n10[n_by_pop>10, length(unique(pop))]
      dd_n10[, n_by_sp := .N, sp]#; nrow(dd_n10[sp>10]); dd_n10[n_by_sp>10,length(unique(sp))]

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
# Prepare data of f & m median bout per nest 
 
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
   
#+ f1 fig.width=9*inch,fig.height=5*1.95*inch
  # f1
  f1a =
    ggplot(dd_n10[n_by_pop>10],aes(x = med_m, y = med_f, group = pop, weight=n, col = slope_pop_certain)) + 
      #geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l)+
      geom_abline(intercept = 0, slope = 1, lty =3)+
      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_y_continuous("♀ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_color_manual(values=c(male, female))+ 
      #scale_size(breaks = c(1,15,30), name = 'n days') +
      annotate("text", x=2.9, y=15.5, label= "Fits to nests' median bouts", col = "grey30", size = 2, hjust = 0) + 
      geom_segment(aes(x = 0.75, y = 15.5, xend = 2.5, yend = 15.5), color = "darkgrey", linewidth = .35) +

      annotate("text", x=3, y=14.5, label= "Slope", col = "grey30", size = 2, hjust = 0) + 
      annotate("text", x=1.1, y=14.5, label= "+", col = male, size = 2.75) + 
      annotate("text", x=2.2, y=14.5, label = "-", col = female, size = 2.75)+
      labs(subtitle = "Populations")+
      theme_MB+ #yheme_bw() +theme_MB2+
      theme(legend.position="none"
            )

  # f1b
  f1b =
  ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = sp, weight=n, col = slope_sp_neg)) + 
      #geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l)+
      geom_abline(intercept = 0, slope = 1, lty =3)+
      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_y_continuous("♀ bout [hours]", expand = c(0, 0), breaks = c(0,4,8,12,16)) +
      scale_color_manual(values=c(male, female))+ 
      labs(subtitle = "Species")+
      theme_MB + #theme_bw() + theme_MB2+
      theme(legend.position="none"
            #axis.title.y=element_blank()
            )    

  # f1c
    n10_fmr = dd_n10[n_by_pop>10 & !(is.na(r) | duplicated(pop))] # summary(n10_fmr[n_by_sp>10, r]); n10_fmr[n_by_pop>10 & 0>r, r]
     
    f1c =
    ggplot(n10_fmr, aes(x=r, fill = r_neg)) + 
      geom_histogram() + geom_vline(xintercept = 0, lty =3, col = 'red')+
      #facet_wrap(~genus, nrow = 5) + 
      coord_cartesian(xlim = c(-0.1, 1)) + 
      scale_x_continuous("Pearson's correlation coefficient\n[for median ♂ & ♀ incubation bout length]", expand = c(0, 0), breaks = c(0,0.25,0.5,0.75,1)) + #, 
      scale_y_continuous("Count", expand = c(0, 0)) +
      scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      #labs(subtitle = "")  +
      theme_MB + #theme_bw() + theme_MB2+
      theme(legend.position="none"
            )
  # f1d
    n10_fmr_sp = dd_n10[n_by_sp>10 & !(is.na(r_sp) | duplicated(sp))] # summary(dd_n10[n_by_sp>10, r_sp]); dd_n10[n_by_pop>10 & 0>r_sp, r_sp]; nrow(n10_fmr_sp)
     
    f1d =
    ggplot(n10_fmr_sp, aes(x=r_sp, fill = r_sp_neg)) + 
      geom_histogram() + geom_vline(xintercept = 0, lty =3, col = 'red')+
      #facet_wrap(~genus, nrow = 5) + 
      coord_cartesian(xlim = c(0, 1)) + 
      scale_x_continuous("Pearson's correlation coefficient\n[for median ♂ & ♀ incubation bout length]", expand = c(0, 0), breaks = c(0,0.25,0.5,0.75,1)) + #, 
      scale_y_continuous("Count", expand = c(0, 0)) +
      scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      #labs(subtitle = "")  +
      theme_MB + #theme_bw() + theme_MB2+
      theme(legend.position="none"
            )

  # combine and export
  f1ab =  f1a + f1b +
    plot_layout(
      axis_titles = "collect"
    )

  f1cd =  f1c + f1d +
    plot_layout(
      axis_titles = "collect"
    )  

  
  if (save_plot == TRUE) {
  ggsave(file = here::here("Output/Fig_1_width-90mm_v2.png"), f1ab/f1cd, width = 9, height = 5*1.95, units = "cm", bg = "white")
  }

  f1ab/f1cd   
#'
#' <br> 
#' 
#+ fs1a fig.width=20*inch,fig.height=13*inch
  fs1a = 
  ggplot(dd_n10[n_by_pop>10],aes(x = med_m, y = med_f, group = pop, weight=n, col = suborder)) + 
      geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      facet_wrap(~pop_lat, nrow = 4, scales = "free") + 
      geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l)+
      geom_smooth(method = 'lm', se = FALSE, alpha = 0.2, linewidth = size_l, col = 'red')+
      geom_abline(intercept = 0, slope = 1, lty =3)+
      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      #coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_size(name = "Number of bouts") + 
      #scale_size(breaks = c(1,15,30), name = 'n days') +
      labs(subtitle = "Populations (species name & ° latitude")+
      theme_MB + 
      theme(strip.background = element_blank())
  if (save_plot == TRUE) {
      ggsave(file = here::here("Output/Fig_S1a.png"), fs1a, width = 20, height = 13, units = "cm", bg = "white")
      }      
  #' 
  #' <br> 
  #' 
  #+ fs2 fig.width=20*inch,fig.height=13*inch
  f1a = 
  ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = scinam, weight=n)) + 
      geom_point(aes(size = n, col = suborder), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE,  col = 'grey40', aes(lwd = slope_sp_certain))+ #linewidth = size_l,alpha = 0.2,
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = med_m, y = med_f, label = ..r.label..), inherit.aes = FALSE) +

      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
      scale_size(name = "Number of bouts") + 
      #scale_size(breaks = c(1,15,30), name = 'n days') +

      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      #coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      labs(subtitle = "A")+
      facet_wrap(~scinam, ncol = 6, scales = "free") + 
      theme_MB + 
      theme(strip.background = element_blank())

  # add inset
    adding_inset <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
      layer(data = data, stat = StatIdentity, position = PositionIdentity, 
            geom = ggplot2:::GeomCustomAnn,
            inherit.aes = TRUE, params = list(grob = grob, 
                                              xmin = xmin, xmax = xmax, 
                                              ymin = ymin, ymax = ymax))
    }
    inset_dunl =
      ggplot(dd_n10[n_by_pop>10 & scinam == 'Calidris alpina'],aes(x = med_m, y = med_f, group = pop, weight=n)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))

    inset_sesa = 
      ggplot(dd_n10[n_by_pop>10 & scinam == 'Calidris pusilla'],aes(x = med_m, y = med_f, group = pop, weight=n)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))

    inset_kepl = 
      ggplot(dd_n10[n_by_pop>10 & scinam == "Charadrius alexandrinus"],aes(x = med_m, y = med_f, group = pop, weight=n)) + 
        geom_smooth(method = 'rlm', se = FALSE, col = 'grey40', aes(lwd = slope_pop_certain))+
        scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain") +
        guides(lwd="none") + 
        geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
        theme_void() +
        theme(panel.border = element_rect(colour="grey80", linewidth=0.15, fill = NA))
    
    inset_amgp =
      ggplot(dd_n10[n_by_pop>10 & scinam == "Pluvialis dominica"],aes(x = med_m, y = med_f, group = pop, weight=n)) + 
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
      n = 1)

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
    f1a_
    
  if (save_plot == TRUE) {
      ggsave(file = here::here("Output/Fig_1a_v2.png"), f1a_, width = 20, height = 10, units = "cm", bg = "white")
      }  #dev.new(width = 20*inch, height = 10*inch)    
  #' 
  #' <br> 
  #'     

#' #### Across evolutionary history 
  # prepere data for plotting
    ds = dd_n10[n_by_sp>10]
    ds = ds[, cor(med_f, med_m), by = list(scinam, animal)]  %>% setnames(old = 'V1', new = 'r')
    ds = merge(ds, dd_n10[!duplicated(scinam), .(scinam,n_by_sp)])
    #summary(ds); summary(ds[!scinam%in%'Limosa lapponica'])

    sp_r=data.frame(ds[,c("r","scinam")])
    tree_r = drop.tip(tree, tree$tip.label[!tree$tip.label%in%sp_r$scinam])
    
    row.names(sp_r)=sp_r$scinam
    sp_r$scinam=NULL
    sp_r$r=as.numeric(sp_r$r)
      
    svlr<-as.matrix(sp_r)[,1]
    svlr<-svlr[tree_r$tip.label]
      
    objr<-contMap(ladderize(tree_r, right=T),svlr,plot=FALSE)
    objr<-setMap(objr,colors=c(brewer.pal(11,"Spectral")[1],brewer.pal(11,"Spectral")[4],brewer.pal(11,"Spectral")[7:11])) 

  # prepare phylogenetic contrasts
    r_pear=ds$r
    names(r_pear)=ds$scinam
    tree_r_l=ladderize(tree_r, right=T)
    
    yourPics <- pic(x=r_pear, phy=tree_r_l)

  # plot
    if(save_plot==TRUE){png(here::here("Output/Fig_2_width-64mm_v2.png"), width=2.5,height=3.5,units="in",res=600)} else {
    dev.new(width=2.3,height=3.5)}

    #save(file='freeze/Data/for_Liam.Rdata', objr)
    #load('for_Liam.Rdata')
    par(mar=c(1.6,1.6,1.7,0.7), ps=12, mgp=c(1.35,0.35,0), las=2, cex.lab=0.7, cex.axis=0.6, tcl=-0.15,bty="n") # 0.6 makes font 7pt, 0.7 8pt

    plot(objr,lwd=2, sig=1,outline=FALSE,fsize=0.5,legend=FALSE)# legend=30)#mar=c(5.1,0.2,0.2,0.2),

    add.color.bar(30, objr$cols, title="", digits=0, prompt=FALSE,
      x=0, y=1.1, subtitle="", outline=FALSE, lwd=3, lims=NULL)
    text(x=3, y=1.1, "0", pos=2, cex = 0.4) ## to the left
    text(x=27, y=1.1, "1", pos=4, cex = 0.4) ## to the right
    text(x=14, y=1, "Pearson's r for\n ♀ & ♂ median nest bout",
      pos=3, cex = 0.4)

    #add.color.bar(30,objr$cols,title=list("Pearson's r for ♀ & ♂\nmedian nest bout"),fsize=0.4,lims=objr$lims,digits=0,prompt=FALSE,x=0,y=1.1,subtitle="",outline=FALSE,lwd=3)#, mgp = c(0, 0,0))
    
    nodelabels(pch=21, cex=abs(yourPics)*10, bg="grey90", col = "grey50") # add phylogenetic contrast contrasts
    
    if(save_plot==TRUE){dev.off()}  

#' <br> 
#' 
#' ***
#' 
#' ### Within nest bout pair similarity
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

  # estimate within pair bout correlations
    fm[, n_by_nest := .N, pk_nest] #summary(fm$n_by_nest); fm[n_by_nest>6, length(unique(pk_nest))]
    # pearson
      fm[n_by_nest>5,  r := cor(bout_f, bout_m), by = pk_nest]
      fm[n_by_nest>5 & r<0, r_neg := 'yes']
      fm[n_by_nest>5 & !r_neg%in%'yes', r_neg := 'no']
      fmr = fm[!is.na(r), list(
        r = unique(r)
        ),
        by = list(suborder,genus,animal,sp,scinam,species,year, breeding_site,pop,lat_pop, pop_lat, nest, nn, app, tidal, tidal_pop,pop_wing_f, pop_wing_m, pk_nest, lat, n_by_nest, r_neg)  
      ] # same as fmr = fm[!(is.na(r) | duplicated(paste(r, pk_nest))) ]

    # rlm
      fm[n_by_nest>5,  slope_nest := rlm(bout_f ~ bout_m, maxit = 100)  %>% coef  %>% magrittr::extract(2), by = pk_nest] 
      fm[n_by_nest>5 & slope_nest<0, slope_nest_neg := 'yes']
      fm[n_by_nest>5 & !slope_nest_neg%in%'yes', slope_nest_neg := 'no']
    
    # find a group that causes the warning
      fm5 = fm[n_by_nest>5]
      problematic_groups <- lapply(unique(fm5$pk_nest), function(g) {
        tryCatch({
          fm5[pk_nest == g, slope_nest := rlm(bout_f ~ bout_m, maxit = 100)  %>% coef  %>% magrittr::extract(2)]
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

    fm_rlm = fm[!pk_nest =="AMGP chur 2013 13AMGP03W 1"] # remove the group where rlm failed to converge #ggplot(x, aes(x = bout_m, y = bout_f)) + geom_point()

#' #### Distribution of within nest correlations
#+ fig.width = 6, fig.height = 5
  # within and across species  
       # in r and suborder specific
        give.n <- function(x){
          return(c(y = 1.1, label = length(x))) 
          # experiment with the multiplier to find the perfect position
        }
      # define color for genus
        go=data.frame(genus=c("Arenaria","Calidris","Tringa","Limnodromus", "Limosa","Numenius", "Charadrius", "Vanellus", "Pluvialis","Haematopus"),
        cols=c(brewer.pal(11,"Spectral")[1:6],brewer.pal(11,"Spectral")[7:8],brewer.pal(11,"Spectral")[10:11]), stringsAsFactors=FALSE)
      
        ggplot(fmr[scinam%in%unique(fmr_sp$scinam[fmr_sp$n>2])], aes(y = fct_reorder(species, r, .fun = median, .desc =TRUE), x = r, fill = genus)) + 
            geom_boxplot(
              lwd = 0.25,
              outlier.size = 0.25,
              outlier.color = "grey40")+
            stat_summary(fun.data = give.n, geom = "text", size = 1.5, col = "grey30") +
            #scale_x_continuous(lim = c(-1,1))+
            scale_fill_manual(values = go$cols, name = 'Genus')+
            facet_grid(rows = vars(forcats::fct_relevel(suborder, "Scolopaci", "Charadrii")), scales = "free_y",space = "free_y") +
            geom_vline(xintercept = 0, lty = 3)+
            xlab("Pearson's r for ♀ & ♂ bout length\n[for each nest]") +
            theme_MB +
            theme(
              panel.grid.major.y = element_line(color = "grey90", size = 0.2),
              text = element_text(family = "Arial Unicode MS"),
              axis.title.y=element_blank()
                )
      #ggsave(here::here('Output/Boxplot_fm-r.png'), height = 4, width = 3.5)
    # within and across genera
    ggplot(fmr[n_by_nest>5], aes(x=r, fill = r_neg)) + 
      geom_histogram() + geom_vline(xintercept = 0, lty =3, col = 'red')+
      facet_wrap(~genus, nrow = 5) + 
      scale_x_continuous("Pearson's correlation coefficient for ♂ & ♀", expand = c(0, 0)) +
      scale_y_continuous("Nests [count]", expand = c(0, 0)) +
      scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      labs(subtitle = "Based on individual bouts")  +
      theme(text = element_text(family = "Arial Unicode MS")) +
      theme_MB
    ggsave('Output/cor_fm_r-bout_hist_genus.png', height = 4, width = 2.5)
#' #### Betweena & within nest variation in rlm 
#+ fig.width=6,fig.height=6
  ggplot(fm_rlm[n_by_nest>10],aes(x = bout_m, y = bout_f, group = pk_nest, col = slope_nest_neg)) + 
    geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, size = size_l, method.args = list(maxit = 100))+
    scale_color_manual(values=c(male, female))+ 
    geom_abline(intercept = 0, slope = 1, lty =3)+
    #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
    facet_wrap(~lat_pop) +
    coord_cartesian(xlim = c(0, 30),ylim = c(0, 30)) +
    scale_x_continuous("♂ bout [hours]", expand = c(0, 0), breaks = c(0,10,20)) +
    scale_y_continuous("♀ bout [hours]", expand = c(0, 0), breaks = c(0,10,20)) +
    labs(title = "Based on individual bouts")+
    theme_MB
# End        