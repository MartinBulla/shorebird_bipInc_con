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
  save_plot = FALSE # save_plot as PNG TRUE/FALSE
  fam = "Arial Unicode MS"# text family
  female='#FCB42C' # 'deepskyblue'
  male='#535F7C' # 'red'
  ax_lines = "grey60"
  size_l = 0.75
  tx_legend_tit = 6
  inch = 0.393701 
   

  # packages and functions
  require(here)
  source(here::here('R/tools.R'))  # TODO:in the final version bring it here within the text
  
  # data
    #  prepare max cred tree
      trees =  read.nexus(here::here("Data/Bulla_et_al_2016-100_Trees_Hackett_all_species_niv.tre"))
      tree <- maxCladeCred(trees)
      tree$tip.label[tree$tip.label=="Catoptrophorus_semipalmatus"]="Tringa_semipalmata" # current name for this species  
      #plot(tree, main="Maximum Credibility Tree")  
      #plot(ladderize(tree, right = TRUE))

    # f & m median bout per nest
      load(here::here("Data/Bulla_et_al_2016-comparative_all.RData"))  
      d$lat_a=abs(d$lat)
      d=d[d$sex%in%c('f','m'),]
      d = data.table(d)

      d[, lat_pop := factor(paste(round(mean(lat),2),sp)), pop]
      d[, lat_pop := factor(lat_pop, levels=rev(levels(lat_pop)[order(levels(lat_pop))]))]

      d[,col_ := ifelse(suborder == 'Scolopaci', female, male)] # adds color for ploting

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
        by = list(suborder,genus,animal,sp,scinam,species,breeding_site,pop, lat_pop, year,nest, nn,pk_nest, pop_wing_f,pop_wing_m,app, tidal, tidal_pop,col_)
        ]

      dd_n=dd_n[which(!is.na(dd_n$med_f) & !is.na(dd_n$med_m)),]
      

#'***
#' ## TODO:ABSTRACT
#' 
#' <br> 
#' Here, we use unprecedented comparative dataset on ~ `r as.character(round_any(nrow(d), 100))` incubation bouts from `r length(unique(d$pk_nest))` nests of `r length(unique(d$pop))` populations of `r length(unique(d$sp))` shorebird species from `r length(unique(d$genus))` genera to investigate how female bouts correlate with those of males and whether the correlations differ or are consistent across evolutionary history.  
#' <br>
#'
#' ***

#' ### Pair similarity in bouts
#' #### Across and within populations
#+ f1 fig.width=9*inch,fig.height=5*inch
    # prepare data
    dd_n10 = dd_n[n>=10 & n_f>=5 & n_m>=5]; nrow(dd_n10); dd_n10[n_by_pop>10,length(unique(pop))]
    dd_n10[, n_by_pop := .N, pop]; nrow(dd_n10[n_by_pop>10])

    dd_n10[n_by_pop>5,  slope_pop := rlm(med_f ~ med_m, weights = n)  %>% coef  %>% magrittr::extract(2), by = pop] 
    dd_n10[n_by_pop>5 & slope_pop<0, slope_pop_neg := 'yes']
    dd_n10[n_by_pop>5 & !slope_pop_neg%in%'yes', slope_pop_neg := 'no']
    
    dd_n10[, n_by_sp := .N, sp]; nrow(dd_n10[sp>10]); dd_n10[n_by_sp>10,length(unique(sp))]
    dd_n10[n_by_sp>5,  slope_sp := rlm(med_f ~ med_m, weights = n)  %>% coef  %>% magrittr::extract(2), by = sp] 
    dd_n10[n_by_sp>5 & slope_sp<0, slope_sp_neg := 'yes']
    dd_n10[n_by_sp>5 & !slope_sp_neg%in%'yes', slope_sp_neg := 'no']

    # f1a
    f1a =
      ggplot(dd_n10[n_by_pop>10],aes(x = med_m, y = med_f, group = pop, weight=n, col = slope_pop_neg)) + 
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
        geom_segment(aes(x = 0.75, y = 15.5, xend = 2.5, yend = 15.5), color = "darkgrey", size = .35) +

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
 
    # combine and export
    f1 =  f1a + f1b +
      plot_layout(
        axis_titles = "collect"
      )

    if (save_plot == TRUE) {
    ggsave(file = here::here("Output/Fig_1_width-90mm.png"), f1, width = 9, height = 5, units = "cm", bg = "white")
    }

    f1   
#'
#' <br> 
#' 
#+ fs1a fig.width=20*inch,fig.height=13*inch
dd_n10[, pop_lat :=paste(scinam, substring(lat_pop, 1,5))]
dd_n10[n_by_pop>10, length(unique(pop_lat))] #dd_n10[n_by_pop>10, unique(pop_lat)]
dd_n10[pop_lat=="Arenaria interpres 71 RU", pop_lat:="Arenaria interpres 71.00"]
dd_n10[pop_lat=="Tringa totanus 52.9 ", pop_lat:="Tringa totanus 52.90"   ]
dd_n10[pop_lat=="Limosa haemastica 61.2 ", pop_lat:="Limosa haemastica 61.20"   ]
dd_n10[pop_lat=="Charadrius alexandrinus 24.26", pop_lat:="Char. alexandrinus 24.26"   ]
dd_n10[pop_lat=="Charadrius alexandrinus 41.29", pop_lat:="Char. alexandrinus 41.29"   ]
dd_n10[pop_lat=="Haematopus bachmani 61.09", pop_lat:="Haem. bachmani 61.09"]
dd_n10[pop_lat== "Numenius phaeopus 63.83", pop_lat:= "Numen. phaeopus 63.83"]

fs1a = 
ggplot(dd_n10[n_by_pop>10],aes(x = med_m, y = med_f, group = pop, weight=n, col = suborder)) + 
        geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
        facet_wrap(~pop_lat, nrow = 4, scales = "free") + 
        geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l)+
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
fs1b = 
ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = scinam, weight=n, col = suborder)) + 
        geom_point(aes(size = n), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
        facet_wrap(~scinam, ncol = 6, scales = "free") + 
        geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l)+
        geom_abline(intercept = 0, slope = 1, lty =3)+
        #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
        #facet_wrap(~pop) +
        #coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
        scale_x_continuous("♂ bout [hours]") +
        scale_y_continuous("♀ bout [hours]") +
        scale_color_manual(values=c(male, female), name = "Suborder")+ 
        scale_size(name = "Number of bouts") + 
        #scale_size(breaks = c(1,15,30), name = 'n days') +
        labs(subtitle = "Species (name & ° latitude)")+
        theme_MB + 
        theme(strip.background = element_blank())

if (save_plot == TRUE) {
    ggsave(file = here::here("Output/Fig_S1b.png"), fs1b, width = 20, height = 10, units = "cm", bg = "white")
    }      
#' 
#' <br> 
#'     

#' #### Across evolutionary history
    ds = dd_n10[n_by_sp>10]
    ds = ds[, cor(med_f, med_m), by = list(scinam, animal)]  %>% setnames(old = 'V1', new = 'r')
    summary(ds)
    
    # prepere data for plotting
    sp_r=data.frame(ds[,c("r","animal")])
    tree_r = drop.tip(tree, tree$tip.label[!tree$tip.label%in%sp_r$animal])
        
    row.names(sp_r)=sp_r$animal
    sp_r$animal=NULL
    sp_r$r=as.numeric(sp_r$r)
      
    svlr<-as.matrix(sp_r)[,1]
    svlr<-svlr[tree_r$tip.label]
      
    objr<-contMap(ladderize(tree_r, right=T),svlr,plot=FALSE)
    objr<-setMap(objr,colors=c(brewer.pal(11,"Spectral")[1],brewer.pal(11,"Spectral")[4],brewer.pal(11,"Spectral")[7:11])) 

    # prepare phylogenetic contrasts
      r_pear=ds$r
      names(r_pear)=ds$animal
      tree_r_l=ladderize(tree_r, right=T)
      yourPics <- pic(x=r_pear, phy=tree_r_l)

    # plot
      if(save_plot==TRUE){png(here::here("Output/Fig_2_width-64mm.png"), width=2.5,height=3.5,units="in",res=600)} else {
      dev.new(width=2.3,height=3.5)}

      #save(file='freeze/Data/for_Liam.Rdata', objr)
      #load('for_Liam.Rdata')
      par(mar=c(1.6,1.6,1.7,0.7), ps=12, mgp=c(1.35,0.35,0), las=2, cex.lab=0.7, cex.axis=0.6, tcl=-0.15,bty="n") # 0.6 makes font 7pt, 0.7 8pt
  
      plot(objr,lwd=2, sig=1,outline=FALSE,fsize=0.5,legend=FALSE)# legend=30)#mar=c(5.1,0.2,0.2,0.2),

      add.color.bar(30,objr$cols,title=list("Pearson's r for ♀ & ♂\nmedian nest bout"),fsize=0.4,lims=objr$lims,digits=0,prompt=FALSE,x=0,y=1.1,subtitle="",outline=FALSE,lwd=3)#, mgp = c(0, 0,0))
      
      nodelabels(pch=21, cex=abs(yourPics)*10, bg="grey90", col = "grey50") # add phylogenetic contrast contrasts
      
      if(save_plot==TRUE){dev.off()}  

#' <br> 
#' 
#' ***
#' 
# End        