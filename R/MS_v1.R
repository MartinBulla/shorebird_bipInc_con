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
      n_days=as.numeric(difftime(max(datetime_off),min(datetime_on),days))),
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
a = dd_n10[n_by_sp>10]
quantile(a$r_sp, probs = c(0.025,0.5,0.975)); mean(a$r_sp) # weighing by number of m-f bouts is meaningless in the cross species context where species differ in bout lengths: 
wtd.quantile(a$r_sp, a$n_by_sp, probs = c(0.025,0.5,0.975));wtd.mean(a$r_sp, a$n_by_sp) 

#+ f1 fig.width=20*inch,fig.height=19.5*inch
  f1a = 
  ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = scinam, weight=n_fm)) + 
      geom_point(aes(size = n, col = suborder), alpha = 0.5)+#geom_point(size = 0.5, alpha = 0.5) + 
      geom_smooth(method = 'rlm', se = FALSE,  col = 'grey40', aes(lwd = slope_sp_certain))+ #linewidth = size_l,alpha = 0.2,
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = med_m, y = med_f, label = ..r.label..), inherit.aes = FALSE) +

      scale_color_manual(values=c(male, female), name = "Suborder")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
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

  #export ggsave(file = here::here("Output/Fig_1A_width-180mm.png"), f1a_, width = 20, height = 10, units = "cm")

# f1b fig.width=10*inch,fig.height=10*inch
f1b =
    ggplot(dd_n10[n_by_sp>10],aes(x = med_m, y = med_f, group = sp, weight=n, color = suborder)) + 
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

# f1c fig.width=10*inch,fig.height=10*inch
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
    scale_color_gradientn(colours = (cols_f1), name = "Assortative mating") +
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
qn <- scales::rescale(quantile(ds$r), probs = seq(0, 1, length.out = length(cols_f1)))

dens <- density(ds$r, n = 2^12)
den <- data.table(x = dens$x, y = dens$y)
#den <- den[x > log10(0.99) & x < log10(50.01)]

f1c_l <-
    ggplot() +
    geom_segment(data = den, aes(x, y, xend = x, yend = 0, colour = x)) +
    scale_color_gradientn(
        colours = cols_f1, # viridis(10),
        values = qn # c(0, seq(qn01[1], qn01[2], length.out = 18), 1)
    ) +
    geom_vline(xintercept = median(ds$r), lty =3, linewidth = 0.5, color = 'red')+
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
    f1a,f1bc,
    nrow=2, heights=c(9.5,10), align = 'v'
    )

if (save_plot == TRUE) {
ggsave(here::here("Output/Fig_1_width-180mm.png"), f1abc, width = 20, height = 19.7, units ='cm', bg='white')
x <- image_read(here::here("Output/Fig_1_width-180mm.png"), density=300)
y <- image_trim(x) # width = 94.5cm, height = 88 (conversion factor 1181/100 )
image_write(y, path = "Output/Fig_1_width-178mm_trimmed.png", format = "png", density = 300)
} 

f1abc

#' <br> 
#' 
#' ***
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

      # median r based on certain rs only, except for Pluvialis sqatarola having only uncertain ones, while making placement for the free axis (both below opitons work, but not 100% because they forget that chat gpt sets the pedding to the axis - TODO:check and adjust - see the threat there)
      #TODO:limit coordinates in free, to zoom in a little
      # first option
      fmrm = fmr[slope_nest_certain%in%'yes', list(
        r = median(r, na.rm = TRUE),
        bout_m_pos =  min(bout_m_min) + 0.05 * (max(bout_m_max) - min(bout_m_min)),
        bout_f_pos =  max(bout_f_max) + 0.05 * (max(bout_f_max) - min(bout_f_min))),
        by = list(suborder,genus,animal,sp,scinam,species)  
      ] 
      fmrm_add = fmr[scinam%in%'Pluvialis squatarola', list(
        r = median(r, na.rm = TRUE),
        bout_m_pos =  min(bout_m_min)+ 0.05 * (max(bout_m_max) - min(bout_m_min)),
        bout_f_pos =  max(bout_f_max) + 0.05 * (max(bout_f_max) - min(bout_f_min))),
        by = list(suborder,genus,animal,sp,scinam,species)  
      ] 
      fmrm = rbind(fmrm,fmrm_add)
    
    # second option
      fmrm = fmr[slope_nest_certain%in%'yes', list(
        r = median(r, na.rm = TRUE),
        bout_m_pos =  min(bout_m_min),
        bout_f_pos =  max(bout_f_max)),
        by = list(suborder,genus,animal,sp,scinam,species)  
      ] 
      fmrm_add = fmr[scinam%in%'Pluvialis squatarola', list(
        r = median(r, na.rm = TRUE),
        bout_m_pos =  min(bout_m_min),
        bout_f_pos =  max(bout_f_max)),
        by = list(suborder,genus,animal,sp,scinam,species)  
      ] 
      fmrm = rbind(fmrm,fmrm_add)

      # Offset the text slightly to avoid overlaps
        x_offset <- 0.025  # Adjust as needed
        y_offset <- 0.05 # Adjust as needed

    # align the two datasets to be plotted
      fm10=fm[n_by_nest>10]
      fmrm <- fmrm %>%
          mutate(scinam = factor(scinam, levels = unique(fm10$scinam)))


#' 
#+ f2 fig.width=20*inch,fig.height=20*inch
  f2a = 
  ggplot() + 
      geom_smooth(data = fm10, aes(x = bout_m, y = bout_f, group = pk_nest, lwd = slope_nest_certain), method = 'rlm', se = FALSE,  col = 'grey40', alpha = 0.8, method.args = list(maxit = 200))+ #linewidth = size_l,alpha = 0.2, col = slope_nest_neg
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      geom_text(data = fmrm, aes(x = 0, y =50, 
      label = paste('r =', round(r,2))), #paste(expression(paste(italic("r"), "="), round(r,2)))), # 
      hjust = 0, vjust =1, size = 2) +
      #      ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = bout_m, y = bout_f, label = ..r.label..), inherit.aes = FALSE) +
      #scale_color_manual(values=c(male, female), name = "Slope negative")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
    
      #scale_size(breaks = c(1,15,30), name = 'n days') +

      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      #coord_cartesian(xlim = c(0, 30),ylim = c(0, 30)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      #labs(subtitle = "A")+
      labs(tag = 'A')+
      facet_wrap(~scinam, ncol = 8)+#, scales = "free") + 
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
ggsave(file = here::here("Output/Fig_2a_width-180mm_fixed-axis-shortNames.png"), f2a, width = 20, height = 11, units = "cm")

f2a_lim = 
  ggplot() + 
      geom_smooth(data = fm10, aes(x = bout_m, y = bout_f, group = pk_nest, lwd = slope_nest_certain), method = 'rlm', se = FALSE,  col = 'grey40', alpha = 0.8, method.args = list(maxit = 200))+ #linewidth = size_l,alpha = 0.2, col = slope_nest_neg
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      geom_text(data = fmrm, aes(x = 0, y =30, 
      label = paste('r =', round(r,2))), #paste(expression(paste(italic("r"), "="), round(r,2)))), # 
      hjust = 0, vjust =1, size = 2) +
      #      ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = bout_m, y = bout_f, label = ..r.label..), inherit.aes = FALSE) +
      #scale_color_manual(values=c(male, female), name = "Slope negative")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
    
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
      theme(strip.background = element_blank()
           #panel.background = element_rect(fill = "transparent",
            #                     colour = NA_character_), # necessary to avoid drawing panel outline
    
          #plot.background = element_rect(fill = "transparent",
           #                         colour = NA_character_), # necessary to avoid drawing plot outline
      #legend.background = element_rect(fill = "transparent"),
      #legend.box.background = element_rect(fill = "transparent"),
      #legend.key = element_rect(fill = "transparent")
      )
ggsave(file = here::here("Output/Fig_2a_width-180mm_fixed-axis-limits30.png"), f2a_lim, width = 20, height = 11, units = "cm")


f2a_free = 
  ggplot() + 
      geom_smooth(data = fm[n_by_nest>10], aes(x = bout_m, y = bout_f, group = pk_nest, lwd = slope_nest_certain), method = 'rlm', se = FALSE,  col = 'grey40', alpha = 0.8, method.args = list(maxit = 100))+ #linewidth = size_l,alpha = 0.2, col = slope_nest_neg
      geom_abline(intercept = 0, slope = 1, lty =3, col = 'red')+
      facet_wrap(~scinam, ncol = 6, scales = "free") + 
      #v1: geom_text(data = fmrm, aes(x = bout_m_pos, y =bout_f_pos, 
      #label = paste('r =', round(r,2))),
      #hjust = 0, vjust =1, size = 2) +
      geom_text(data = fmrm, aes(x = bout_m_pos-x_offset*abs(bout_m_pos), y =bout_f_pos+y_offset*abs(bout_f_pos), 
      label = paste('r =', round(r,2))),
      hjust = 0, vjust =1, size = 2) + #v2
      #ggpubr::stat_cor(method="pearson",size = 2, cor.coef.name = 'r',aes(x = bout_m, y = bout_f, label = ..r.label..), inherit.aes = FALSE) +
      #scale_color_manual(values=c(male, female), name = "Slope negative")+ 
      scale_linewidth_manual(values=c(.25, size_l), name = "Slope certain")+ 
    
      #scale_size(breaks = c(1,15,30), name = 'n days') +

      #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
      #facet_wrap(~pop) +
      #coord_cartesian(xlim = c(0, 16),ylim = c(0, 16)) +
      scale_x_continuous("♂ bout [hours]") +
      scale_y_continuous("♀ bout [hours]") +
      #labs(subtitle = "A")+
      labs(tag = 'A')+
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
ggsave(file = here::here("Output/Fig_2a_width-180mm_free-axis_v2.png"), f2a_free, width = 20, height = 16, units = "cm")

ann_text_f2b <- data.frame(r = c(-0.5, 0.5),lab = c("Negative", "Positive"),
                       genus = factor('Arenaria',levels = c("Arenaria", "Calidris","Charadrius","Haematopus","Limnodromus","Limosa","Numenius","Pluvialis","Tringa","Vanellus")))
f2b = 
   ggplot(fmr[n_by_nest>10 & slope_nest_certain%in%'yes'], aes(x=r))+#, fill = r_neg)) + 
      geom_rect(xmin = -2, xmax = 0, ymin = -Inf, ymax = Inf,fill = 'grey80',inherit.aes = FALSE)+
      geom_histogram() + geom_vline(xintercept = 0, lty =3, col = 'black')+
      facet_wrap(~genus, nrow = 5) + 
      geom_text(data = ann_text_f2b,y = 14, label = c("Negative", "Positive"), size = 0.7/scale_size, col = 'grey30')+
      scale_x_continuous("Pearson's correlation coefficient for ♂ & ♀\nincubation bouts", expand = c(0, 0)) +
      scale_y_continuous("Nests [count]", expand = c(0, 0)) +
      #scale_fill_manual(values = c(male, female), name = 'Negative correltation')+
      #labs(subtitle = "Based on individual bouts")  +
      theme(text = element_text(family = "Arial Unicode MS")) +
      theme_MB
 ggsave('Output/Fig_2b.png', f2b, height = 6, width = 2.5)

f2c = 


blank = ggplot() + theme_void() 
f2b_b = ggarrange(
    blank, f1b,blank,
    nrow=3, heights=c(0.4,6.7,2.9), align = 'v'
    )
f1bc = ggarrange(
    f1b_b,blank, f1c, blank,
    ncol=4, widths=c(6.6,1.4,11, 1), align = 'h'
    )
f1abc = ggarrange(
    f1a,f1bc,
    nrow=2, heights=c(9.5,10), align = 'v'
    )

#' <br> 
#' 
#' ***
#' 

#+ f2c, fig.width = 6, fig.height = 5
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
    ggplot(fm[n_by_nest>10], aes(x=r, fill = r_neg)) + 
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
  ggplot(fm[n_by_nest>10],aes(x = bout_m, y = bout_f, group = pk_nest, col = slope_nest_neg)) + 
    geom_smooth(method = 'rlm', se = FALSE, alpha = 0.2, linewidth = size_l, method.args = list(maxit = 100))+
    scale_color_manual(values=c(male, female))+ 
    geom_abline(intercept = 0, slope = 1, lty =3)+
    #stat_cor(aes(label = ..r.label..),  label.x = 3, size = 2) + 
    facet_wrap(~scinam) +
    coord_cartesian(xlim = c(0, 30),ylim = c(0, 30)) +
    scale_x_continuous("♂ bout [hours]", expand = c(0, 0), breaks = c(0,10,20)) +
    scale_y_continuous("♀ bout [hours]", expand = c(0, 0), breaks = c(0,10,20)) +
    labs(title = "Based on individual bouts")+
    theme_MB
# End        