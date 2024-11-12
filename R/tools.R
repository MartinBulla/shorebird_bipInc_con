# ==========================================================================
# Contributor: Martin Bulla
# 📍 This script runs relative to the project's root directory, and loads
# functions and packages used in the analyses
# ==========================================================================

#' Loads packages and installs those that are not in the library
#' @param  vector of package names
#' @export

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))#, quietly  = TRUE
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}

# Set system time
   Sys.setenv(TZ="UTC")

#' Extract time as numeric from POSIXct
#' @param  x (POSIXct)
#' @export
getime = function (x) {ifelse(is.na(x), as.numeric(NA), as.numeric(difftime(x, trunc(x,"day"), units = "hours")))}

#' Extract DATE from POSIXct
#' @param  x (POSIXct)
#' @export
getDay = function (x) {as.Date(trunc(x, "day"))}

# load/install packages
  packages = c('ape','arm','RColorBrewer','data.table', 'effects', 'forcats', 'ggExtra', 'ggnewscale', 'ggplot2', 'ggthemes', 'glue',  'grid','gridExtra', 'here', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'maptools', 'multcomp', 'pals','patchwork', 'performance', 'phangorn','phytools','plyr','raster','reshape2','scales','stringr','readxl','zoo', 'gt', 'tidyverse', 'ggpubr')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )

# Customized ggplot themes
    theme_MB2 = theme(
      text = element_text(family = fam),
      plot.tag = element_text(size = 7,face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = tx_legend_tit),
      legend.text = element_text(size = 6),
      # legend.spacing.y = unit(0.1, 'cm'),
      legend.key.height = unit(0.5, "line"),
      legend.margin = margin(0, 0, 0, 0),
      # legend.position=c(0.5,1.6),
      plot.title = element_text(color = "grey", size = 7),
      plot.margin = margin(b = 0.5, l = 0.5, t = 0.5, r = 1, unit = "pt"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = ax_lines, linewidth = 0.25),
      axis.ticks = element_line(colour = ax_lines, linewidth = 0.25),
      axis.ticks.length = unit(1, "pt"),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 7)
    )

    theme_MB = theme(  
              text = element_text(family = fam),
              title = element_text(size=8, colour="grey30"),
              plot.subtitle = element_text(size=7.5),
              axis.line = element_blank(),
              #axis.line = element_line(colour="grey70", linewidth=0.25),
              axis.title = element_text(size=7, colour="grey30"),
              axis.title.y = element_text(vjust=3.5),
              axis.title.x = element_text(vjust=1),
              axis.text = element_text(size=6),#, vjust = 0.5, hjust=1),# margin=units(0.5,"mm")),
              axis.ticks.length=unit(0.5,"mm"),
              axis.ticks = element_line(colour = "grey70", linewidth = 0.1),
              #axis.ticks.margin,
              
              strip.text.x = element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
              strip.text.y = element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
              strip.background = element_rect(fill="grey99",colour="grey70", linewidth=0.25),
                #strip.background = element_blank(), 
                #strip.text = element_blank(),
              panel.spacing = unit(0, "mm"),
              panel.background=element_blank(),
              panel.border = element_rect(colour="grey70", linewidth=0.1, fill = NA), #panel.border=element_blank(),
              panel.grid = element_blank(),

              legend.text=element_text(size=6),
              legend.title=element_text(size=6),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.height= unit(0.5,"line"),
              legend.key.width = unit(0.25, "cm"),
              legend.margin = margin(0,0,0,0, unit="cm"),
              legend.box.margin = margin(l = -6), #legend.justification = c(-1,0),
              legend.background = element_blank()
              )  

# model assumption functions
  # mixed models
  m_ass = function(file_name = 'define', mo = m0, dat = d, fixed = NULL, categ = NULL, trans = "none", spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'Output/Model_ass/'){
   l=data.frame(summary(mo)$varcor)
   l = l[is.na(l$var2),]
   if(PNG == TRUE){
    png(paste(outdir,file_name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = nrow(l)-1+length(fixed)+length(categ) + 6 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
   #unique(l$grp[l$grp!="Residual"])
   for(i in unique(l$grp[l$grp!="Residual"])){
    #i = "species"
    ll=ranef(mo)[names(ranef(mo))==i][[1]]
    if(ncol(ll)==1){
     qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
     }else{
      qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
      qqnorm(ll[,2], main = paste(i,names(ll)[2]),col='grey');qqline(ll[,2], col ='red')
     }
    }
    
   # variables
   scatter={} 
   for (i in rownames(summary(mo)$coef)) {
        #i = "lat_abs"
      j=sub("\\).*", "", sub(".*\\(", "",i)) 
      scatter[length(scatter)+1]=j
    }
    x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                    log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
    if(length(fixed)!=0){
      for (i in 1:length(fixed)){
          jj =fixed[i]
          variable=dat[, ..jj][[1]]
          if(trans[i]=='log'){
          scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='abs'){
          scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else{
          scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }
       }
      }
    if(length(categ)>0){
      for(i in categ){
         variable=dat[, ..i][[1]]
          boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
         }
    }     
          
    if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
        }
    
    if(spatial == TRUE){    
      spdata=data.frame(resid=resid(mo), x=dat$lat, y=dat$lon)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
      plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
        legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
      plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
      plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
        }
   
   mtext(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''), side = 3, line = -1, cex=0.5,outer = TRUE)
   
   if(PNG==TRUE){dev.off()}
  }
  
  # simple models
  m_ass_s = function(file_name = 'define', title = 'define', binomial = FALSE, mo = m0, dat = d, fixed = NULL, categ = NULL, trans = NULL, spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir'){
    # binomial - shall a plot visualizing response means per sequence of fitted data be visualized?
    # trans - vector containing transformation function used to transform each predictor
   if(PNG == TRUE){
    png(paste(outdir,file_name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = length(fixed)+length(categ) + 4 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   if(binomial == TRUE){
      plot(fitted(mo), jitter(mo$model[,1], amount=0.05), xlab="Fitted values", ylab="Original values", las=1, cex.lab=1, cex=0.8,  main=list(paste("Probability of", names(mo$model)[1]),cex=0.8) )
      abline(0,1, lty=3)
      t.breaks <- cut(fitted(mo), quantile(fitted(mo)))
      means <- tapply(mo$model[,1], t.breaks, mean)
      semean <- function(x) sd(x)/sqrt(length(x))
      means.se <- tapply(mo$model[,1], t.breaks, semean)
      points(quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
      segments(quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
   }

   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
  
   # variables
     scatter={} 
     for (i in rownames(summary(mo)$coef)) {
          #i = "lat_abs"
        j=sub("\\).*", "", sub(".*\\(", "",i)) 
        scatter[length(scatter)+1]=j
      }
      x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                      log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
      for (i in 1:length(fixed)){
          jj =fixed[i]
          variable=dat[, ..jj][[1]]
          if(trans[i]=='log'){
          scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='abs'){
          scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='sin'){
            scatter.smooth(resid(mo)~sin(variable),xlab=paste('sin(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='cos'){
            scatter.smooth(resid(mo)~cos(variable),xlab=paste('cos(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          } else {  
          scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }
       }
      
      if(length(categ)>0){
        for(i in categ){
           variable=dat[, ..i][[1]]
            boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
           }
      }     
          
   if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
        }
   if(spatial == TRUE){    
      spdata=data.frame(resid=resid(mo), x=dat$Longitude, y=dat$Latitude)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
      plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
        legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
      plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
      plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
        }
   
   mtext(title, side = 3, line = -1, cex=0.7,outer = TRUE)
   
   if(PNG==TRUE){dev.off()}
  }  

# END
