# remove white space and reexport (the final exported size should be reduced by 10% to 85mm)
    #x <- image_read(here::here("Output/Fig_tree_v7_ss.png"), density=300)
    #y <- image_trim(x) # width = 94.5cm, height = 88
    #image_write(y, path = "Output/Fig_1C_width-85mm.png", format = "png", density = 300)

#TODO:adjust position of the densityplot and try the fig without (b) tag lable to see whether it fits better on the page

require("ggtext")
require('ggtree')
library('ggimage')
library('magick')
font_size = 2.5
ladderize_ = TRUE
source(here::here("R/z_offspring.R"))
source(here::here("R/z_as-tibble.R"))
source(here::here("R/z_ancestor.R"))
add_class <- function(x, name) {
    xx <- setdiff(name, class(x))
    if (length(xx) > 0) {
        class(x) <- base::union(xx, class(x))
    }
    return(x)
}

getnode <- function(...) {
    if (hasArg(env)) {
        env <- list(...)$env
    } else {
        env <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    }
    xy <- unlist(locator(n = 1))
    points(xy[1], xy[2]) 
    d <- sqrt((xy[1] - env$xx)^2 + (xy[2] - env$yy)^2)
    ii <- which(d == min(d))[1]
    ii
}


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
    geom_point(
            aes(x = x, y = y, size = sqrt(abs(pic/pi))),
            fill = "grey90", color = "grey50", pch = 21) +
    scale_size_area(max_size = 4) +
    coord_cartesian(xlim = c(0,110))+
    guides(size = "none") + 
    #labs(tag = '(b)') +
    #theme_tree2()+
    theme_MB + 
    theme(  legend.position="none",
            panel.border = element_blank()
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

p_g

# use this to add horizontal lines
#p_g = p_g + geom_tiplab(aes(subset = (node %in% c(1)), label = ""), offset = 27, color = "lightgrey", align = TRUE, linetype = 1, vjust = 1, linesize = font_size) # treeid[treeid$label == 'xenicus_gilviventris','label']


# add scale 
qn <- scales::rescale(quantile(ds$r), probs = seq(0, 1, length.out = length(cols_f1)))

dens <- density(ds$r, n = 2^12)
den <- data.table(x = dens$x, y = dens$y)
#den <- den[x > log10(0.99) & x < log10(50.01)]

f1b_l <-
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
f2_tree = p_g + theme(legend.position = "none") + inset_element(f1b_l, 
left = 0.10, right = 0.30,
bottom = 0.03, top = 0.308, 
on_top = TRUE, align_to = "full")

ggsave(here::here("Output/Fig_tree_v4.png"), f2_tree, width = 11, height = 10, units ='cm')

library(magick)
x <- image_read(here::here("Output/Fig_tree_v4.png"), density=300)

image_write(image_trim(x), path = "Output/Fig_tree_v4_trim.png", format = "png", density = 300)

ggsave(here::here("Output/Fig_tree_v4_trim.png"), f2_tree, width = 9.9, height = 9, units ='cm')

ggsave(here::here("Output/Fig_tree_v5.png"), f2_tree, width = 9.9, height = 9, units ='cm', scale=0.9)




x <- image_read(here::here("Output/Fig_tree_v4.png"), density=300)
y <- image_trim(x)
panel_border <- theme(panel.border=element_rect(colour='black', 
                                            fill=NA, size=2))
xx <- image_ggplot(x) + panel_border
yy <- image_ggplot(y) + panel_border
plot_list(xx, yy, tag_levels = "A", ncol=2)

#ggsave(here::here("Output/Fig_1.png"), gmos / f1b_d + plot_layout(heights = c(7.5, 15)), width = 15, height = 15 + 7.5, units = "cm")
#f1b_d