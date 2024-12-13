library(RColorBrewer)
library(gplots)
library(ggplot2)
library(umap)
library(readxl)


makeplot <- function(dat, i){
  dat$colorby <- dat[,colnames(dat)[i]]
  ggplot(dat,aes(x,y,color=colorby)) + 
    geom_point() + 
    xlab("UMAP1") + ylab("UMAP2") + 
    scale_color_discrete(name = colnames(dat)[i])
}

get_umap <- function(cursheet){
  inp_file <- "cluster 240610 for analysis.xlsx"

  print(cursheet)
  dat <- as.data.frame(readxl::read_excel(inp_file,sheet = cursheet))
  
  num_meta <- which(colnames(dat)=="Donor")
  dat_only <- as.data.frame(dat[,-c(1:num_meta)])
  
  if(num_meta==3){
    rownames(dat_only) <- paste(dat[,1],dat[,2],dat[,3],sep=" ")
  } 
  if(num_meta==4){
    rownames(dat_only) <- paste(dat[,1],dat[,2],dat[,3],dat[,4],sep=" ")
  } 
  
  
  ######## UMAPs
  us <- umap::umap.defaults
  us$n_neighbors <- min(us$n_neighbors, nrow(dat_only))
  out <- umap::umap(dat_only, config = us)
  dat$x <- out$layout[,1]
  dat$y <- out$layout[,2]
  dat

}

################################################################################
############################ For Fig 4e ########################################
################################################################################

umap_t0 <- get_umap("T0 BULK FREQ")
umap_t1 <- get_umap("SPEC STIM T1 BULK")
umap_t2 <- get_umap("SPEC STIM T2 BULK")

thelev <- c("NN", "NH", "HH", "N", "H")
umap_t0$`Oxygen condition` <- factor(umap_t0$`Oxygen condition`, levels=thelev)
umap_t1$`Oxygen condition` <- factor(umap_t1$`Oxygen condition`, levels=thelev)
umap_t2$`Oxygen condition` <- factor(umap_t2$`Oxygen condition`, levels=thelev)

makewhite <- function(p) p + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.background = element_blank())

#https://lospec.com/palette-list/ibm-color-blind-safe
add_oxy_color <- function(p) p + scale_colour_manual(
  #values = c("#dc267f","#ffb000","#648fff",    "#dc267f","#648fff"),  #find 3 color palette color blind
  values = c("#3d85c6","#9900ff","#ff0000",    "#3d85c6","#ff0000"),  #find 3 color palette color blind
  breaks = thelev,
  name = ""
)


add_donor_color <- function(p) p + scale_colour_manual(
#  values = c("#648fff","#785ef0","#dc267f","#fe6100","#ffb000"),#,#"#000000"), 
  values = c("#38761d","#e06666","#ecb50e","#00ffff","#999999"),#,#"#000000"), 
  breaks = c("BC005" , "BC964",  "BC3035", "BC173" , "BC5353"),#,
  name = ""
)

####### oxy
p1o <- add_oxy_color(makewhite(makeplot(umap_t0, 2)))# + guides(fill=guide_legend(title="New Legend Title"))
p2o <- add_oxy_color(makewhite(makeplot(umap_t1, 2)))
p3o <- add_oxy_color(makewhite(makeplot(umap_t2, 2)))

#ptot_oxy <- egg::ggarrange(p1,p2,p3, ncol=1)
#ptot_oxy


###### donor
p1d <- add_donor_color(makewhite(makeplot(umap_t0, 3)))
p2d <- add_donor_color(makewhite(makeplot(umap_t1, 3)))
p3d <- add_donor_color(makewhite(makeplot(umap_t2, 3)))

ptot <- egg::ggarrange(
  p1o,p1d,
  p2o,p2d,
  p3o,p3d,
  ncol=2)
ptot
ggsave(plot = ptot, "out/fig4.pdf", width = 8, height = 8)




################################################################################
############################ For Fig S4 ########################################
################################################################################



umap_t1 <- get_umap("NONSPEC STIM T1 BULK")
umap_t2 <- get_umap("NONSPEC STIM T2 BULK")


thelev <- c("NN", "NH", "HH", "N", "H")
umap_t1$`Oxygen condition` <- factor(umap_t1$`Oxygen condition`, levels=thelev)
umap_t2$`Oxygen condition` <- factor(umap_t2$`Oxygen condition`, levels=thelev)

makewhite <- function(p) p + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                panel.background = element_blank())



add_cd_color <- function(p) p + scale_colour_manual(
  values = c("#648fff","#dc267f"), 
  breaks = c("CD4","CD8"),#,
  name = ""
)

####### oxy
p2o <- add_oxy_color(makewhite(makeplot(umap_t1, 2)))
p3o <- add_oxy_color(makewhite(makeplot(umap_t2, 2)))

###### donor
p2d <- add_donor_color(makewhite(makeplot(umap_t1, 3)))
p3d <- add_donor_color(makewhite(makeplot(umap_t2, 3)))


####### cd4/8
p2c <- add_cd_color(makewhite(makeplot(umap_t1, 1)))
p3c <- add_cd_color(makewhite(makeplot(umap_t2, 1)))

ptot <- egg::ggarrange(
  p2c,p3c,
  p2o,p3o,
  p2d,p3d,
  ncol=2)
ptot
ggsave(plot = ptot, "out/subfig4.pdf", width = 8, height = 8)




