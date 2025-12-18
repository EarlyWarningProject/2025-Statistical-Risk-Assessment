
#libraries
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(ggridges)
library(ggrepel)

#color scales
library(viridis)
library(scales)
library(RColorBrewer)

#set colors
ggplot.red <- hue_pal()(3)[1]
ggplot.green <- hue_pal()(3)[2]
ggplot.blue <- hue_pal()(3)[3]
ggplot.teal <- hue_pal()(2)[2]

margin <- theme(plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"))

base_size = 10

theme_facet <- function() {theme(
    strip.background = element_blank(),
    #strip.text.x = element_text(face="bold"),
    #strip.text.y = element_text(face="bold"),
    strip.text = element_text(hjust = 0, size=base_size))}

theme_tile <- function() {theme(panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank())}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right"), which=1, sep=0) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[which]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none") + theme(plot.margin = unit(c(0,sep,0,sep), "cm")))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
}