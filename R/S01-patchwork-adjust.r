
getwd()
options(width = 80, max.print = 150)

outd <- "path/to/your/output/folder"
ind <- file.path(outd, "itmfile")

needs::needs(tidyverse, patchwork, RColorBrewer, ggpp, gginnards, cowplot) 

## Figure 1 ####################################################################
dir(ind)
# [1] "overlap_year_v0.rds"

Aa <- readRDS(file = file.path(ind, "overlap_year_v0.rds"))$A_anc
As <- readRDS(file = file.path(ind, "overlap_year_v0.rds"))$A_sar
Ea <- readRDS(file = file.path(ind, "overlap_year_v0.rds"))$E_anc
Es <- readRDS(file = file.path(ind, "overlap_year_v0.rds"))$E_sar
Esz <- readRDS(file = file.path(ind, "overlap_year_v0.rds"))$Esar_z

11/43 / 2
# [1] 0.127907
11/43 + (10/43 / 2)
# [1] 0.372093
21/43 + (12/43 / 2)
# [1] 0.627907
33/43 + (10/43 / 2)
# [1] 0.8837209

phase_names <- data.frame(nx = c(0.128, 0.372, 0.628, 0.884), 
                          ny = 0.99,
                          nm = paste("Phase", 1:4, sep = "-"))
# [1] "Phase-1" "Phase-2" "Phase-3" "Phase-4"

legend_blank <- 
    data.frame(asctg = c(paste("A", c("positive", "dominant10", "dominant2", "dominant"), sep = "-"),
                         paste("S", c("positive", "dominant10", "dominant2", "dominant"), sep = "-")),
               val = 1:8) %>%
    dplyr::mutate(asctg = forcats::fct_inorder(asctg))
ctgcolors <- brewer.pal(8, "RdBu")

addlist <- list(geom_text_npc(data = phase_names, 
                              aes(npcx = nx, npcy = ny, label = nm),
                              vjust = 1, hjust = 0.5, size = 2.8, family = "Arial"),
                theme(axis.title.y = element_blank())) 

p <- ggplot() + labs(x = "Year", 
                     y = bquote("Spawning area summed"~(10^3~km^2)))
p2 <- ggplot() + labs(x = NULL, 
                      y = bquote("Egg abundance"~(10^12)))
p3 <- ggplot() + 
    geom_tile(data = legend_blank, aes(x = val, y = val, fill = asctg)) +
    scale_fill_manual(values = c(ctgcolors[1:4], ctgcolors[8:5]),
                      name = NULL) +
    guides(fill = guide_legend(ncol = 2,
                               keywidth = unit(12, "pt"),
                               keyheight = unit(9, "pt"))) +
    theme(legend.background = element_rect(color = "black", linewidth = 0.25),
          legend.margin = margin(4, 4, 4, 4))

xtit <- cowplot::get_plot_component(p, "xlab-b")
ytit <- cowplot::get_plot_component(p, "ylab-l")
ytit2 <- cowplot::get_plot_component(p2, "ylab-l")
plot_components(p3)
plot_component_names(p3)
ledg <- cowplot::get_plot_component(p3, "guide-box-right")

design <- "
FAIGB
FCIGD
#HIGE
"

g <- list(
    (delete_layers(As, idx = 4) + addlist +
        geom_text_npc(npcx = 0.35, npcy = 0.90, label = "(a) Sardine",
                      vjust = 1, hjust = 0, size = 3.0, family = "Arial")), # A
    (Es + addlist +
        geom_text_npc(npcx = 0.25, npcy = 0.90, label = "(c) Sardine",
                      vjust = 1, hjust = 0, size = 3.0, family = "Arial")), # B
    (delete_layers(Aa, idx = 4) + addlist + 
        geom_text_npc(npcx = 0.03, npcy = 0.90, label = "(b) Anchovy",
                      vjust = 1, hjust = 0, size = 3.0, family = "Arial")), # C
    (Esz + addlist +
        geom_text_npc(npcx = 0.45, npcy = 0.90, label = "(d) Sardine zoomed",
                      vjust = 1, hjust = 0, size = 3.0, family = "Arial")), # D
    (Ea + addlist +
        geom_text_npc(npcx = 0.03, npcy = 0.90, label = "(e) Anchovy",
                      vjust = 1, hjust = 0, size = 3.0, family = "Arial")), # E
    ytit,  # F
    ytit2, # G
    #xtit,  # H
    ledg,  # H
    ggplot() + geom_blank() + theme_void() # I
) |> wrap_plots() +
    plot_layout(heights = c(50, 50, 50), widths = c(3, 50, 3, 3, 50),
                design = design) & 
    theme(plot.margin = margin(t = 1, r = 4, 0, 0),
          legend.position = "none",
          axis.text.x.bottom = element_text(color = "black", size = rel(1.1)))

ggsave(g, file = file.path(outd, "fig1.png"), wid = 169, hei = 123, unit = "mm",
       dpi = 500)
################################################################################

## Figure 3 ####################################################################
dir(ind)
# [1] "overlap_year_v0.rds" "STIs.rds"           

STI <- readRDS(file = file.path(ind, "STIs.rds"))$STI
STIb <- readRDS(file = file.path(ind, "STIs.rds"))$STIbar

STIr <- STI +
    theme(legend.position.inside = c(0.30, 0.99)) +
    geom_text_npc(data = data.frame(nx = 0.02, ny = c(0.99, 0.99, 0.99, 0.99, 0.99),
                                    phase = c(paste0("Phase-", 1:4), "TheEntire"),
                                    label = paste0("(", letters[1:5], ")")),
                  aes(npcx = nx, npcy = ny, label = label))

STIbr <- 
    STIb + 
    coord_cartesian(xlim = c(10, 31), clip = "off") +
    guides(color = guide_legend(nrow = 1), shape = "none") +
    theme(aspect.ratio = 6.7/10, 
          legend.position = "top",
          legend.justification = c(1, 1),
          legend.box.spacing = unit(1, "pt"),
          plot.margin = margin(t = 1, l = 1),
          axis.title.x.bottom = element_blank())

g2 <- STIr + inset_element(STIbr, 0.50, -0.04, 1.00, 0.31)
ggsave(g2, file = file.path(outd, "fig3.pdf"), 
       wid = 169, hei = 140, unit = "mm", device = cairo_pdf)

# NOTE: https://www.stat.auckland.ac.nz/~paul/Reports/gggrid/gggrid.html
# NOTE: https://stackoverflow.com/questions/75211964/plot-grobs-on-ggplot-faceted-graph
cairo_pdf(file = file.path(outd, "fig3.pdf"), wid = 16.9/2.54, hei = 14.5/2.54)
plot(g2, newpage = FALSE)
plab <- textGrob("(f)", x = unit(0.56, "npc"), y = unit(0.35, "npc"), just = c("left", "top"))
grid.draw(plab)
dev.off()

png(file = file.path(outd, "fig3.png"), wid = 16.9, hei = 14.5, res = 500,
    units = "cm")
plot(g2, newpage = FALSE)
plab <- textGrob("(f)", x = unit(0.56, "npc"), y = unit(0.35, "npc"), just = c("left", "top"))
grid.draw(plab)
dev.off()
################################################################################
