
getwd()
options(max.print = 1250, width = 80)

# install.packages("needs")
needs::needs(tidyverse, RColorBrewer, gginnards, sf, rnaturalearth, rnaturalearthdata)

outd <- "path/to/your/output/folder"

# read data --------------------------------------------------------------------
source("Func_read_data.R")
source("Func_world_map.R")

ed <- load_eldata(species = c("Sardine", "Anchovy"), gridsize = 15, add_env = TRUE)

phases <- tibble::tribble(
    ~ start  , ~ end    ,
    19780101 , 19881231 ,
    19890101 , 19981231 ,
    19990101 , 20101231 ,
    20110101 , 20210101 ,
) %>% dplyr::mutate_all(.funs = lubridate::ymd)

# Drawing ----------------------------------------------------------------------
maptheme <-
    theme_bw(base_family = "sans", base_line_size = 0.2, base_size = 10) +
    theme(axis.text = element_text(color = "black"),
          panel.background = element_rect(fill = NA, color = "black"),
          plot.margin = margin(),
          axis.ticks = element_line(linewidth = 0.08),
          legend.key = element_rect(linewidth = 0.1),
          legend.margin = margin(0, 0, 0, 0),
          legend.spacing = unit(0, "pt"))

# TEP ##########################################################################
devn <- 7
mod <- 6

tmp_proc <- function(df, mod, yr_colnm) {
    df %>%
    dplyr::mutate(dm1 = lubridate::make_date({{ yr_colnm }}, month, 15),
                  phase = dplyr::case_when(
                      between(dm1, phases[[1, 1]], phases[[1, 2]]) ~ "Phase-1",
                      between(dm1, phases[[2, 1]], phases[[2, 2]]) ~ "Phase-2",
                      between(dm1, phases[[3, 1]], phases[[3, 2]]) ~ "Phase-3",
                      between(dm1, phases[[4, 1]], phases[[4, 2]]) ~ "Phase-4",
                      TRUE ~ NA_character_),
                  SSTclass = floor((SST_mean - mod + 5E-16) / devn) * devn + mod,
                  SSTclass = dplyr::if_else(SSTclass < devn + mod, mod, SSTclass),
                  SSTclass = dplyr::if_else(SSTclass > 3 * devn + mod, 3 * devn + mod, SSTclass)) %>%
    dplyr::select(-dm1)
}

anc <- dplyr::filter(ed, species == "Anchovy") %>% 
    tmp_proc(mod = mod, yr_colnm = year) %>%
    dplyr::group_by(phase, SSTclass, Long, Lati) %>%
    dplyr::summarise(val = sum(value, na.rm = TRUE) / 1e12, .groups = "drop",
                     n = sum())

sar <- dplyr::filter(ed, species == "Sardine") %>%
    tmp_proc(mod = mod, yr_colnm = year) %>%
    dplyr::group_by(phase, SSTclass, Long, Lati) %>%
    dplyr::summarise(val = sum(value, na.rm = TRUE) / 1e12, .groups = "drop")

par(mfrow = c(2, 1), mar = c(4, 4, 1, 0))
hist(anc$val)
hist(sar$val)
dev.off()

## parameters for plot =========================================================
minval <- min(c(anc$val, sar$val), na.rm = TRUE)
maxval_a <- max(anc$val, na.rm = TRUE)
maxval_s <- max(sar$val, na.rm = TRUE)

jet.colors <-
    colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                       "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

lab_phase <- paste0("Phase-", 1:4, ": ", 
                    phases %>% 
                        dplyr::mutate(per = paste(year(start),
                                                  year(end - days(1)),
                                                  sep = "–")) %>%
                        dplyr::pull(per))
names(lab_phase) <- paste0("Phase-", 1:4)
lab_phase <- as_labeller(lab_phase)

lab_SST <- cut(1, breaks = sstborders, include.lowest = TRUE, right = FALSE) %>%
    levels() %>% paste(., "°C")
names(lab_SST) <- sort(unique(anc$SSTclass))
lab_SST <- as_labeller(lab_SST)

sort(unique(anc$SSTclass))
range(ed$SST_mean, na.rm = TRUE)
sstborders <- c(0, sort(unique(anc$SSTclass))[-1], 35)
# [1]  0 13 20 27 35

# Overlap ######################################################################
d <-
    ed %>%
    dplyr::mutate(year2 = dplyr::if_else(species == "Sardine" & month %in% 10:12,
                                         year + 1L, year)) %>%
    # NOTE: For sardine, use the same years as Fig. 1
    dplyr::filter(!(species == "Sardine" & year2 %in% c(1978, 2021))) %>%
    tmp_proc(mod = mod, yr_colnm = year2) %>%
    dplyr::group_by(phase, SSTclass, Long, Lati, species) %>%
    dplyr::summarise(val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "species", values_from = "val") %>%
    dplyr::rename(a = Anchovy, s = Sardine) %>%
    dplyr::mutate(asegg = rowSums(.[, c("a", "s")], na.rm = TRUE),
                  aps   = a/s,
                  asctg = dplyr::case_when(aps == Inf             ~ "A-p",
                                           aps >  10              ~ "A-d10",
                                           aps >  2   & aps <= 10 ~ "A-d2",
                                           aps >  1   & aps <= 2  ~ "A-d",
                                           aps >= 0.5 & aps < 1   ~ "S-d",
                                           aps >= 0.1 & aps < 0.5 ~ "S-d2",
                                           aps >  0   & aps < 0.1 ~ "S-d10",
                                           aps == 0               ~ "S-p",
                                           is.nan(aps)            ~ "None",
                                           TRUE            ~ NA_character_)) %>%
    tidyr::pivot_longer(cols = `s`:`aps`, names_to = "spec", values_to = "val")

sapply(d, unique)
unique(d$asctg)
# [1] "None"  "S-p"   "S-d2"  "S-d10" "A-p"   "A-d2"  "S-d"   "A-d"   "A-d10"

range(d$Long)
range(d$Lati)

d %>%
    dplyr::filter(val != 0) %>% 
    dplyr::group_by(spec) %>% 
    dplyr::reframe(scientific(range(val, na.rm = TRUE)))

sapply(d, range, na.rm = TRUE)

# histograms of egg abundance
{
    par(mfrow = c(2, 1))
    d %>% filter(spec == "asegg", val != 0) %>% pull(val) %>% hist()
    d %>% filter(spec == "asegg", val != 0) %>% pull(val) %>% log10() %>% hist()
}
dev.off()

## overlap categories
asctgorder <- c(paste("A", c("p", "d10", "d2", "d"), sep = "-"),
                paste("S", rev(c("p", "d10", "d2", "d")), sep = "-"))

asratio <- d %>%
    dplyr::filter(spec == "aps") %>%
    dplyr::mutate(asctg = forcats::fct_relevel(asctg, !!asctgorder),
                  asctg = forcats::fct_recode(asctg,
                                              `A-posi`   = "A-p",
                                              `A-domi10` = "A-d10",
                                              `A-domi2`  = "A-d2",
                                              `A-domi`   = "A-d",
                                              `S-posi`   = "S-p",
                                              `S-domi10` = "S-d10",
                                              `S-domi2`  = "S-d2",
                                              `S-domi`   = "S-d"))
unique(asratio$asctg)

# colors
rdbu <- RColorBrewer::brewer.pal(n = 8, "RdBu")

gmap <- gginnards::delete_layers(g1, idx = c(1:7)) %+% 
    dplyr::filter(asratio, !is.na(SSTclass)) +
    ggplot() +
    geom_point(data = . %>% dplyr::filter(asctg == "None"),
               aes(x = Long + 0.25/2, y = Lati + 0.25/2),
               shape = 4, stroke = 0.15, size = rel(0.25), color = "gray50") +
    geom_tile(data = . %>% dplyr::filter(asctg != "None"), alpha = 1,
              aes(x = Long + 0.25/2, y = Lati + 0.25/2, fill = asctg)) +
    wmap_sf(as_gg = FALSE, lgl = c(128.5, 152.0), ltl = c(28.5, 43.5),
            lgb = seq(130, 160, by = 10), ltb = seq(25, 50, by = 5),
            color = "black", linewidth = 0.15, fill = "gray80") +
    scale_fill_manual(values = rdbu,
                      name = "Overlap\ncategory") +
    facet_grid(rows = vars(SSTclass), cols = vars(phase),
               labeller = labeller(SSTclass = lab_SST, phase = lab_phase),
               axes = "all", axis.labels = "margins") +
    labs(x = "Longitude", y = "Latitude", 
         fill = bquote("Anchovy\nTEP"~(10^12))) +
    maptheme +
    theme(axis.text = element_text(size = rel(0.7)),
          legend.background = element_rect(linewidth = 0.2, color = "black"),
          legend.box.background = element_blank(),
          legend.box.margin = margin(2, 2, 2, 2),
          legend.box.spacing = unit(2, "pt"),
          legend.direction = "horizontal",
          legend.justification = "right",
          legend.key = element_blank(),
          legend.margin = margin(2, 2, 2, 2),
          legend.position = "bottom",
          legend.spacing.x = unit(4, "pt"),
          legend.spacing.y = unit(4, "pt"),
          legend.text = element_text(margin = margin()),
          legend.title = element_text(size = rel(0.8)),
          panel.spacing = unit(0, "pt"),
          plot.margin = margin(b = 0),
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1.1), face = "bold"),
          strip.text.x.top = element_text(margin = margin(0, 0, 1, 0)),
          strip.text.y.right = element_text(margin = margin(0, 0, 0, 2))) +
    guides(fill = guide_legend(nrow = 1, title.position = "left",
                               label.position = "bottom",
                               keywidth = unit(2.2, "line"),
                               keyheight = unit(6.0, "pt")))

ggsave(gmap, file = file.path(outd, "fig4.png"), w = 169, h = 145, unit = "mm", dpi = 500)
