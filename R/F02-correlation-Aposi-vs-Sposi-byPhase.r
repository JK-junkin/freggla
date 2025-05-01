
getwd()
options(max.print = 100, width = 80)

# install.packages("needs")
needs::needs(tidyverse, ggrepel, ggpp)
dir()

# read data --------------------------------------------------------------------
outd <- "path/to/your/output/folder"
gs <- readRDS(file.path(outd, "itmfile/overlap_year_v0.rds"))

phases <- tibble::tribble(
    ~ start  , ~ end    ,
    19780101 , 19881231 ,
    19890101 , 19981231 ,
    19990101 , 20101231 ,
    20110101 , 20210101 ,
) %>% dplyr::mutate_all(.funs = lubridate::ymd)

add_phase <- function(df, year_colname) {
    df %>%
    dplyr::mutate(dm1 = lubridate::make_date({{ year_colname }}, month, 15),
                  phase = dplyr::case_when(
                      between(dm1, phases[[1, 1]], phases[[1, 2]]) ~ "Phase-1",
                      between(dm1, phases[[2, 1]], phases[[2, 2]]) ~ "Phase-2",
                      between(dm1, phases[[3, 1]], phases[[3, 2]]) ~ "Phase-3",
                      between(dm1, phases[[4, 1]], phases[[4, 2]]) ~ "Phase-4",
                      TRUE ~ NA_character_)) %>%
    dplyr::select(-dm1)
}

make_cooc <- function(df) {
    df %>%
        dplyr::select(asctg, year2, val, phase) %>%
        dplyr::mutate(asctg = forcats::fct_collapse(asctg,
                                                    yval = c("S-domi10", "S-domi2",
                                                             "S-domi", "A-domi",
                                                             "A-domi2", "A-domi10")),
                      asctg = forcats::fct_drop(asctg)) %>%
        dplyr::group_by(phase, year2, asctg) %>%
        dplyr::summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = "asctg", values_from = "val")
}

# Correlation analyses #########################################################
## Area
gs$A_anc$layers

unique(gs$A_anc$data$asctg)
# [1] S-domi10 S-domi2  S-domi   A-domi   A-domi2  A-domi10 A-posi  
# Levels: S-domi10 S-domi2 S-domi A-domi A-domi2 A-domi10 A-posi

anc <- gs$A_anc$data %>% dplyr::filter(asctg %in% "A-posi") %>% add_phase(year2)
sar <- gs$A_sar$data %>% dplyr::filter(asctg %in% "S-posi") %>% add_phase(year2)

Area <- dplyr::bind_rows(anc, sar) %>%
    dplyr::select(asctg, year2, val, phase) %>%
    tidyr::pivot_wider(names_from = "asctg", values_from = "val")

Area_a <- gs$A_anc$data %>% add_phase(year2) %>% make_cooc()
Area_s <- gs$A_sar$data %>% add_phase(year2) %>% make_cooc()

### Correlation coefficients and p values
# Aposi vs. Sposi
cor_whole_ApSp <- with(data = Area, cor.test(`A-posi`, `S-posi`, method = "pearson"))
c(cor_whole_ApSp$estimate, cor_whole_ApSp$p.value)

# Aposi vs. Co-occurrance
cor_whole_ApCo <- dplyr::filter(Area_a, year2 >= 1979) %>% 
    with(cor.test(`A-posi`, `yval`, method = "pearson"))
c(cor_whole_ApCo$estimate, cor_whole_ApCo$p.value)

# Sposi vs. Co-occurrance
cor_whole_SpCo <- with(data = Area_s, cor.test(`S-posi`, `yval`, method = "pearson"))
c(cor_whole_SpCo$estimate, cor_whole_SpCo$p.value)

## SAVE analisis objects #######################################################
save(cor_whole_ApSp, cor_whole_ApCo, cor_whole_SpCo,
     file = file.path(outd, "itmfile/correlations_whole.Rda"))
# --> NOTE: these objects are used in T01-multi-ttest-byPhase.r
################################################################################

# label in figure -------------------------------------------------------------
cors <- Area %>% dplyr::filter(phase == "Phase-4") %>%
    with(cor.test(`S-posi`, `A-posi`, method = "pearson"))
cors

names(cors)
cors$p.value
cors$estimate

stringi::stri_escape_unicode("–")
# [1] "\\u2013" # NOTE: endash
stringi::stri_unescape_unicode("\u2212") # NOTE: hyphen
# [1] "−"       

lab <- sprintf('italic(r)=="\u2212%.4f,"~italic(p)=="%.4f"',
               abs(cors$estimate), cors$p.value)
lab
# --> NOTE: OK

Alab <- Area %>%
    dplyr::group_by(phase) %>%
    tidyr::nest() %>%
    dplyr::mutate(fit = purrr::map(.x = data,
                                   .f = ~ cor.test(.$`S-posi`, .$`A-posi`, 
                                                   method = "pearson")),
                  lab = purrr::map(fit,
                                   ~ ifelse(.$estimate >= 0,
                                            sprintf('italic(r)=="  %.4f,"~italic(p)=="%.4f"',
                                                    .$estimate, .$p.value),
                                            sprintf('italic(r)=="\u2212%.4f,"~italic(p)=="%.4f"',
                                                    abs(.$estimate), .$p.value)))) %>%
    dplyr::select(phase, lab) %>%
    tidyr::unnest(c(lab)) %>%
    dplyr::ungroup()

## Figure by Phase #############################################################
myt <- theme_classic(base_family = "Arial", base_line_size = 0.2, base_size = 11) +
    theme(axis.text = element_text(color = "black", size = 9),
          plot.title = element_text(size = 11, family = "Arial Bold"),
          panel.background = element_blank(),
          panel.border = element_rect(linewidth = 0.5, color = "black", fill = NA))

colorspace::swatchplot(palette.colors())
dev.off()

gcorr <- Area %>% 
    ggplot(data = ., aes(x = `A-posi`, y = `S-posi`, group = phase)) +
    geom_smooth(data = . %>% dplyr::filter(phase == "Phase-4"), 
                aes(color = phase), se = FALSE,
                formula = y ~ x, method = "lm", linewidth = 0.8, linetype = "21") +
    geom_path(aes(x = `A-posi`, y = `S-posi`),
              linewidth = 0.15, color = "gray50", inherit.aes = FALSE) +
    geom_point(aes(fill = phase, shape = phase), size = 1.6, stroke = 0.2) +
    geom_text_repel(data = . %>% dplyr::filter(year2 - 3 * floor(year2/3) == 1),
                    aes(label = year2), family = "Arial", size = 2.5,
                    label.padding = 0.05, box.padding = 0.3,
                    max.time = 60, segment.color = "gray0",
                    min.segment.length = 0.01, segment.size = 0.3) + 
    # NOTE:(Coding hack) Have to add this layer to combine legend.
    geom_text_npc(data = Alab, aes(label = lab, color = phase), parse = TRUE,
                  npcx = 0.58, npcy = seq(0.65, by = -0.04, length.out = 4), 
                  hjust = 0, vjust = 0, family = "Arial", size = 2.5, alpha = 0) +
    scale_shape_manual(values = c(21, 25, 22, 24),
                       breaks = paste0("Phase-", 1:4)) +
    scale_color_manual(values = unname(palette.colors()[c(6, 9, 7, 5)]),
                       breaks = paste0("Phase-", 1:4)) +
    scale_fill_manual(values = unname(palette.colors()[c(6, 9, 7, 5)]),
                      breaks = paste0("Phase-", 1:4)) +
    scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    guides(x = guide_axis(minor.ticks = TRUE), 
           y = guide_axis(minor.ticks = TRUE)) +
    myt +
    theme(aspect.ratio = 1/1,
          panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
          legend.title = element_text(hjust = 0.5),
          legend.spacing.y = unit(1, "pt"),
          legend.margin = margin(2, 2, 2, 2),
          legend.background = element_rect(color = "black", linewidth = 0.2),
          legend.key.height = unit(8, "pt"),
          legend.position = "inside",
          legend.position.inside = c(0.99, 0.99),
          legend.justification = c(1, 1),
          plot.margin = margin(0, 1, 0, 1),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10)) +
    labs(x = bquote("Annual area of A-positive"~(10^3~km^2)),
         y = bquote("Annual area of S-positive"~(10^3~km^2)),
         color = "Phase", fill = "Phase", shape = "Phase")

ggsave(gcorr, file = file.path(outd, "fig2.png"), dpi = 500, unit = "mm", w = 81, h = 79)  
