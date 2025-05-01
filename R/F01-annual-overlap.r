
getwd()
options(width = 80, max.print = 1500)

# install.packages("needs")
needs::needs(tidyverse, gginnards, foreach, RColorBrewer, openxlsx)

outd <- "path/to/your/output/folder"

# read data --------------------------------------------------------------------
source("Func_read_data.R")

ed <- load_eldata(species = c("Sardine", "Anchovy"), gridsize = 15)

phases <- tibble::tribble(
    ~ start  , ~ end    ,
    19780101 , 19881231 ,
    19890101 , 19981231 ,
    19990101 , 20101231 ,
    20110101 , 20210101 ,
) %>% dplyr::mutate_all(.funs = lubridate::ymd)

borderline <- phases %>% dplyr::mutate(bors = year(start)) %>% dplyr::pull(bors)
# [1] 1978 1989 1997 2011

################################################################################ 
asctgorder <- 
    c(paste("A", c("posi", "domi10", "domi2", "domi"), sep = "-"),
      paste("S", rev(c("posi", "domi10", "domi2", "domi")), sep = "-"))

d0 <- ed %>%
    tidyr::replace_na(replace = list(value = 0)) %>%
    dplyr::mutate(year2 = dplyr::if_else(species == "Sardine" & month %in% 10:12,
                                         year + 1L, year)) %>%
    dplyr::distinct(year, month, year2, species)

xtabs( ~ species + year, data = d0)
xtabs( ~ species + year2, data = d0)

d <-
    ed %>%
    tidyr::replace_na(replace = list(value = 0)) %>%
    tidyr::pivot_wider(names_from = "species", values_from = "value", values_fn = sum) %>%
    dplyr::rename(a = Anchovy, s = Sardine) %>%
    dplyr::mutate(aps   = a/s, #) %>% dplyr::pull(aps) %>% unique
                  asctg = dplyr::case_when(aps == Inf             ~ "A-posi",
                                           aps >  10              ~ "A-domi10",
                                           aps >  2   & aps <= 10 ~ "A-domi2",
                                           aps >  1   & aps <= 2  ~ "A-domi",
                                           aps >= 0.5 & aps < 1   ~ "S-domi",
                                           aps >= 0.1 & aps < 0.5 ~ "S-domi2",
                                           aps >  0   & aps < 0.1 ~ "S-domi10",
                                           aps == 0               ~ "S-posi",
                                           is.nan(aps)            ~ "None",
                                           TRUE ~ NA_character_)) %>%
    dplyr::select(year:area, s, a, asctg) %>%
    dplyr::filter(asctg != "None") %>% # Remove 0/0
    dplyr::mutate(asctg = forcats::fct_relevel(asctg, !!asctgorder))

# Anchovy or Sardine categories: positive(= only), dominant
unique(d$asctg)
# [1] S-posi   S-domi   A-posi   S-domi2  S-domi10 A-domi2  A-domi10 A-domi  
# Levels: A-posi A-domi10 A-domi2 A-domi S-domi S-domi2 S-domi10 S-posi

# global plot theme ------------------------------------------------------------
myt <- theme_classic(base_family = "Arial", base_line_size = 0.3, 
                     base_size = 10) +
    theme(axis.text = element_text(color = "black"),
          axis.text.x = element_text(size = rel(0.8)),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

## color
fillcolors <- RColorBrewer::brewer.pal(8, "RdBu")

# ------------------------------------------------------------------------------
### AREA ####################################################################### 
# Figs. 1a and 1b
tmp_proc <- function(df, group_year) {
    df %>%
    dplyr::group_by({{ group_year }}, asctg) %>%
    dplyr::summarise(val = sum(area, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    tidyr::complete(asctg, {{ group_year }}, fill = list(val = 0)) %>%
    dplyr::group_by({{ group_year }}) %>%
    dplyr::mutate(avecum = cumsum(val)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange({{ group_year }})
}

d1a <- 
    d %>%
    dplyr::filter(asctg != "S-posi", a > 0) %>%
    dplyr::mutate(asctg = forcats::fct_drop(asctg, "S-posi"),
                  asctg = forcats::fct_relevel(asctg, !!asctgorder[7:1])) %>%
    tmp_proc(group_year = year) %>%
    dplyr::rename(year2 = year)

d1s <- 
    d %>%
    # NOTE: Have to make `year2` here. Never make before pivot_wider()
    dplyr::mutate(year2 = dplyr::if_else(month %in% 10:12, year + 1L, year)) %>%
    dplyr::filter(asctg != "A-posi", s > 0, between(year2, 1979, 2020)) %>%
    dplyr::mutate(asctg = forcats::fct_drop(asctg, "A-posi"),
                  asctg = forcats::fct_relevel(asctg, !!asctgorder[2:8])) %>%
    tmp_proc(group_year = year2)

# Anchovy ----------------------------------------------------------------------
g1a <-
    ggplot(d1a, aes(x = year2)) +
    geom_area(aes(y = val, fill = asctg), linewidth = 0.15, color = "black",
              stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_path(data = . %>% dplyr::filter(asctg == "S-domi"), 
              aes(y = avecum), linewidth = 0.30) +
    geom_vline(xintercept = borderline[-1] - 0.5,
              linewidth = 0.15, linetype = "11") +
    scale_fill_manual(values = fillcolors[7:1], name = "Anchovy") +
    scale_x_continuous(breaks = seq(1970, 2020, by = 5),
                       minor_breaks = 1978:2020,
                       limits = c(1978, 2020),
                       expand = expansion(add = c(0.0, 0.0))) +
    scale_y_continuous(expand = expansion(mult = c(0.0, 0.0)),
                       limits = scales::breaks_extended(n = 2, only.loose = TRUE),
                       breaks = breaks_extended(n = 5, only.loose = TRUE)) +
    labs(x = NULL) +
    guides(fill = guide_legend(title.position = "top", ncol = 1,
                               reverse = TRUE,
                               keywidth = unit(6, "pt"),
                               keyheight = unit(9, "pt")),
           x = guide_axis(minor.ticks = TRUE),
           y = guide_axis(minor.ticks = TRUE)) +
    myt +
    theme(aspect.ratio = 1/2,
          legend.box.spacing = unit(3, "pt"),
          legend.margin = margin())

# Sardine ----------------------------------------------------------------------
g1s <- (gginnards::delete_layers(g1a, idx = 2) %+% d1s +
        geom_path(data = . %>% dplyr::filter(asctg == "A-domi"),
                  aes(y = avecum), linewidth = 0.30) +
        scale_fill_manual(values = fillcolors[2:8], name = "Sardine")) %>%
    gginnards::shift_layers(., i = 2, shift = 1)

### Egg abundance ##############################################################
# Figs. 1c-1e
tmp_proc2 <- function(df, group_year) {
    df %>%
    dplyr::group_by({{ group_year }}, asctg) %>%
    dplyr::summarise(val = sum(egg, na.rm = TRUE) / 1e12, .groups = "drop") %>%
    tidyr::complete(asctg, {{ group_year }}, fill = list(val = 0)) %>%
    dplyr::group_by({{ group_year }}) %>%
    dplyr::mutate(avecum = cumsum(val)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange({{ group_year }})
}

d2a <- 
    d %>%
    dplyr::filter(asctg != "S-posi", a > 0) %>%
    dplyr::rename(egg = a) %>%
    dplyr::mutate(asctg = forcats::fct_drop(asctg, "S-posi"),
                  asctg = forcats::fct_relevel(asctg, !!asctgorder[7:1])) %>%
    tmp_proc2(group_year = year) %>%
    dplyr::rename(year2 = year)

d2s <- 
    d %>%
    # NOTE: Have to make `year2` here. Never make before pivot_wider()
    dplyr::mutate(year2 = dplyr::if_else(month %in% 10:12, year + 1L, year)) %>%
    dplyr::filter(asctg != "A-posi", s > 0, between(year2, 1979, 2020)) %>%
    dplyr::rename(egg = s) %>%
    dplyr::mutate(asctg = forcats::fct_drop(asctg, "A-posi"),
                  asctg = forcats::fct_relevel(asctg, !!asctgorder[2:8])) %>%
    tmp_proc2(group_year = year2)

# Anchovy -----
g1a$layers
g2a <- (gginnards::delete_layers(g1a, idx = 2) %+% d2a +
    theme(axis.text.x = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.spacing.y = unit(1, "pt")) + 
    guides(fill = guide_legend(title.position = "top", ncol = 1,
                               reverse = TRUE,
                               keywidth = unit(5, "pt"),
                               keyheight = unit(5, "pt"))) +
    geom_path(data = . %>% dplyr::filter(asctg == "S-domi"), 
              aes(y = avecum), linewidth = 0.30)) %>%
    gginnards::shift_layers(., i = 2, shift = 1)

# Sardine -----
g1s$layers
g2s <- (gginnards::delete_layers(g1s, idx = 2) %+% d2s +
    theme(axis.text.x = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.spacing.y = unit(1, "pt")) + 
    guides(fill = guide_legend(title.position = "top", ncol = 1,
                               reverse = TRUE,
                               keywidth = unit(5, "pt"),
                               keyheight = unit(5, "pt"))) +
    geom_path(data = . %>% dplyr::filter(asctg == "A-domi"),
              aes(y = avecum), linewidth = 0.30)) %>%
    gginnards::shift_layers(., i = 2, shift = 1)

# Sardine zoomed ---
g2sz <- g2s + coord_cartesian(ylim = c(0, 250))

### SAVE figures ###############################################################
dir.create(file.path(outd, "itmfile"), recursive = TRUE)
saveRDS(tibble::lst(A_anc = g1a, A_sar = g1s, E_anc = g2a, E_sar = g2s, Esar_z = g2sz), 
        file = file.path(outd, "itmfile/overlap_year_v0.rds"))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

## SAVE as Excel ###############################################################
# NOTE: Area: convert unit from 10^4 km^2 to 10^3 km^2
# NOTE: Egg abundance: convert unit from 10^15 to 10^12
wb <- openxlsx::createWorkbook()
modifyBaseFont(wb, fontSize = "10.5", fontName = "Arial")

## arealaps ====================================================================
# NOTE: Areas in each grid were same between two species. Then can use left_join()
arealaps <-
    purrr::map_dfr(list(Anchovy = d1a, Sardine = d1s), .id = "species",
                   .f = ~ dplyr::select(.x, -avecum) %>%
                       tidyr::pivot_wider(names_from = "asctg", values_from = "val")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total   = sum(c_across(-c(species:year2)), na.rm = TRUE),
                  Anchovy = sum(c_across(-c(species:year2, `S-posi`, Total)), na.rm = TRUE),
                  Sardine = sum(c_across(-c(species:year2, `A-posi`, Total, Anchovy)), na.rm = TRUE),
                  Coexist = sum(c_across(matches("-domi"))),
                  `A-dom` = sum(c_across(starts_with("A-domi"))),
                  `S-dom` = sum(c_across(starts_with("S-domi")))) %>%
    dplyr::ungroup() %>% as.data.frame()

any(is.na(arealaps))

areaperc <- 
    arealaps %>%
    dplyr::mutate(across(c(`S-domi10`:`S-posi`, Anchovy:`S-dom`),
                         ~ .x * 100 / Total, .names = "% {.col}")) %>%
    dplyr::select(species:year2, starts_with("%")) %>% as.data.frame()

# statistical values (e.g. Mininum, Maximun, etc..)
add_phase <- function(df, .col) {
    df %>%
    dplyr::mutate(dm1 = lubridate::make_date({{ .col }}, month, 15),
                  phase = dplyr::case_when(
                      between(dm1, phases[[1, 1]], phases[[1, 2]]) ~ "Phase-1",
                      between(dm1, phases[[2, 1]], phases[[2, 2]]) ~ "Phase-2",
                      between(dm1, phases[[3, 1]], phases[[3, 2]]) ~ "Phase-3",
                      between(dm1, phases[[4, 1]], phases[[4, 2]]) ~ "Phase-4",
                      TRUE ~ NA_character_)) %>%
    dplyr::select(-dm1)
}

arealaps %>% add_phase(.col = year2) %>% dplyr::slice_head(n = 22)

Area_minmax <- arealaps %>% 
    add_phase(.col = year2) %>% 
    tidyr::pivot_longer(cols = -c(species, year2, phase)) %>%
    dplyr::group_by(species, name) %>%
    dplyr::reframe(ranges = paste(round(range(value, na.rm = TRUE), digits = 2),
                                  collapse = "–"),
                   SD = round(sd(value, na.rm = TRUE), digits = 3)) %>%
    dplyr::filter(name %in% c("Coexist", "A-posi", "S-posi"),
                  !stringr::str_detect(ranges, "Inf"))

AreaP_minmax <- 
    areaperc %>% 
    add_phase(.col = year2) %>% 
    tidyr::pivot_longer(cols = -c(species, year2, phase)) %>%
    dplyr::group_by(species, name) %>%
    dplyr::reframe(ranges = paste(round(range(value, na.rm = TRUE), digits = 1),
                                  collapse = "–"),
                   SD = round(sd(value, na.rm = TRUE), digits = 3)) %>%
    dplyr::filter(name %in% c("% Coexist", "% A-posi", "% S-posi"),
                  !stringr::str_detect(ranges, "Inf"))

## Egg abundance overlaps ======================================================
# NOTE: Category names are the same as Area, but values are not same between
# two species. Then can't use left_join().
egglaps <- 
    purrr::map_dfr(list(Anchovy = d2a, Sardine = d2s), .id = "species", 
                   .f = ~ dplyr::select(.x, -avecum) %>%
                       tidyr::pivot_wider(names_from = "asctg", values_from = "val")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total   = sum(c_across(-c(species:year2)), na.rm = TRUE),
                  Anchovy = sum(c_across(-c(species:year2, `S-posi`, Total)), na.rm = TRUE),
                  Sardine = sum(c_across(-c(species:year2, `A-posi`, Total, Anchovy)), na.rm = TRUE),
                  Coexist = sum(c_across(matches("-domi"))),
                  `A-dom` = sum(c_across(starts_with("A-domi"))),
                  `S-dom` = sum(c_across(starts_with("S-domi")))) %>%
    dplyr::ungroup() %>% as.data.frame()

eggperc <-
    egglaps %>%
    dplyr::mutate(across(c(`S-domi10`:`S-posi`, Anchovy:`S-dom`),
                         ~ .x * 100 / Total, .names = "% {.col}")) %>%
    dplyr::select(species:year2, starts_with("%")) %>% as.data.frame()

# ------------------------------------------------------------------------------
Egg_minmax <- egglaps %>% 
    add_phase(.col = year2) %>% 
    tidyr::pivot_longer(cols = -c(species, year2, phase)) %>%
    dplyr::group_by(species, name) %>%
    dplyr::reframe(ranges = paste(round(range(value, na.rm = TRUE), digits = 2),
                                  collapse = "–"),
                   SD = round(sd(value, na.rm = TRUE), digits = 3)) %>%
    dplyr::filter(name %in% c("Coexist", "A-posi", "S-posi"),
                  !stringr::str_detect(ranges, "Inf"))

EggP_minmax <- eggperc %>% 
    add_phase(.col = year2) %>% 
    tidyr::pivot_longer(cols = -c(species, year2, phase)) %>%
    dplyr::group_by(species, name) %>%
    dplyr::reframe(ranges = paste(round(range(value, na.rm = TRUE), digits = 1),
                                  collapse = "–"),
                   SD = round(sd(value, na.rm = TRUE), digits = 3)) %>%
    dplyr::filter(name %in% c("% Coexist", "% A-posi", "% S-posi"),
                  !stringr::str_detect(ranges, "Inf"))

foreach::foreach(i = list(arealaps, areaperc, egglaps, eggperc),
                 j = c("Area", "Area_%", "TEP", "TEP_%")) %do% {
    tmpd <- split(i, i$species)
    for(k in names(tmpd)) {
        sn <- paste0(unique(tmpd[[k]]$species), j)
        addWorksheet(wb, sheetName = sn)
        writeData(wb, sheet = sn, x = tmpd[[k]])
        addStyle(wb, sheet = sn, 
                 style = createStyle(numFmt = "#,#0.00"),
                 rows = seq_len(nrow(tmpd[[k]])) + 1L,
                 cols = 3:ncol(tmpd[[k]]), gridExpand = TRUE)
        addStyle(wb, sheet = sn, stack = TRUE,
                 style = createStyle(border = "bottom"),
                 rows = which(tmpd[[k]]$year2 == 2004) + 1L,
                 cols = seq_len(ncol(tmpd[[k]])), gridExpand = TRUE)
    }
}

tmpd <- tibble::lst(Area_minmax, AreaP_minmax, Egg_minmax, EggP_minmax) %>%
    dplyr::bind_rows(.id = "Data")
addWorksheet(wb, sheetName = "MinMax")
writeData(wb, sheet = "MinMax", x = tmpd) 
addStyle(wb, sheet = "MinMax", stack = TRUE,
         style = createStyle(border = "bottom"),
         rows = seq_len(nrow(tmpd))[seq_len(nrow(tmpd)) %% 4 == 1],
         cols = seq_len(ncol(tmpd)), gridExpand = TRUE)

saveWorkbook(wb, file = file.path(outd, "annual-overlaps.xlsx"), overwrite = TRUE)
