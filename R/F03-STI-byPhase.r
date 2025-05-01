
getwd()
options(max.print = 500, width = 80)

# install.packages("needs")
needs::needs(tidyverse, RColorBrewer, gginnards, RcppRoll, gghighlight,
             openxlsx)

outd <- "path/to/your/output/folder"

# read data --------------------------------------------------------------------
source("Func_read_data.R")

ed <- load_eldata(species = c("Sardine", "Anchovy"), gridsize = 5, 
                  add_environment_conditions = TRUE, long_format = FALSE)

## Rename long names to shorter ones
ed <- ed %>%
    dplyr::rename(Step = Sardine_Egg,
                  Atep = Anchovy_Egg) %>%
    tidyr::replace_na(replace = list(Step = 0, Atep = 0))

phases <- tibble::tribble(
    ~ start  , ~ end    ,
    19780101 , 19881231 ,
    19890101 , 19981231 ,
    19990101 , 20101231 ,
    20110101 , 20210101 ,
) %>% dplyr::mutate_all(.funs = lubridate::ymd)

################################################################################ 
## temporary helper ------------------------------------------------------------
conv2long <- function(df) {
    df %>%
    tidyr::pivot_longer(cols = c(ends_with("tep"))) %>%
    dplyr::mutate(species = stringr::str_extract(name, "^.+(?=tep)"),
                  dattype = dplyr::if_else(!is.na(species),
                                           stringr::str_remove(name, species),
                                           name),
                  species = dplyr::if_else(species == "A", "Anchovy",
                                           "Sardine"),
                  year2 = dplyr::if_else(species == "Sardine" & month %in% 10:12,
                                         year + 1L, year)) %>%
    dplyr::filter(!is.na(species),
                  # NOTE: For sardine, use the same years as Fig. 1
                  !(species == "Sardine" & year2 %in% c(1978, 2021))) %>%
    dplyr::select(-name, year2, year, Nrec, everything())
}

add_phase_sst <- function(df, .col) {
    df %>%
    dplyr::mutate(dm1 = lubridate::make_date({{ .col }}, month, 15),
                  phase = dplyr::case_when(
                      between(dm1, phases[[1, 1]], phases[[1, 2]]) ~ "Phase-1",
                      between(dm1, phases[[2, 1]], phases[[2, 2]]) ~ "Phase-2",
                      between(dm1, phases[[3, 1]], phases[[3, 2]]) ~ "Phase-3",
                      between(dm1, phases[[4, 1]], phases[[4, 2]]) ~ "Phase-4",
                      TRUE ~ NA_character_),
                  sst = floor((SSTmean + tol) / isst0) * isst0) %>%
    dplyr::select(-dm1)
}
################################################################################

# parameters for data filtering -------------------------------------------------
tol <- 5E-16 # NOTE: Hack for floor()

isst0 <- 0.1 # bin width of SST
nlow0 <- 20  # lower limit of the number of observations
nterm <- 11

year0 <- 1978:2020
sects <- as.character(c(1:11))
#  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11"
# ------------------------------------------------------------------------------

################################################################################
## Figs. 3a-3e: Spawning temperature index #####################################
################################################################################
# (1) The entire period  =======================================================
# NOTE: Relative frequencies by species 

## Positive data (Sample size on egg collected)
d0 <- 
    ed %>% conv2long() %>%
    dplyr::select(-year) %>%
    dplyr::filter(year2 %in% !!year0) %>%
    tidyr::replace_na(replace = list(value = 0)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::mutate(sst = floor((SSTmean + tol) / isst0) * isst0) %>%
    dplyr::group_by(species, sst) %>%
    dplyr::summarise(count = sum(Nrec, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(rFreq = count / sum(count, na.rm = TRUE),
                  count = NULL) %>%
    dplyr::ungroup() %>%
    tidyr::complete(species, sst = seq(0.0, 35.0, by = 0.1), fill = list(rFreq = 0))

## Effort data (Sample size at each sst intervals)
d0a <- 
    ed %>% conv2long() %>% 
    dplyr::select(-year) %>%
    dplyr::filter(year2 %in% !!year0, !is.na(SSTmean)) %>% 
    add_phase_sst(.col = year2) %>%
    dplyr::mutate(sst = floor((SSTmean + tol) / isst0) * isst0) %>%
    dplyr::group_by(species, sst) %>%
    dplyr::summarise(count = sum(Nrec, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(rFreq2 = count / sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(species, sst = seq(0.0, 35.0, by = 0.1),
                    fill = list(rFreq2 = 0, count = 0))

## Check sample size (= effort) whether lower than the limit or not.
ggplot(d0a) + geom_col(aes(x = sst, y = count), width = isst0, linewidth = 0) +
    gghighlight(count < nlow0) + coord_cartesian(ylim = c(0, 2 * nlow0)) +
    facet_wrap(vars(species))

cutlow <- filter(d0a, count >= nlow0) %>% 
    group_by(species) %>% slice_head(n = 1) %>% rename(lowsst = sst)
cutlow
cuthig <- filter(d0a, count >= nlow0) %>% 
    group_by(species) %>% slice_tail(n = 1) %>% rename(higsst = sst)
cuthig

d0a_rev <- left_join(d0a, cutlow[, 1:2]) %>% 
    left_join(cuthig[, 1:2]) %>%
    filter(sst >= lowsst, sst <= higsst) # same as between(sst, lowsst, higsst, incbound = TRUE)

nrow(d0a_rev)
nrow(d0a) - nrow(d0a_rev)
nrow(d0)

## rolling (= running) means of the (%positive / %effort)
d0f <- 
    dplyr::inner_join(d0, d0a_rev[, 1:4], by = join_by(species, sst)) %>%
    dplyr::mutate(STI = rFreq / rFreq2,
                  STI = dplyr::if_else(is.nan(STI), NA_real_, STI),
                  nv = count * STI) %>%
    dplyr::arrange(species, sst) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(snv = RcppRoll::roll_sum(nv, n = nterm, fill = NA, na.rm = TRUE),
                  scount = RcppRoll::roll_sum(count, n = nterm, fill = NA, na.rm = TRUE),
                  # NOTE: weighted maen by count
                  roll_STI = snv / scount) %>%
    dplyr::select(-c(nv, snv, scount)) %>%
    dplyr::filter(!is.na(roll_STI), !is.nan(roll_STI))

range(d0f$roll_STI, na.rm = TRUE)

dplyr::slice_head(d0f, n = 1)

## colors for plot
fillcolors <- RColorBrewer::brewer.pal(8, "RdBu")

g0 <- ggplot(data = d0f) +
    geom_point(aes(x = sst, y = roll_STI, color = species), shape = 1,
               stroke = 0.3, size = 1.5, position = "identity") +
    geom_hline(data = data.frame(y = c(0.75, 1), lty = c("31", "1")), 
               aes(yintercept = y, linetype = lty), linewidth = 0.3) +
    scale_color_manual(values = c(fillcolors[c(1, 8)]),
                       breaks = c("Anchovy", "Sardine"),
                       labels = c("Anchovy", "Sardine"),
                       name = NULL) +
    scale_x_continuous(breaks = seq(0, 40, by = 5),
                       minor_breaks = seq(0, 40, by = 1),
                       limits = c(0, 35),
                       expand = expansion(add = c(0, 0))) +
    scale_y_continuous(breaks = seq(0, 4, by = 0.5),
                       limits = scales::breaks_extended(n = 2, only.loose = TRUE),
                       expand = expansion(add = c(0, 0))) +
    guides(color = guide_legend(override.aes = list(size = 2.0, stroke = 0.5)),
           linetype = "none",
           x = guide_axis(minor.ticks = TRUE),
           y = guide_axis(minor.ticks = TRUE)) +
    theme_classic(base_family = "Arial", base_line_size = 0.3, base_size = 12) +
    theme(axis.text = element_text(color = "black"),
          axis.title = element_text(size = 12), 
          legend.background = element_rect(color = "black", linewidth = 0.20),
          legend.justification = c(0, 1),
          legend.key.size = unit(12, "pt"),
          legend.margin = margin(2, 4, 2, 4),
          legend.position = "inside",
          legend.position.inside = c(0.01, 0.99),
          legend.spacing.y = unit(0, "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(color = NA)) +
    labs(x = "SST (°C)", 
         y = paste0("Spawning temperature index\n(", nterm, "-term running mean)"))

# (2) by Phase  ================================================================
# ------------------------------------------------------------------------------
year1 <- 1978:2020
nlow1 <- 10
# ------------------------------------------------------------------------------

range(ed$SSTmean, na.rm = TRUE)
floor(c(35.1, 35.3, 35.5, 35.8) / (15/60)) * (15/60)

## Positive data (Sample size on egg collected)
d1 <-
    ed %>% conv2long() %>%
    dplyr::filter(year2 %in% !!year1) %>%
    tidyr::replace_na(replace = list(value = 0)) %>%
    dplyr::filter(value > 0) %>%
    add_phase_sst(.col = year2) %>%
    dplyr::select(phase, species, sst, Nrec) %>%
    dplyr::group_by(phase, species, sst) %>%
    dplyr::summarise(count = sum(Nrec, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(rFreq = count / sum(count, na.rm = TRUE),
                  count = NULL) %>%
    dplyr::ungroup() %>%
    tidyr::complete(phase, species,
                    sst = seq(0.0, 35.0, by = 0.1),
                    fill = list(rFreq = 0))

## Effort data (Sample size at each sst intervals)
d1a <- 
    ed %>% conv2long() %>%
    dplyr::filter(year2 %in% !!year1, !is.na(SSTmean)) %>%
    add_phase_sst(.col = year2) %>%
    dplyr::select(phase, species, sst, Nrec) %>%
    dplyr::group_by(phase, species, sst) %>%
    dplyr::summarise(count = sum(Nrec, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(rFreq2 = count / sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(phase, species,
                    sst = seq(0.0, 35.0, by = 0.1),
                    fill = list(rFreq2 = 0, count = 0)) %>%
    dplyr::arrange(species, phase, sst)
     
## Check sample size (= effort) whether lower than the limit or not.
ggplot(d1a) + geom_col(aes(x = sst, y = count), width = isst0, linewidth = 0) +
    gghighlight(count < !!nlow1) + coord_cartesian(ylim = c(0, 2 * nlow1)) +
    facet_grid(cols = vars(species), rows = vars(phase))

cutlow1 <- filter(d1a, count >= nlow1) %>% 
    group_by(species, phase) %>% 
    slice_head(n = 1) %>% 
    rename(lowsst = sst)
cutlow1

cuthig1 <- filter(d1a, count >= nlow1) %>% 
    group_by(species, phase) %>% 
    slice_tail(n = 1) %>% 
    rename(higsst = sst)
cuthig1

d1a_rev <- left_join(d1a, cutlow1[, 1:3]) %>% 
    left_join(cuthig1[, 1:3]) %>%
    dplyr::filter(sst >= lowsst, sst <= higsst)

nrow(d1a) - nrow(d1a_rev)

## rolling (= running) means of the (%positive / %effort)
d1f <- 
    dplyr::inner_join(d1, d1a_rev[, 1:5], by = join_by(phase, species, sst)) %>%
    dplyr::mutate(STI = rFreq / rFreq2,
                  STI = dplyr::if_else(is.nan(STI), NA_real_, STI),
                  nv = count * STI) %>%
    dplyr::arrange(phase, species, sst) %>%
    dplyr::group_by(species, phase) %>%
    dplyr::mutate(snv = RcppRoll::roll_sum(nv, n = nterm, fill = NA, na.rm = TRUE),
                  scount = RcppRoll::roll_sum(count, n = nterm, fill = NA, na.rm = TRUE),
                  roll_STI = snv / scount) %>%
    dplyr::select(-c(nv, snv, scount)) %>%
    dplyr::filter(!is.na(roll_STI), !is.nan(roll_STI))

dplyr::slice_head(d1f, n = 1)
dplyr::slice_tail(d1f, n = 1)

unique(d1f$species)
range(d1f$roll_STI)
hist(d1f$roll_STI)

as.data.frame(dplyr::slice_max(ungroup(d1f), roll_STI, by = c("phase")))
slice_max(d1f, roll_STI) %>% select(!starts_with("rFreq")) %>% arrange(species) %>% as.data.frame()

# (3) Bind the entire and phases ###############################################
purrr::map(tibble::lst(d0f, d1f), head)

d10f <- 
    dplyr::bind_rows(d1f, dplyr::mutate(d0f, phase = "TheEntire")) %>%
    dplyr::mutate(phase = forcats::fct_inorder(phase))

lab_ph10 <- paste0("Phase-", 1:4, ": ", 
                   phases %>% 
                       dplyr::mutate(per = paste(year(start),
                                                 year(end - days(1)),
                                                 sep = "–")) %>%
                       dplyr::pull(per)) %>% c(., "The entire period: 1978–2020")
names(lab_ph10) <- c(paste0("Phase-", 1:4), "TheEntire")
lab_ph10 <- as_labeller(lab_ph10)

g0$layers
g10 <-
    gginnards::delete_layers(g0, idx = 1) %+% d10f +
    geom_point(aes(x = sst, y = roll_STI, color = species), shape = 1,
               stroke = 0.3, size = 1.5, position = "identity") +
    facet_wrap(vars(phase), axes = "all", scales = "free_y",
                labeller = labeller(phase = lab_ph10), ncol = 2, dir = "v") +
    g0$theme +
    theme(aspect.ratio = 1/2,
          legend.background = element_rect(fill = "#FFFFFF", color = "black", linewidth = 0.1),
          legend.justification = c(0, 1),
          legend.key.height = unit(5, "pt"),
          legend.key.width = unit(3, "pt"),
          legend.margin = margin(2, 4, 2, 4),
          legend.position = "inside",
          legend.position.inside = c(0.01/2, 0.99),
          panel.grid.major.x = element_line(linewidth = 0.2, color = "gray90"),
          panel.spacing.y = unit(6, "pt"),
          plot.margin = margin(r = 5),
          strip.background = element_rect(fill = NA, color = NA, linewidth = 0),
          strip.text.x = element_text(face = "bold", margin = margin(1, 1, 1, 1), 
                                      size = 9, hjust = 0.03)) +
    labs(y = paste0("Spawning temperature index (", nterm, "-term running mean)"),
         x = "Sea surface temperature (°C)") +
    coord_cartesian(xlim = c(4, 31), ylim = c(0, 3.5))

################################################################################
## Fig. 3f: Horizontal bars ####################################################
################################################################################
lower_rSTI <- 0.75

# (1) by Phase  ================================================================
## Maximun STI
maxSTI <- 
    d1f %>%
    dplyr::group_by(phase, species) %>%
    dplyr::slice_max(order_by = roll_STI) %>%
    dplyr::select(phase, species, maxsst = sst) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species, phase)
maxSTI

## Minimum STI
minSTI <- 
    d1f %>%
    dplyr::group_by(phase, species) %>%
    dplyr::filter(roll_STI > !!lower_rSTI) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::select(phase, species, minsst = sst) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species, phase)
minSTI

## Upper limit of SST over lower_rSTI
d1f %>%
    dplyr::group_by(phase, species) %>%
    dplyr::filter(roll_STI > !!lower_rSTI) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::select(phase, species, maxsst = sst) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species, phase)

d2 <- d1f %>%
    dplyr::left_join(., minSTI) %>%
    dplyr::filter(sst >= minsst) %>%
    dplyr::mutate(width = dplyr::if_else(roll_STI < lower_rSTI, 0, isst0),
                  sst1 = sst,
                  sst2 = sst1 + width,
                  ymin = dplyr::if_else(species == "Sardine",
                                        as.integer(stringr::str_extract(phase, "\\d+")) + 0.1,
                                        as.integer(stringr::str_extract(phase, "\\d+")) - 0.1),
                  grad = dplyr::if_else(roll_STI > 1, 1.5, lower_rSTI / 2)) %>%
    dplyr::filter(sst1 != sst2)

g2 <-
    ggplot(d2) +
    geom_segment(aes(x = sst1, xend = sst2, y = ymin, yend = ymin,
                     color = species, alpha = grad), linewidth = 1.8) +
    geom_point(data = maxSTI %>% dplyr::mutate(
                   y = dplyr::if_else(species == "Sardine",
                           as.integer(stringr::str_extract(phase, "\\d+")) + 0.29,
                           as.integer(stringr::str_extract(phase, "\\d+")) - 0.29)
                   ), aes(x = maxsst, y = y, shape = species, fill = species),
               stroke = 0.3, color = "black") +
    scale_alpha_continuous(range = c(0.35, 1), guide = "none") +
    scale_color_manual(values = c(fillcolors[c(1, 8)]),
                       breaks = c("Anchovy", "Sardine"),
                       labels = c("Anchovy", "Sardine"),
                       name = NULL) +
    scale_shape_manual(values = c(25, 24),
                       breaks = c("Anchovy", "Sardine"),
                       labels = c("Anchovy", "Sardine"),
                       guide = "none") +
    scale_fill_manual(values = c(fillcolors[c(1, 8)]),
                      breaks = c("Anchovy", "Sardine"),
                      labels = c("Anchovy", "Sardine"),
                      guide = "none") +
    scale_x_continuous(breaks = seq(0, 40, by = 5),
                       minor_breaks = seq(0, 40, by = 1),
                       limits = c(0, 35),
                       expand = expansion(mult = c(0, 0.001)),
                       name = "Sea surface temperature (°C)") +
    guides(x = guide_axis(minor.ticks = TRUE)) +
    scale_y_reverse(breaks = 1:4,
                    labels = paste0("Phase-", 1:4),
                    expand = expansion(add = c(0.2, 0.2)),
                    name = NULL) +
    g0$theme +
    theme(aspect.ratio = 1/2,
          axis.text = element_text(color = "black"),
          plot.margin = margin(r = 5),
          panel.border = element_rect(linewidth = 0.2, color = "black", fill = NA),
          panel.grid.major.x = element_line(linewidth = 0.3, color = "gray90"),
          legend.text = element_text(margin = margin(l = 2)),
          legend.position = "inside",
          legend.position.inside = c(0.01, 0.98),
          legend.justification = c(0, 1),
          legend.key.size = unit(8, "pt"),
          legend.background = element_rect(color = "black", linewidth = 0.2),
          legend.margin = margin(2, 4, 2, 4))

# (2) The entire period ========================================================
minSTI0 <-
    d0f %>%
    dplyr::group_by(species) %>%
    dplyr::filter(roll_STI > !!lower_rSTI) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::select(species, minsst = sst) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species)

maxSTI0 <- 
    d0f %>%
    dplyr::group_by(species) %>%
    dplyr::slice_max(order_by = roll_STI) %>%
    dplyr::select(species, maxsst = sst) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species)

d0_2 <- d0f %>%
    dplyr::left_join(., minSTI0) %>%
    dplyr::filter(sst >= minsst) %>%
    dplyr::mutate(width = dplyr::if_else(roll_STI < lower_rSTI, 0, isst0),
                  sst1 = sst,
                  sst2 = sst1 + width,
                  ymin = dplyr::if_else(species == "Sardine",
                                        5 + 0.1, 5 - 0.1),
                  grad = dplyr::if_else(roll_STI > 1, 1.5, lower_rSTI / 2)) %>%
    dplyr::filter(sst1 != sst2)

# (3) Bind the entire and phases ===============================================
d20 <- 
    dplyr::bind_rows(d2, dplyr::mutate(d0_2, phase = "TheEntire")) %>%
    dplyr::mutate(phase = forcats::fct_inorder(phase))

maxSTI20 <- 
    dplyr::bind_rows(maxSTI, dplyr::mutate(maxSTI0, phase = "TheEntire")) %>%
    dplyr::mutate(phase = forcats::fct_inorder(phase))

# plot ------
g20 <- gginnards::delete_layers(g2, idx = 2) %+% d20 +
    geom_point(data = maxSTI20 %>% dplyr::mutate(
                   y = dplyr::if_else(species == "Sardine",
                           as.integer(phase) + 0.29, as.integer(phase) - 0.29)
                   ), aes(x = maxsst, y = y, shape = species, fill = species),
               stroke = 0.3, size = 1.5, color = "black") +
    geom_hline(yintercept = 4.5, linewidth = 0.3) + 
    scale_y_reverse(breaks = 1:5,
                    labels = c(paste0("Phase-", 1:4), "The entire\nperiod"),
                    expand = expansion(add = c(0.2, 0.2)),
                    name = NULL) +
    theme(aspect.ratio = 7/10) +
    coord_cartesian(xlim = c(4, 31))

# SAVE figure objects ##########################################################
saveRDS(tibble::lst(STI = g10, STIbar = g20), 
        file = file.path(outd, "itmfile", "STIs.rds"))

## SAVE as Excel ###############################################################
levels(d20$phase)
# [1] "Phase-1"      "Phase-2"      "Phase-3"      "Phase-4"      "TheEntire"

d10f %>% filter(phase == "TheEntire", roll_STI > 0) %>% group_by(species) %>% slice_head()

alowlim <- ed %>% filter(Atep > 0) %>% slice_min(SSTmean) %>% as.data.frame()
alowlim$SSTmean

slowlim <- ed %>% filter(Step > 0) %>% slice_min(SSTmean) %>% as.data.frame()
slowlim$SSTmean

tmpd <- d20 %>%
    dplyr::filter(roll_STI >= 1) %>%
    dplyr::select(phase:sst, roll_STI) %>% #, minsst) %>%
    dplyr::arrange(phase, species) %>%
    tidyr::pivot_wider(names_from = c(phase, species), values_from = roll_STI) %>%
    dplyr::arrange(sst) %>%
    dplyr::relocate(sst, ends_with("Anchovy"))

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = "10.5", fontName = "Arial")

addWorksheet(wb, sheetName = "majorSTI")
writeData(wb, sheet = "majorSTI", x = tmpd)
openxlsx::addStyle(wb, sheet = "majorSTI", 
                   style = createStyle(numFmt = "#,#0.00"),
                   rows = seq_len(nrow(tmpd)) + 1L,
                   cols = 3:ncol(tmpd), gridExpand = TRUE)
openxlsx::addStyle(wb, sheet = "majorSTI", stack = TRUE,
                   style = createStyle(border = "bottom"),
                   rows = which(tmpd$sst == 20) + 1L,
                   cols = seq_len(ncol(tmpd)), gridExpand = TRUE)

addWorksheet(wb, sheetName = "maxSTI")
writeData(wb, sheet = "maxSTI", x = maxSTI20)

saveWorkbook(wb, file = file.path(outd, "STIbars_byPhase+ALL.xlsx"), overwrite = TRUE)
