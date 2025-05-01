
getwd()
options(max.print = 200, width = 80)

# install.packages("needs")
needs::needs(tidyverse, foreach, psych, openxlsx)

outd <- "path/to/your/output/folder"

# read data --------------------------------------------------------------------
gs <- readRDS(file.path(outd, "itmfile/overlap_year_v0.rds"))
load(file = file.path(outd, "itmfile/correlations_whole.Rda"))
# cor_whole_ApSp, cor_whole_ApCo, cor_whole_SpCo

phases <- tibble::tribble(
    ~ start  , ~ end    ,
    19780101 , 19881231 ,
    19890101 , 19981231 ,
    19990101 , 20101231 ,
    20110101 , 20210101 ,
) %>% dplyr::mutate_all(.funs = lubridate::ymd)

phyr <- phases %>% dplyr::mutate_all(.funs = ~ lubridate::year(.x))

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

# Correlation analysis #########################################################
## (1) Area overlapped (scaled) ================================================
gs$A_anc$layers

unique(gs$A_anc$data$asctg)
# [1] S-domi10 S-domi2  S-domi   A-domi   A-domi2  A-domi10 A-posi  
# Levels: S-domi10 S-domi2 S-domi A-domi A-domi2 A-domi10 A-posi

anc <- gs$A_anc$data %>% dplyr::filter(asctg %in% "A-posi") %>% add_phase(year2)
sar <- gs$A_sar$data %>% add_phase(year2)

Area <- dplyr::bind_rows(anc, sar) %>%
    dplyr::select(asctg, year2, val, phase) %>%
    tidyr::pivot_wider(names_from = "asctg", values_from = "val") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`A>S` = sum(c_across(`A-domi10`:`A-domi`)),
                  `S>A` = sum(c_across(`S-domi`:`S-domi10`))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year2 >= 1979)

## by Phase 
res_3group <- foreach::foreach(i = 1:4, .combine = "c") %do% {
    phasename <- paste0("Phase-", i) 
    A <- dplyr::filter(Area, phase == phasename) %>%
        dplyr::select(-c(year2, phase, `A-domi10`:`S-domi10`)) %>% 
        dplyr::mutate(cooccur = `A>S` + `S>A`, .keep = "unused")
    p <- psych::corr.test(x = A,
                          method = "pearson", 
                          adjust = "bonferroni", # "holm", #
                          alpha = 0.05)
    o <- list(list(data = A, result = p))
    setNames(object = o, nm = stringr::str_remove(phasename, "[:punct:]+"))
}

res_3group[["Phase1"]]$data %>% nrow()
res_3group[["Phase1"]]$result$n       

res_3group[["Phase1"]]$result$p
res_3group[["Phase1"]]$result$ci
res_3group[["Phase1"]]$result$p.adj
res_3group[["Phase1"]]$result$ci2

res_3group[["Phase1"]]$result$ci[, c("r", "p")] %>% t()

cbind(res_3group[["Phase1"]]$result$n, t(res_3group[["Phase1"]]$result$ci[, c("r", "p")]))

od1 <- foreach::foreach(i = 1:4, .combine = "bind_rows",
                            .packages = loadedNamespaces()) %do% {
    phasename <- paste0("Phase", i) 
    cbind(phasename, 
          N = res_3group[[phasename]]$result$n,
          t(res_3group[[phasename]]$result$ci[, c("r", "p")])
    ) %>% as.data.frame() %>%
    dplyr::mutate(across(`A-pos-S-pos`:`S-pos-coccr`,
                         .fns = ~ signif(as.double(.x), digits = 4)),
                  N = as.integer(N)) %>%
    rownames_to_column(var = "item") %>%
    dplyr::relocate(phasename:N, item)
}

## The entire
od2 <- 
    purrr::map_dfr(tibble::lst(cor_whole_ApSp, cor_whole_ApCo, cor_whole_SpCo),
                   ~ signif(rbind(.x$estimate, .x$p.value), digits = 4)) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rlang::set_names(nm = c("A-pos-S-pos", "A-pos-coccr", "S-pos-coccr")) %>%
    dplyr::mutate(phasename = "All", N = length(unique(Area$year2)),
                  item = c("r", "p"))

outdata <- dplyr::bind_rows(od1, od2)

# SAVE as Excel ################################################################
s0 <- createStyle(halign = "right", valign = "center", numFmt = "#,#0.0000")
wb <- createWorkbook()
modifyBaseFont(wb = wb, fontName = "Arial")
addWorksheet(wb, sheetName = "Table 1")
addStyle(wb = wb, sheet = "Table 1", style = s0, gridExpand = TRUE, stack = TRUE,
         cols = seq_len(ncol(outdata) - 1L), rows = seq_len(nrow(outdata)) + 1L)
writeData(wb = wb, sheet = "Table 1", x = outdata)
saveWorkbook(wb = wb, file = file.path(outd, "table1.xlsx"), overwrite = TRUE)
