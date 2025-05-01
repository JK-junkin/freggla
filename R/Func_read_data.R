load_eldata <- function(species = "Anchovy", gridsize = 15, dattype = "Egg",
                        add_environment_conditions = FALSE, long_format = TRUE) {

    # Main processing
    out <- 
        extract_from_p1(species = species, gridsize = gridsize, dattype = dattype) %>%
        dplyr::relocate(year, month, Lati, Long) %>%
        dplyr::mutate_all(.funs = as.double)

    if (long_format) {
        out <- out %>%
            # NOTE: bottleneck
            conv2longfmt(add_env_conds = add_environment_conditions) %>%
            dplyr::mutate_at(.vars = dplyr::vars(-c("species", "dattype")),
                             .funs = as.double)
    }

    out
}

extract_from_p1 <- function(species = "Anchovy", gridsize = 15, dattype = "Egg") {
    d <- specs <- ctgs <- NULL

    assertthat::assert_that(
        any(as.character(gridsize) %in% c("5", "15", "30")),
        msg = paste("`gridsize` should be selected from 5, 15, or 30.",
                    "See help(load_eldata).")
    )

    dataname <- paste0("Egg", stringr::str_pad(gridsize, 2, pad = "0"), "min")
    load(paste0("../data/", dataname, ".Rda"))

    d <- eval(parse(text = dataname))
    d %>%
        dplyr::select(year:SSTmean,
                      tidyselect::matches(charvec2regex(species))) %>%
        dplyr::select(year:SSTmean,
                      tidyselect::matches(charvec2regex(dattype)))
}

conv2longfmt <- function(daf, add_env_conds = FALSE) {
    out <- 
        daf %>%
        tidyr::pivot_longer(cols = -c(year:Nrec, SSTmean)) %>%
        dplyr::mutate(species = stringr::str_extract(name, "^.+(?<=[^[:alnum:]+])"),
                      dattype = dplyr::if_else(!is.na(species),
                                               stringr::str_remove(name, species),
                                               name),
                      species = stringr::str_remove(species, "[^[:alnum:]+]")) %>%
        dplyr::filter(!is.na(species)) %>%
        dplyr::select(-name)

    if (add_env_conds) {
        out
    } else {
        dplyr::select(out, -c(SSTmean))
    }
}

################################################################################
# Inner helpers
################################################################################
# #' @rdname load_eldata
# #' @param x a character vector
charvec2regex <- function(x) {
    paste0(".*", paste(x, collapse = ".*|.*"), ".*")
}
