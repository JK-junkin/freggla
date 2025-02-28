#' @title Load Egg and Larvae data as long format
#' @description `load_eldata()` is the main function to load and extract data
#' from ELDB.
#' @param species a character vector, Default: 'カタクチイワシ'. In order to
#' get the latest species name list, run `load_eldata(show_species = TRUE)`.
#' @param gridsize an integer or a character string of data resolution; select
#' from 5, 15 (default), 30.
#' @param dattype a character vector, Default: '産卵量'. In order to
#' get the latest dattype name list, run `load_eldata(show_dattype = TRUE)`.
#' @param year_used a vector of years such as 1978:2000 and c(1980, 2000) to
#' load. In default (NULL), all years will be load.
#' @param show_species logical. print species names without loading data,
#' Default: FALSE
#' @param show_dattype logical. print data types (categories) without loading
#' data, Default: FALSE
#' @param show_year_months logical. **not implemented (future work)** print
#' first and last year-months (data) available in the latest version package
#' without loading data, Default: FALSE
#' @param add_environment_conditions logical. FALSE (default) is never loading
#' environmental conditions; if TRUE, loading six environmental conditions:
#' `SST_mean`, `SST_var`, `SSS_mean`, `SSS_var`, `PL_vol_density_mean`, and
#' `PL_vol_density_var`.
#' @param add_environment_conditions = FALSE (default), it has 9 variables
#' (columns): `year`, `month`, `Long`, `Lati`, `area`, `section`, `value`,
#' `species`, `dattype`; If add_environment_conditions = TRUE, 6 environamental
#' variables (columns)- `SST_mean`, `SST_var`, `SSS_mean`, `SSS_var`,
#' `PL_vol_density_mean`, and `PL_ vol_density_var`- are also obtained.
#' @param long_format logical. TRUE (default) returns the data as long (tidy)
#' format; if FALSE, returns the data as wide format.
#' @param as_datatable logical. TRUE (default) returns the data as data.table
#' class; if FALSE, returns the data as tbl_df class.
#' @return a data.table (default) is returned, if none of the show_* arguments
#' are TRUE and as_datatable = FALSE.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' if(interactive()){
#'    # (Default) Extract Japanese anchovy's TEP with resolution of 15 arc-minutes
#'    load_eldata()
#'    load_eldata(long_format = FALSE)
#'
#'    # Check inner processing
#'    ddd <- extract_from_p1()
#'    conv2longfmt(ddd)
#'  }
#' }
#' @rdname load_eldata
#' @export

# stringi::stri_escape_unicode("カタクチイワシ")
# [1] "\\u30ab\\u30bf\\u30af\\u30c1\\u30a4\\u30ef\\u30b7"

load_eldata <- function(species = "カタクチイワシ", gridsize = 15,
                        dattype = "産卵量", year_used = NULL,
                        show_species = FALSE, show_dattype = FALSE,
                        show_year_months = FALSE, 
                        add_environment_conditions = FALSE,
                        long_format = TRUE, as_datatable = TRUE) {

    if (any(show_species, show_dattype, show_year_months)) {
        # NOTE: ここ, 順番大事
        return(shows()[c(show_species, show_dattype, show_year_months)])
    }

    # Make objects sent to a console
    tar_sp <- tar_ct <- tar_yr <- NULL
    tar_sp <- stringr::str_subset(shows()$species, charvec2regex(species))
    tar_ct <- stringr::str_subset(shows()$dattype, charvec2regex(dattype))
    if (is.null(year_used)) {
        tar_yr <- shows()$years
    } else {
        tar_yr <- year_used[year_used %in% shows()$years]
    }

    # Print on console
    cat(console_header(prefix = "  Species", vec = tar_sp, wid = 22),
            print_console(tar_sp, width = 35), "\n",
        console_header(prefix = " Grid size", vec = gridsize),
            gridsize, "arc-minutes\n",
        console_header(prefix = " Data categories", vec = tar_ct),
            print_console(tar_ct, width = 35), "\n",
        console_header(prefix = " Years", vec = tar_yr),
            print_console(tar_yr), "\n",
        "Now loading ...\n")

    # Main processing
    out <- 
        extract_from_p1(species = species, gridsize = gridsize,
                        dattype = dattype) %>%
        dplyr::filter(.data$year %in% !!tar_yr) %>%
        conv_latlon_decimal() %>% # NOTE: bottleneck
        dplyr::relocate(.data$year, .data$month, .data$lati_60, .data$Lati,
                        .data$long_60, .data$Long) %>%
        dplyr::mutate_at(.vars = dplyr::vars(-c("lati_60", "long_60")),
                         .funs = as.double)

    if (long_format) {
        out <- out %>%
            # NOTE: bottleneck
            conv2longfmt(add_env_conds = add_environment_conditions) %>%
            dplyr::mutate_at(.vars = dplyr::vars(-c("species", "dattype")),
                             .funs = as.double)
    }

    if (as_datatable) {
        data.table::as.data.table(out)
    } else {
        out
    }
}

#' @rdname load_eldata
#' @description `extract_from_p1()` is an inner function of `load_eldata()`
#' extracting data from `p1_<gridsize>minGrid.rda` by species as wide format.
#' @importFrom rlang .data
#' @export
extract_from_p1 <- function(species = "カタクチイワシ", gridsize = 15,
                            dattype = "産卵量",
                            show_species = FALSE, show_dattype = FALSE) {
    d <- specs <- ctgs <- NULL

    assertthat::assert_that(
        any(as.character(gridsize) %in% c("5", "15", "30")),
        msg = paste("`gridsize` should be selected from 5, 15, or 30.",
                    "See help(load_eldata).")
    )
    d <- eval(parse(text = paste0("p1_", gridsize, "minGrid")))

    specs <- stringr::str_extract(names(d), "^\\p{katakana}+\\p{Han}{1}") %>%
        stringr::str_remove("卵|産|子|平|仔") %>%
        purrr::discard(.p = is.na) %>% unique()
    rmstr <- stringr::str_extract(names(d), paste(specs, collapse = "|"))
    ctgs <- stringr::str_remove(names(d), rmstr) %>%
        purrr::discard(.p = is.na) %>% unique()

    if (show_species & show_dattype) {
        return(tibble::lst(species = specs, dattype = ctgs))
    }
    if (show_species) return(specs)
    if (show_dattype) return(ctgs)

    d %>%
        rename_ambinfo_to_eng() %>%
        dplyr::select(.data$year:.data$PL_vol_density_var,
                      tidyselect::matches(charvec2regex(species))) %>%
        dplyr::select(.data$year:.data$PL_vol_density_var,
                      tidyselect::matches(charvec2regex(dattype)))
}

#' @description `conv2longfmt()` is an inner function of `load_eldata()` using
#' with `extract_from_p1()` to convert wide format into long format.
#' @param daf a data.frame (wide format) loaded by `extract_from_p1()`.
#' @param add_env_conds logical. FALSE (default) is never loading environmental
#' conditions; if TRUE, loading six environmental conditions: SST_mean, SST_var,
#' SSS_mean, SSS_var, PL_vol_density_mean, and PL_vol_density_var.
#' @importFrom rlang .data
#' @rdname load_eldata
#' @export
conv2longfmt <- function(daf, add_env_conds = FALSE) {
    out <- 
        daf %>%
        tidyr::pivot_longer(cols = -c(.data$year:.data$PL_vol_density_var)) %>%
        dplyr::mutate(species =
                          stringr::str_extract(.data$name,
                                               "^\\p{katakana}+\\p{Han}{1}"),
                      species = stringr::str_remove(.data$species,
                                                    "卵|産|子|平|仔"),
                      dattype = dplyr::if_else(!is.na(.data$species),
                                               stringr::str_remove(.data$name,
                                                                   .data$species),
                                               .data$name)) %>%
        dplyr::filter(!is.na(.data$species)) %>%
        dplyr::select(-.data$name, -.data$dlevel)

    if (add_env_conds) {
        out
    } else {
        dplyr::select(out, -c(.data$SST_mean:.data$PL_vol_density_var))
    }
}

################################################################################
# Inner helpers
################################################################################

#' @param daf data.frame-like object
#' @param col_pattern PARAM_DESCRIPTION, Default: '_60'
#' @importFrom dplyr mutate_at vars rename_with
#' @importFrom tidyselect contains ends_with
#' @importFrom stringr str_extract str_to_title
#' @importFrom frabento conv_dm2dd
#' @rdname load_eldata
conv_latlon_decimal <- function(daf, col_pattern = "_60") {
    # NOTE: map & mutateで汎用性を持たせられるか?? 2023/06/09 12:27(金).
    # 区切り文字と列指定, 新しい列名の3つの引数を追加したい.
    daf %>%
        dplyr::mutate_at(.vars = dplyr::vars(tidyselect::contains(col_pattern)),
                         .funs = list(conv = ~ frabento::conv_dm2dd(.x))) %>%
        dplyr::rename_with(.fn = ~ stringr::str_extract(.x, "[^_]+") %>%
                               stringr::str_to_title(),
                           tidyselect::ends_with("_conv"))
}

#' @param year_used a vector of years such as 1978:2000 and c(1980, 2000) to
#' load. In default (NULL), all years are will load.
#' @importFrom assertthat assert_that
#' @rdname load_eldata
avail_year <- function(year_used = NULL) {
    assertthat::assert_that(
        class(year_used) %in% c("numeric", "integer", "double", "NULL"),
        msg = paste("`class(year_used)` should be numeric / integer / double.",
                    "See help(load_eldata).")
    )
    # NOTE: species_regexに合わせるべきかも
    unique(extract_from_p1()$year)
}

#' @param chr a vector of character
#' @param width Maximun width of character string. Default: 50
#' @param side Location which indicates content has been removed. Default: "center"
#' @importFrom stringr str_trunc
#' @rdname load_eldata
print_console <- function(chr, width = 50, side = "center") {
    paste0(chr, collapse = ", ") %>%
        stringr::str_trunc(width = width, side = side)
}

#' @param prefix a character atomic.
#' @param vec a vector (character or numeric) to be padded.
#' @param wid an integer. Width of padding, Default: 21
#' @return a character atomic.
#' @importFrom stringr str_pad
#' @rdname load_eldata
console_header <- function(prefix, vec, wid = 21) {
    paste0(prefix, " (", length(vec), ")") %>%
        stringr::str_pad(., width = wid, side = "right") %>% 
        paste0(., ":")
}

#' @rdname load_eldata
shows <- function() {
    c(extract_from_p1(show_species = TRUE, show_dattype = TRUE),
      list(years = avail_year(year_used = NULL)))
}

#' @param daf data.frame-like object 
#' @rdname load_eldata
#' @importFrom dplyr rename
rename_ambinfo_to_eng <- function(daf) {
    daf %>%
        dplyr::rename(year     = .data$年,
                      month    = .data$月,
                      lati_60  = .data$緯度,
                      long_60  = .data$経度,
                      gridsize = .data$メッシュサイズ,
                      dlevel   = .data$データ抽出レベル,
                      area     = .data$海域面積,
                      section  = .data$海区番号,
                      SST_mean = .data$平均水温,
                      SST_var  = .data$水温分散,
                      SSS_mean = .data$平均塩分,
                      SSS_var  = .data$塩分分散,
                      PL_vol_density_mean = .data$平均プランクトン沈殿量密度,
                      PL_vol_density_var = .data$平均プランクトン沈殿量密度分散)
}

#' @rdname load_eldata
#' @param x a character vector
charvec2regex <- function(x) {
    paste0(".*", paste(x, collapse = ".*|.*"), ".*")
}
