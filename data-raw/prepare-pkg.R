remove.packages("freggla")

devtools::load_all(here::here(), export_all = FALSE)
# library(freggla) # Cannot run only after devtools::load_all().
# devtools::reload() # Reload.

## FUNDAMENTAL
usethis::create_package(here::here())  # create minimal (R/ DESCRIPTION NAMESPACES) pkg
usethis::use_git(here::here()) # initialise git
usethis::use_data_raw()        # create data-raw/, add data-raw/ to .Rbuildignore, etc..
usethis::use_readme_md()       # make README.md file
usethis::use_news_md()         # make NEWS.md file
usethis::use_data()            # create data/ and export data (.rda)
usethis::use_testthat()        # create tests/ etc..
usethis::use_gpl3_license()    # Add GPL3 Licence to DESCRIPTION
usethis::use_pipe()            # add pipe %>%
usethis::use_mit_license()     # Add MIT Licence to DESCRIPTION

## BUILD
devtools::document() # Create man/ etc.. (inherit from roxygen2::roxygenize)
devtools::check()    # Build bandle package and scan general problems
devtools::install(here::here(), dependencies = c("Depends"),
                  build_vignette = FALSE)  # Wrapper function of `R CMD INSTALL`

# devtools::build(here::here()) # Create bandle or binary pkg from source by `R CMD BUILD`
# devtools::build(here::here(), binary = TRUE) # Create bandle & binary pkg from source pkg `R CMD BUILD`
# devtools::check_built(here::here(), cran = FALSE) # scan general problems
# install.packages(pkgs = "../freggla_0.1.0.9001.tar.gz",
#                  dependencies = TRUE,
#                  INSTALL_opts = c("--build_vignettes"))

## DESCRIPTION
purrr::map(.x = c("rlang", "dplyr", "ggplot2", "magrittr"),
           .f = usethis::use_package, "Imports")
    
purrr::map(.x = c("knitr", "RColorBrewer", "rmarkdown", "tidyr", "stringr",
                  "foreach", "forcats", "purrr", "scales", "rnaturalearth", 
                  "data.table", "tibble", "assertthat"),
           .f = usethis::use_package, "Suggests")

## TEST
usethis::use_test("load-data")                    # add test code
testthat::test_dir(here::here("tests/testthat/")) # Run all tests (devtools::test wraps this.)
testthat::test_file(here::here("tests/testthat/test-load-data.R")) # Run a specific test code.
usethis::use_coverage() # add code test coverage to README.md
covr::codecov()         # test coverage

## TYPO
lintr::lint_package()   # inspect and suggest linter errors.

## VIGNETTE
# usethis::use_vignette("Introduction", title = "fregglaの紹介") # create vignette
# usethis::use_vignette("Map", title = "基本地図の作成") # create vignette

## GITHUB PAGES
usethis::use_pkgdown()                # use pkgdown webpage
pkgdown::build_site(here::here())     # build pkgdown webpage
usethis::use_github_action("pkgdown") # use CI

## See also
httr::BROWSE("https://usethis.r-lib.org")

