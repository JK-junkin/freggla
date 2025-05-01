#' @title create an sf worldmap layer or gg
#' @description This function is intended to be used in two ways: first, to
#' overlay a world map on an existing gg object, and second, to create a base
#' world map using ggplot2.
#' @param sfmap World map data in sf format, Default: NULL
#' @param sf_resol Resolution of sf world map data passed to 
#' `rnaturalearth::ne_countries(scale)`, Default: 50 (medium).
#' @param pacific_centered If TRUE (default), the world map is centred on the
#' Pacific Ocean; if FALSE, the world map is centred on the prime meridian (i.e.
#' the meridian on Greenwich).
#' @param as_gg Whether or not to make a gg object, Default: TRUE
#' @param lgl Longitude limits, Default: NULL
#' @param ltl Latitude limits, Default: NULL
#' @param lgb Longitude breaks, Default: NULL
#' @param ltb Latitude breaks, Default: NULL
#' @param ... Arguments passed to `geom_sf`
#' @details If sfmap is NULL, the default map projection system is a geographic
#' coordinate system based on the WGS84. It has not yet been checked whether
#' there are any problems with other coordinate reference systems.  See example
#' and vignette("wmap_sf").
#' @return gg or a list (LayerInstance)
#' @examples 
#' library(ggplot2)
#' library(frabento)
#' set.seed(180)
#'
#' df <- data.frame(Long = seq(140, 240, by = 5),
#'                  Lati = runif(n = 21, min = 40, max = 65),
#'                  group = c(rep(1:5, each = 4), 6))
#' 
#' # plain world map as pacific centered (default)
#' wmap_sf()
#' 
#' # plain world map as Greenwich centered
#' wmap_sf(pacific_centered = FALSE)
#' 
#' # Trim worldmap and overlay data points on worldmap
#' wmap_sf(lgl = c(135, 245), ltl = c(35, 70)) +
#'   geom_point(data = df, aes(x = Long, y = Lati, color = as.factor(group)))
#'
#' # Trim world map and overlay worldmap on data points
#' ggplot() +
#'   geom_point(data = df, aes(x = Long, y = Lati, color = as.factor(group))) +
#'   wmap_sf(as_gg = FALSE, lgl = c(135, 245), ltl = c(35, 70))
#' @seealso 
#'  \code{\link[rnaturalearth]{ne_countries}}
#'  \code{\link[sf]{st_break_antimeridian}}, \code{\link[sf]{st_shift_longitude}}
#' @import ggplot2
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_break_antimeridian st_shift_longitude
#' @rdname wmap_sf
#' @export 
wmap_sf <- function(sfmap = NULL, sf_resol = 50, pacific_centered = TRUE, 
                    as_gg = TRUE, lgl = NULL, ltl = NULL, lgb = NULL, ltb = NULL, ...) {

    if (is.null(sfmap)) {
        sfmap <-
            rnaturalearth::ne_countries(scale = sf_resol, returnclass = "sf")
        if (pacific_centered) {
            sfmap <- sfmap %>%
                sf::st_break_antimeridian(., lon_0 = 180) %>%
                sf::st_shift_longitude(.)
        }
    }

    lays <- 
        list(geom_sf(data = sfmap,
                     inherit.aes = FALSE, show.legend = FALSE, ...),
             scale_x_continuous(breaks = lgb),
             scale_y_continuous(breaks = ltb),
             coord_sf(xlim = lgl, ylim = ltl, expand = FALSE))

#     sf::sf_use_s2(FALSE) # XXXX:
    if (as_gg) { ggplot() + lays } else { lays }
}

utils::globalVariables(".")
