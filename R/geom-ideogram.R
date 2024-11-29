#' ideogram with ggplot2
#'
#' @title Ideogram
#' @param mapping Set of aesthetic mappings created by ggplot2::aes() or ggplot2::aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param linejoin Line join style (round, mitre, bevel).
#' @param radius Radius of rounded corners.
#' @param chrom.col Color of the chromosome.
#' @param chrom.lwd Line width of the chromosome.
#' @param chrom.lty Line type of the chromosome.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @description draw ideogram geom
#' @importFrom ggplot2 layer Geom
#' @importFrom grid unit
#' @importFrom rlang list2
#' @examples
#' library(ggplot2)
#' library(ggideogram)
#'
#' region <- seq(0.0, 0.5, length.out = 48)
#' band_data <- data.frame(
#'   x = rep(1, 48),
#'   ymin = region,
#'   ymax = region + diff(region)[1],
#'   fill = c("cyan", "purple", "cyan", "purple", "cyan", "purple"),
#'   arm = rep(c("p", "q"), each=24)
#' )
#'
#' ggplot(band_data, aes(chrom = 1)) +
#'   geom_ideogram(aes(x = x, ymin = ymin, ymax = ymax, arm = arm, fill = fill),
#'                 radius = unit(20, 'pt')) +
#'   theme_void()
#' @export
#' @rdname geom_ideogram
geom_ideogram <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
                      radius = unit(.1, 'npc'),
                      chrom.col = NULL,
                      chrom.lwd = NULL,
                      chrom.lty = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomIdeogram,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            radius = radius,
            chrom.col = chrom.col,
            chrom.lwd = chrom.lwd,
            chrom.lty = chrom.lty,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @title GeomIdeogram
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom aes draw_key_polygon fill_alpha
#' @importFrom dplyr mutate
#' @importFrom grid unit
#' @export
GeomIdeogram <- ggproto("GeomIdeogram", Geom,
                    default_aes = aes(colour = NA, fill = NA, linewidth = 0.5, linetype = 1,
                                      alpha = NA, arm = NA, width = NULL, just = NULL),

                    required_aes = c("x", "ymin", "ymax", "chrom"),

                    setup_data = function(data, params) {
                        data$x <- as.integer(data$x)
                        data$width <- data$width %||%
                            params$width %||% (min(vapply(
                                split(data$x, data$PANEL, drop = TRUE),
                                resolution, numeric(1), zero = FALSE
                            )) * 0.9)
                        data$just <- params$just %||% 0.5
                        mutate(data,
                               xmin = x - width * just,
                               xmax = x + width * (1 - just))
                    },

                    draw_panel = function(self, data, panel_params, coord,
                                          radius = unit(.1, 'npc'),
                                          chrom.col = NULL, chrom.lwd = NULL, chrom.lty = NULL,
                                          lineend = "butt", linejoin = "mitre") {
                        data <- ggplot2:::check_linewidth(data, ggplot2:::snake_class(self))
                        if (!coord$is_linear()) {
                            cli::cli_warning("Ideogram geom expects linear x and y scales")
                        } else {
                            coords <- coord$transform(data, panel_params)
                            ggplot2:::ggname("geom_ideogram", ideogramGrob(coords, radius,
                                                                           chrom.col, chrom.lwd, chrom.lty,
                                                                           linejoin, lineend))
                        }
                    },

                    draw_key = draw_key_polygon,

                    rename_size = TRUE
)

roundrectGrobs <- function(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           width = unit(1, "npc"), height = unit(1, "npc"),
                           radius = unit(0.1, "npc"), color = 'black', linewidth = 1,
                           linetype = 1, just = "centre", default.units = "npc",
                           name = NULL, vp = NULL) {
    if (length(x) != length(y) || length(x) != length(width) || length(x) != length(height)){
        stop("length(y) must be the same as length(x)")
    }
    rrgList <- mapply(function(x, y, width, height) {
        roundrectGrob(
            x = x,
            y = y,
            width = width,
            height = height,
            default.units = default.units,
            r = radius,
            just = just,
            name = name,
            gp = gpar(
                col = color,
                fill = NA,
                lwd = linewidth,
                lty = linetype
            ),
            vp = vp
        )
        }, x, y, width, height, SIMPLIFY = FALSE)
    gTree(children = do.call(gList, rrgList))
}
#' @title ideogramGrob
#' @format NULL
#' @usage NULL
#' @importFrom grid roundrectGrob viewport rectGrob gTree gList as.path gpar
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 .pt fill_alpha
#' @importFrom magrittr "%>%"
#' @export
ideogramGrob <- function(coords, radius,
                         chrom.col = NULL, chrom.lwd = NULL, chrom.lty = NULL,
                         linejoin = "mitre", lineend = "butt") {
    if (any(is.na(coords$arm))) {
        chromSize <- group_by(coords, chrom) %>%
            summarise(ymin = min(ymin), ymax = max(ymax), xmin = min(xmin), xmax = max(xmax))
    } else {
        chromSize <- group_by(coords, chrom, arm) %>%
            summarise(ymin = min(ymin), ymax = max(ymax), xmin = min(xmin), xmax = max(xmax), .groups = "drop")
    }
    clip <- as.path(roundrectGrobs(
        x = chromSize$xmin,
        y = chromSize$ymax,
        width = chromSize$xmax - chromSize$xmin,
        height = chromSize$ymax - chromSize$ymin,
        just = c("left", "top"),
        radius = radius,
        color = "black",
        linewidth = 0.5,
        linetype = 1
    ))
    vp <- viewport(clip = clip)
    rrg <- roundrectGrobs(
        x = chromSize$xmin,
        y = chromSize$ymax,
        width = chromSize$xmax - chromSize$xmin,
        height = chromSize$ymax - chromSize$ymin,
        just = c("left", "top"),
        radius = radius,
        color = chrom.col %||% coords$colour[1],
        linewidth = (chrom.lwd %||% coords$linewidth[1]) * .pt,
        linetype = chrom.lty %||% coords$linetype[1]
    )
    rg <- rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "npc",
        just = c("left", "top"),
        gp = gpar(
            col = coords$colour,
            fill = fill_alpha(coords$fill, coords$alpha),
            lwd = coords$linewidth * .pt,
            lty = coords$linetype,
            linejoin = linejoin,
            lineend = lineend
        )
    )
    gTree(name = 'ideogram', children = gList(rg, rrg), vp = vp)
}
