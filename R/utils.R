#' Add chromosome size to a data frame
#' @param data A data frame with columns Chrom, Start, End
#' @param chromSize A data frame with columns Chrom, Size
#' @param karyotype A character string specifying the karyotype to use
#' @return A data frame with an additional column size
#' @export
add_chromosome_size <- function(data, chromSize = NULL, karyotype = 'hg19') {
    if (is.null(chromSize)) {
        data(list = c(paste0('chromosome.size.', karyotype)), package = "ggideogram")
        chromSize <- get(paste0('chromosome.size.', karyotype))
    }
    other_cols <- setdiff(colnames(data), c("Chrom", "Start", "End"))
    group_by(data, Chrom) %>%
        summarise(left = min(Start), right = max(End), .groups = "drop") %>%
        inner_join(chromSize, by = "Chrom") %>%
        rowwise() %>%
        summarise(
            Chrom = Chrom,
            Start = list(c(Start, right)),
            End = list(c(left, End)),
            .groups = "drop"
        ) %>%
        unnest(c(Start, End)) %>%
        mutate(!!!setNames(rep(list(''), length(other_cols)), other_cols)) %>%
        rbind(data, .)
}

#' Put together ideogram data
#' @param data A data frame with columns Chrom, Start, End, ...
#' @param regions A data frame with columns Chrom, Size. If NULL, it will be set to data.
#'                If not NULL, it will be used to calculate the increment for each chromosome.
#'                The chromosome should be the same as in data and ordered in the same way.
#' @param width The width of the ideogram
#' @return A data frame with the ideogram data put together
#' @export
put_togather <- function(data, regions = NULL, width = 1e6) {
    if (is.null(regions)) {
        regions <- data
    }
    increment <- regions %>%
        group_by(Chrom) %>%
        summarise(
            interval = (cur_group_id() - 1) * width,
            max_value = max(End),
            .groups = "drop"
        ) %>%
        mutate(increment = replace_na(lag(cumsum(as.numeric(max_value)), n = 1), 0) + interval,) %>%
        dplyr::pull(increment)

    group_by(data, Chrom) %>%
        mutate(Start = Start + increment[cur_group_id()],
               End = End + increment[cur_group_id()]) %>%
        ungroup()
}

#' Get cytoband colors
#' @return A named vector with cytoband colors
#' @export
cytoband_colors <- c(
    gneg = "#FFFFFF",
    gvar = "#C0C0C0",
    gpos25 = "#D9D9D9",
    gpos33 = "#D9D9D9",
    gpos50 = "#A6A6A6",
    gpos66 = "#A6A6A6",
    gpos75 = "#737373",
    gpos100 = "#000000",
    acen = "#FF0000",
    stalk = "#C0C0C0"
)
