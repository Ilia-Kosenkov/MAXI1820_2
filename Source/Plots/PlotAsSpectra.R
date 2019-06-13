# MIT License
#
# Copyright(c) 2018 Ilia Kosenkov[ilia.kosenkov.at.gm@gmail.com]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files(the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

PlotAsSpectra <- function(data,
                          x, y, ymin, ymax,
                          group = Group,
                          xlab = quo_text(enquo(x)),
                          ylab = quo_text(enquo(y))) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    group <- enquo(group)

    data <- data %>% arrange(Group)
    ynms <- GetMinMaxNames(!!y)

    if (quo_is_missing(ymin))
        ymin <- ynms$min
    if (quo_is_missing(ymax))
        ymax <- ynms$max

    p <- ggplot(
            data,
            aes(x = !!x, y = !!y,
                ymin = !!ymin, ymax = !!ymax,
                group = !!group,
                shape = !!group,
                col = !!group,
                fill = !!group,
                alpha = !!group,
                linetype = !!group)) +
        DefaultTheme(textSz = Style_TickFontSz, titleSz = Style_LabelFontSz) +
        scale_color_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Colors,
            guide = FALSE) +
        scale_fill_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Colors,
            guide = FALSE) +
        scale_shape_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Shapes,
            guide = FALSE) +
        scale_alpha_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Alphas,
            guide = FALSE) +
        scale_linetype_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_LineTypes2,
            guide = FALSE)


    p <- p +
        geom_point(size = Style_SymbolSize) +
        geom_line(size = Style_LineSize) +
        geom_errorbar(size = Style_LineSize, width = 0)

    xrng <- data %>% pull(!!x) %>%
        range %>% Expand(factor = 0.12, direction = c(1, 1.75))

    yrng <- GetRange(data, col_min = !!ymin, col_max = !!ymax) %>%
        Expand(factor = 0.06, direction = c(2, 2))


    bands <- data %>% pull(Band) %>% unique

    data2 <- data %>%
        filter(Band == last(bands)) %>%
        select(!!x, !!y) %>%
        mutate(Id = row_number())

    print(data2)
    print(diff(range(xrng)))
    wl <- bands %>%
        map_dbl(~data %>% filter(Band == .x) %>% slice(1) %>% pull(WL))

    p <- p +
        coord_cartesian(
            xlim = xrng, ylim = yrng, expand = FALSE, clip = "off") +
        xlab(xlab) +
        ylab(ylab) +
        GGCustomTextAnnotation(bands, wl, - Inf, vjust = -0.3, hjust = 0.6,
            gp = gpar(fontsize = Style_LabelFontSz)) +
        geom_text(aes(x = !!x, y = !!y, label = Id),
                  data2,
                  inherit.aes = FALSE,
                  position = position_nudge(x = 0.05 * diff(range(xrng))),
                  size = 7)


    p %>%
        LinearScaleTicks(
            rng = xrng, side = "x",
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            rng = yrng, side = "y",
            gp = gpar(fontsize = Style_TickFontSz))



}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    bndOrder <- Bands %>% pull(Band)
    avgData <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)") %>%
        bind_rows %>%
        inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
        mutate(Group = as_numeric(Group) + 1L)

    stars <- c(2, 3, 6, 7, 9)
    fData <- field_stars %>%
        map2(names(field_stars), ~ mutate(.x, Band = .y)) %>%
        map(filter, Star %in% stars) %>%
        bind_rows %>%
        inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
        mutate(Group = -Star) %>%
        mutate(Star = as.factor(Star))


    avgFData <- AverageFieldStars() %>%
        map2(names(.), ~ mutate(.x, Band = .y)) %>%
        bind_rows %>%
        inner_join(select(Bands, Band, ID, WL, Freq), by = "Band") %>%
        mutate(Group = 0L)

    #data <- bind_rows(avgData, fData, avgFData) %>%
        #mutate(Group = as.factor(Group))

    data2 <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder]
    field <- AverageFieldStars()[bndOrder]

    data <- data2 %>% SubtractISM(field) %>%
        bind_rows %>%
        inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
        mutate(Group =
            forcats::fct_relabel(Group, ~as.character(as.numeric(.) + 1L)))

    #stop()
    cols <- c("P", "A")
    labs <- c("$P_{BVR}$ (\\%)", "$\\theta_{BVR}$ (deg)")

    drPath <- fs::path("Output", "Plots") %T>% (fs::dir_create)


    future_pmap(list(cols, labs, seq_along(cols)),
        function(x, y, i) {

            fPath <- file.path(drPath, glue("spectra_subtr_{x}.tex"))

            tikz(fPath,
                width = Style_WidthWideInch,
                height = Style_HeightStdInch,
                standAlone = TRUE)

            tryCatch({
                PlotAsSpectra(data,
                        WL, !!sym(x),
                        group = Group,
                        xlab = "$\\lambda$ (\\AA)",
                        ylab = y) %>%
                    GGPlotPanelLabs(
                        letters[i], hjust = 5, vjust = 2,
                        gp = gpar(fontsize = Style_LabelFontSz)) %>%
                    GGPlot2GrobEx %>%
                    GrobMarginSet(
                        labsMar = Style_LabsMarStd,
                        axisMar = Style_AxisMarStd) %>%
                        GrobPlot
                }, finally = dev.off())

            Tex2Pdf(fPath, verbose = TRUE)
        }, .progress = TRUE)
}