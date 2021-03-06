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
        range %>% Expand(factor = 0.06)

    yrng <- GetRange(data, col_min = !!ymin, col_max = !!ymax) %>%
        Expand(factor = 0.06)


    bands <- data %>% pull(Band) %>% unique

    wl <- bands %>%
        map_dbl(~data %>% filter(Band == .x) %>% slice(1) %>% pull(WL))

    p <- p +
        coord_cartesian(
            xlim = xrng, ylim = yrng, expand = FALSE, clip = "off") +
        xlab(xlab) +
        ylab(ylab) +
        GGCustomTextAnnotation(bands, wl, - Inf, vjust = -1,
            gp = gpar(fontsize = Style_LabelFontSz))

    p %>%
        LinearScaleTicks(
            rng = xrng, side = "x",
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            rng = yrng, side = "y",
            gp = gpar(fontsize = Style_TickFontSz))



}

if (get0("ShouldRun", ifnotfound = FALSE)) {
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

    data <- bind_rows(avgData, fData, avgFData) %>%
        mutate(Group = as.factor(Group))

    cols <- c("P", "A")
    labs <- c("$P_{BVR}$ (\\%)", "$\\theta_{BVR}$ (deg)")

    drPath <- file.path("Output", "Plots")
    if (!dir.exists(drPath))
        dir.create(drPath, recursive = TRUE)

    future_map2(cols, labs, function(x, y) {

        fPath <- file.path(drPath, glue("spectra_{x}.tex"))

        tikz(fPath,
            width = Style_WidthStdInch,
            height = Style_HeightWideInch,
            standAlone = TRUE)

        tryCatch({
            PlotAsSpectra(data,
                WL, !!sym(x),
                group = Group,
                xlab = "$\\lambda$ (\\AA)",
                ylab = y) %>%
            GGPlot2GrobEx %>%
            GrobMarginSet(
                labsMar = Style_LabsMarStd,
                axisMar = Style_AxisMarStd) %>%
                GrobPlot
            }, finally = dev.off())

        Tex2Pdf(fPath)
    })
}