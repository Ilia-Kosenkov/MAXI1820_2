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

PlotLC <- function(data, avg_data,
        x, y,
        ymin, ymax,
        group,
        xrng = NULL, yrng = NULL,
        xlab = quo_text(x),
        ylab = quo_text(y),
        isTex = FALSE) {

    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    group <- enquo(group)

    ynms <- GetMinMaxNames(!!y)

    if (quo_is_missing(ymin))
        ymin <- ynms$min

    if (quo_is_missing(ymax))
        ymax <- ynms$max

    if (!is.null(yrng)) {
        data %<>%
            Clamp(!!ymin, yrng) %>%
            Clamp(!!ymax, yrng)

        avg_data %<>%
            Clamp(!!ymin, yrng) %>%
            Clamp(!!ymax, yrng)
    }

    p <- ggplot(data,
        aes(x = !!x, y = !!y,
            ymin = !!ymin,
            ymax = !!ymax,
            col = !!group,
            fill = !!group,
            shape = !!group
            )) +
        scale_color_manual(
            limits = Style_Groups,
            values = Style_GroupColors,
            guide = FALSE) +
        scale_fill_manual(
            limits = Style_Groups,
            values = Style_GroupFills,
            guide = FALSE) +
        scale_shape_manual(
            limits = Style_Groups,
            values = Style_GroupShapes,
            guide = FALSE) +
        geom_point(size = Style_SymbolSizeSmall) +
        geom_errorbar(size = Style_ErrorBarSize,
            width = 0) +
        geom_point(data = avg_data %>%
                ModifyLevels(Group, function(g) as.numeric(g) + 4),
            size = Style_SymbolSize) +
        geom_errorbar(data = avg_data %>%
                ModifyLevels(Group, function(g) as.numeric(g) + 4),
            width = 0,
            size = Style_ErrorBarSizeLarge) +
        DefaultTheme(textSz = Style_TickFontSz, titleSz = Style_LabelFontSz) +
        coord_cartesian(xlim = xrng, ylim = yrng,
                        expand = FALSE)

    if (is_null(xrng))
        xrng <- GGPlotGetRange(p)$x
    if (is_null(yrng))
        yrng <- GGPlotGetRange(p)$y


    p %<>%
        LinearScaleTicks(xrng, gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            yrng,
            side = "y", n = 4,
            brTrans = function(x)
                LabelsRep(x, isTex = isTex, sameDigitCount = TRUE),
            gp = gpar(fontsize = Style_TickFontSz))

    p + ylab(ylab) + xlab(xlab)
}

#if (get0("ShouldRun", ifnotfound = FALSE)) {
if (FALSE) {
    grps <- c(0, 2, 3)
    bndOrder <- Bands %>% pull(Band)
    data <- ReadAllAvgData()[bndOrder] %>%
        map(filter, Group %in% grps)

    data_avg <- ReadAllAvgData(
        pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder] %>%
        map(filter, Group %in% grps)

    types <- c("P", "A")
    typeLabs <- c("$P_{..3}$ (\\%)", "$\\theta_{..3}$ (deg) ")

    xlim <- data %>%
        map(pull, MJD) %>%
        range %>%
        Expand(factor = 0.06)

    ylim <- types %>%
        map(GetMinMaxNames) %>%
        map(~map(data, select, !!.x[[1]], !!.x[[2]])) %>%
        map(unlist) %>%
        map(quantile, c(0.001, 0.991))

    grobs <-
        future_pmap(
            list(types, ylim, typeLabs),
            function(col, ylim, lab) {
                pmap(list(data, data_avg, names(data)),
                    ~PlotLC(.x, .y,
                        MJD, !!sym(col),
                        xrng = xlim, yrng = ylim,
                        group = Group,
                        ylab = glue(lab),
                        isTex = TRUE)) %>%
                GGPlotPanelLabs(
                    labels = letters[seq_len(length(data))],
                    hjust = 3, vjust = 2,
                    gp = gpar(fontsize = Style_LabelFontSz))
            })

    pth <- file.path("Output", "Plots")
    if (!dir.exists(pth))
        dir.create(pth, recursive = TRUE)

    future_map2(grobs, types,
        function(g, tp) {
            lPth <- file.path(pth, glue("light_curve_{tp}.tex"))
            tikz(lPth,
                width = Style_WidthStdInch,
                height = Style_HeightStdInch,
                standAlone = TRUE)
            tryCatch({
                g %>%
                    GGPlot2GrobEx %>%
                    GrobsArrange(
                        ncol = 1,
                        labsMar = Style_LabsMarStd,
                        axisMar = Style_AxisMarStd,
                        vGap = Style_VGapStd) %>%
                     GrobPlot
            }, finally = dev.off())

            Tex2Pdf(lPth)
        })

}