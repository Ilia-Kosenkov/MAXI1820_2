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

PlotQU <- function(data,
        q, u, qmin, qmax, umin, umax, group,
        isTex = FALSE, plotGrid = FALSE) {
    q <- ensym(q)
    u <- ensym(u)
    qmin <- enquo(qmin)
    qmax <- enquo(qmax)
    umin <- enquo(umin)
    umax <- enquo(umax)
    group <- ensym(group)

    q_end <- sym(paste0(q, "_end"))
    u_end <- sym(paste0(u, "_end"))

    if (quo_is_missing(qmin))
        qmin <- GetMinMaxNames(!!q)$min

    if (quo_is_missing(umin))
        umin <- GetMinMaxNames(!!u)$min

    if (quo_is_missing(qmax))
        qmax <- GetMinMaxNames(!!q)$max

    if (quo_is_missing(umax))
        umax <- GetMinMaxNames(!!u)$max

    subtr <- -0.03

    arrowData <- data %>%
        select(!!q, !!u, !!group) %>%
        SplitByGroups(!!group) %>%
        map(ModifySegemnts, x = !!q, y = !!u, shift = subtr) %>%
        bind_rows %>%
        mutate(!!quo_squash(group) := as.factor(!!group))

    p <- ggplot(data, aes(
        x = !!q, y = !!u,
        xmin = !!qmin, xmax = !!qmax,
        ymin = !!umin, ymax = !!umax,
        col = !!group,
        fill = !!group,
        shape = !!group))

    xrng <- GetRange(
        data,
        col = !!q, col_min = !!qmin, col_max = !!qmax) %>%
        Expand(factor = 0.06)

    yrng <- GetRange(
        data,
        col = !!u, col_min = !!umin, col_max = !!umax) %>%
        Expand(factor = 0.06)

    width <- max(diff(yrng), diff(xrng))

    xrng %<>% Expand(factor = width / diff(xrng) - 1)
    yrng %<>% Expand(factor = width / diff(yrng) - 1)

    if (plotGrid) {
        p <- p +
            geom_segment(
                aes(x = x, xend = xend, y = y, yend = yend),
                tibble(
                    x = c(xrng[1], 0), xend = c(xrng[2], 0),
                    y = c(0, yrng[1]), yend = c(0, yrng[2])),
                inherit.aes = FALSE,
                alpha = Style_AlphaBackground, size = Style_LineSize)
    }
    p <- p +
        geom_point(size = Style_SymbolSize) +
        geom_errorbarh(size = Style_ErrorBarSize, height = 0) +
        geom_errorbar(size = Style_ErrorBarSize, width = 0) +
        scale_color_manual(
            limits = Style_GroupsBands,
            values = Style_GroupColorsBands,
            guide = FALSE) +
        scale_fill_manual(
            limits = Style_GroupsBands,
            values = Style_GroupColorsBands,
            guide = FALSE) +
        scale_shape_manual(
            limits = Style_GroupsBands,
            values = Style_GroupShapesBands,
            guide = FALSE) +
        scale_linetype_manual(
            limits = Style_GroupsBands,
            values = Style_GroupLinesBands,
            guide = FALSE) +
        geom_segment(
            aes(
                x = !!q, xend = !!q_end,
                y = !!u, yend = !!u_end,
                col = !!group,
                linetype = NULL),
            arrowData,
            inherit.aes = FALSE,
            size = Style_LineSize,
            arrow = arrow(length = Style_ArrowLength)) +
        DefaultTheme(
            textSz = Style_TickFontSz,
            titleSz = Style_LabelFontSz) +
        coord_cartesian(clip = "off",
            xlim = xrng, ylim = yrng, expand = FALSE) +
        xlab(
            if (isTex)
                "$q_{BVR}$ (\\%)"
            else
                expression(italic(q[BVR]) ~ "(%)")) +
        ylab(
            if (isTex)
                "$u_{BVR}$ (\\%)"
            else
                expression(italic(u[BVR]) ~ "(%)"))


    p %>%
        LinearScaleTicks(
            rng = xrng, side = "x",
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            rng = yrng, side = "y",
            gp = gpar(fontsize = Style_TickFontSz))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
#if (FALSE) {
    grps <- c(0, 1, 2, 3)
    bndOrder <- Bands %>% pull(Band)
    dt <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder]
    field <- AverageFieldStars()[bndOrder]

    data <- dt %>%
        SubtractISM(field, .propagate_errors = FALSE) %>%                        # Comment for normal plot
        bind_rows %>%
        inner_join(select(Bands, Band, ID), by = "Band") %>%
        mutate(ID = as.factor(ID)) %>%
        filter(Group %in% grps)


    plt <- data %>% PlotQU(Px, Py,
            group = ID,
            plotGrid = TRUE,                          # Comment for normal plot
            isTex = TRUE)# %>%
        #GGPlotPanelLabs("d", hjust = 3, vjust = 2,    # Comment for normal plot
            #gp = gpar(fontsize = Style_LabelFontSz))  # Comment for normal plot

    drPath <- fs::path("Output", "Plots") %T>% (fs::dir_create)

    fPath <- fs::path(drPath, "QU_avg" %&%
    "_intr" %&%                                       # Comment for normal plot
    ".tex")

    tikz(fPath, width = Style_WidthStdInch, height = Style_HeightStdInch,
        standAlone = TRUE)

    tryCatch({
             plt %>% GGPlot2GrobEx %>%
                GrobMarginSet(
                    labsMar = Style_LabsMarStd,
                    axisMar = Style_AxisMarStd) %>%
                GrobPlot
        }, finally = dev.off())

    Tex2Pdf(fPath, verbose = TRUE)
}