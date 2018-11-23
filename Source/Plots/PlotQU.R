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
        q, u, qmin, qmax, umin, umax, colGroup,
        isTex = FALSE, plotGrid = FALSE) {
    q <- enquo(q)
    u <- enquo(u)
    qmin <- enquo(qmin)
    qmax <- enquo(qmax)
    umin <- enquo(umin)
    umax <- enquo(umax)
    colGroup <- enquo(colGroup)
    q_end <- sym(paste0(quo_squash(q), "_end"))
    u_end <- sym(paste0(quo_squash(u), "_end"))

    if (quo_is_missing(qmin))
        qmin <- GetMinMaxNames(!!q)$min

    if (quo_is_missing(umin))
        umin <- GetMinMaxNames(!!u)$min

    if (quo_is_missing(qmax))
        qmax <- GetMinMaxNames(!!q)$max

    if (quo_is_missing(umax))
        umax <- GetMinMaxNames(!!u)$max

    subtr <- 0.03

    arrowData <- data %>%
        select(!!q, !!u, !!colGroup) %>%
        SplitByGroups(!!colGroup) %>%
        map(AsSegments, !!q, !!u) %>%
        bind_rows %>%
        mutate(!!quo_squash(colGroup) := as.factor(!!colGroup)) %>%
        mutate(
            Mdl = sqrt((!!q - !!q_end) ^ 2 + (!!u - !!u_end) ^ 2),
            Angle = atan2(!!u - !!u_end, !!q - !!q_end),
            Xc = 0.5 * (!!q + !!q_end),
            Yc = 0.5 * (!!u + !!u_end)) %>%
        mutate(Mdl = 0.5 * (Mdl - subtr)) %>%
        mutate(
            !!quo_name(q) := Xc + Mdl * cos(Angle),
            !!quo_name(q_end) := Xc - Mdl * cos(Angle),
            !!quo_name(u) := Yc + Mdl * sin(Angle),
            !!quo_name(u_end) := Yc - Mdl * sin(Angle))

    p <- ggplot(data, aes(
        x = !!q, y = !!u,
        xmin = !!qmin, xmax = !!qmax,
        ymin = !!umin, ymax = !!umax,
        col = !!colGroup,
        fill = !!colGroup,
        shape = !!colGroup))

    xrng <- GetRange(
        data,
        col = !!q, col_min = !!qmin, col_max = !!qmax) %>%
        Expand(factor = 0.06)

    yrng <- GetRange(
        data,
        col = !!u, col_min = !!umin, col_max = !!umax) %>%
        Expand(factor = 0.06)

    if (plotGrid) {
        p <- p +
            geom_segment(
                aes(x = x, xend = xend, y = y, yend = yend),
                tibble(
                    x = c(xrng[1], 0), xend = c(xrng[2], 0),
                    y = c(0, yrng[1]), yend = c(0, yrng[2])),
                inherit.aes = FALSE,
                alpha = 0.3, size = 1)
    }
    p <- p +
        geom_point(size = 4) +
        geom_errorbarh(size = 0.75, height = 0) +
        geom_errorbar(size = 0.75, width = 0) +
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
                col = !!colGroup,
                linetype = NULL),
            arrowData,
            inherit.aes = FALSE,
            size = 0.75,
            arrow = arrow(length = unit(10, "pt"))) +
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
    bndOrder <- Bands %>% pull(Band)
    dt <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder]
    field <- AverageFieldStars()[bndOrder]

    data <- dt %>%
        SubtractISM(field) %>%
        bind_rows %>%
        inner_join(select(Bands, Band, ID), by = "Band") %>%
        mutate(ID = as.factor(ID))


    plt <- data %>% PlotQU(Px, Py,
        colGroup = ID,
        plotGrid = TRUE,
        isTex = TRUE)

    drPath <- file.path("Output", "Plots")
    if (!dir.exists(drPath))
        dir.create(drPath, recursive = TRUE)
    fPath <- file.path(drPath, "QU_avg" %&%
    "_intr" %&%
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