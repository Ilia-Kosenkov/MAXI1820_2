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

PlotQUPerBand <- function(data, field,
    q = Px, u = Py, qmin, qmax, umin, umax,
    colGroup = ID,
    shapeGroup = Group,
    xlab = quo_text(ensym(q)),
    ylab = quo_text(ensym(u)),
    xrng = NULL,
    yrng = NULL) {

    q <- ensym(q)
    u <- ensym(u)
    qmin <- enquo(qmin)
    qmax <- enquo(qmax)
    umin <- enquo(umin)
    umax <- enquo(umax)
    colGroup <- ensym(colGroup)
    shapeGroup <- ensym(shapeGroup)
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

    fieldAvg <-
        AverageFieldStars(list(field), which = pull(field, Star))[[1]] %>%
        mutate(Group = as.factor(-1)) %>%
        mutate(ID = as.factor(-1))

    arrData <- data %>%
        select(!!q, !!u, !!colGroup, !!shapeGroup) %>%
        ModifySegemnts(!!q, !!u, shift = -0.03)

    p <- ggplot(
            data,
            aes(
                x = !!q, y = !!u,
                xmin = !!qmin, xmax = !!qmax,
                ymin = !!umin, ymax = !!umax,
                col = !!colGroup,
                fill = !!colGroup,
                shape = !!shapeGroup))

    p <- p +
        geom_point(
            data = field,
            size = Style_SymbolSizeSmall,
            alpha = Style_AlphaBackground) +
        geom_errorbar(
            data = field,
            size = Style_ErrorBarSize,
            width = 0,
            alpha = Style_AlphaBackground) +
        geom_errorbarh(
            data = field,
            size = Style_ErrorBarSize,
            height = 0,
            alpha = Style_AlphaBackground)

    p <- p +
        geom_point(
            data = fieldAvg,
            size = Style_SymbolSize) +
        geom_errorbar(
            data = fieldAvg,
            size = Style_ErrorBarSize,
            width = 0) +
        geom_errorbarh(
            data = fieldAvg,
            size = Style_ErrorBarSize,
            height = 0)

    p <- p +
        geom_segment(
            aes(
                x = !!q, y = !!u,
                xend = !!q_end, yend = !!u_end,
                col = !!colGroup),
            data = arrData,
            size = Style_LineSize,
            arrow = arrow(length = Style_ArrowLength),
            inherit.aes = FALSE)


    p <- p +
        geom_point(size = Style_SymbolSize) +
        geom_errorbar(size = Style_ErrorBarSize, width = 0) +
        geom_errorbarh(size = Style_ErrorBarSize, height = 0)

    if (is.null(xrng))
        xrng <- list(data, field) %>%
            map(select, !!qmin, !!qmax) %>%
            bind_rows %>%
            GetRange(col_min = !!qmin, col_max = !!qmax) %>%
            Expand(factor = 0.06)

    if (is.null(yrng))
        yrng <- list(data, field) %>%
            map(select, !!umin, !!umax) %>%
            bind_rows %>%
            GetRange(col_min = !!umin, col_max = !!umax) %>%
            Expand(factor = 0.06)

    width <- max(diff(xrng), diff(yrng))

    xrng %<>% Expand(factor = width / diff(xrng) - 1)
    yrng %<>% Expand(factor = width / diff(yrng) - 1)


    p <- p +
        DefaultTheme(textSz = Style_TickFontSz, titleSz = Style_LabelFontSz) +
        scale_color_manual(
            limits = Style_GroupsBands,
            values = Style_GroupColorsBands, guide = FALSE) +
        scale_fill_manual(
            limits = Style_GroupsBands,
            values = Style_GroupColorsBands, guide = FALSE) +
        scale_shape_manual(
            limits = Style_GroupsAvgOnly,
            values = Style_GroupShapes, guide = FALSE) +
        xlab(xlab) +
        ylab(ylab) +
        coord_cartesian(xlim = xrng, ylim = yrng, expand = FALSE, clip = "off")


    p %>%
        LinearScaleTicks(
            rng = xrng, side = "x",
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            rng = yrng, side = "y",
            gp = gpar(fontsize = Style_TickFontSz))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    tic("Total")
    tic("Plots prepared")

    grps <- c(0, 1, 2, 3)
    bndOrder <- Bands %>% pull(Band)
    data <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder] %>%
        map(inner_join, select(Bands, Band, ID), by = "Band") %>%
        map(mutate, ID = as.factor(ID)) %>%
        map(filter, Group %in% grps)

    field <- field_stars[bndOrder] %>%
        map(filter, Star %in% c(2, 3, 6, 7, 9)) %>%
        map(mutate, Group = as.factor(-1)) %>%
        map(mutate, ID = as.factor(-1))

    rngs <- list(data, field) %>%
        flatten %>%
        map(select, Px_min, Px_max, Py_min, Py_max) %>%
        bind_rows %>% {
            list(
                xrng = GetRange(., col_min = Px_min, col_max = Px_max),
                yrng = GetRange(., col_min = Py_min, col_max = Py_max))
        } %>%
        map(Expand, factor = 0.06)

    width <- rngs %>% map_dbl(diff) %>% max
    rngs %<>% map(~Expand(.x, factor = width / diff(.x) - 1))

    plts <- future_pmap(
            list(data, field, seq_along(data)),
            ~ PlotQUPerBand(
                ..1, ..2,
                xrng = rngs$xrng, yrng = rngs$yrng,
                xlab = glue("$q_{{{.x$Band[1]}}}$ (\\%)"),
                ylab = glue("$u_{{{.x$Band[1]}}}$ (\\%)"),
                ) %>%
                GGPlotPanelLabs(
                    labels = letters[..3],
                    hjust = 3, vjust = 2,
                    gp = gpar(fontsize = Style_LabelFontSz)),
            .progress = TRUE)

    toc()
    tic("PDFs saved")

    drPath <- fs::path("Output", "Plots") %T>% (fs::dir_create)

    future_walk2(plts, bndOrder,
        function(p, bnd) {
            fPath <- file.path(drPath, glue("QU_indv_{bnd}.tex"))
            tikz(fPath,
                width = Style_WidthStdInch, height = Style_HeightStdInch,
                standAlone = TRUE)
            tryCatch({
                p %>%
                    GGPlot2GrobEx %>%
                    GrobMarginSet(
                        labsMar = Style_LabsMarStd,
                        axisMar = Style_AxisMarStd) %>%
                    GrobPlot
            }, finally = dev.off())

            Tex2Pdf(fPath, verbose = TRUE)
        }, .progress = TRUE)

    toc()
    toc()
    tic.clear()
    tic.clearlog()
}