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
    xlab = quo_text(enquo(q)),
    ylab = quo_text(enquo(u)),
    xrng = NULL,
    yrng = NULL) {

    q <- enquo(q)
    u <- enquo(u)
    qmin <- enquo(qmin)
    qmax <- enquo(qmax)
    umin <- enquo(umin)
    umax <- enquo(umax)
    colGroup <- enquo(colGroup)
    shapeGroup <- enquo(shapeGroup)
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
            inherit.aes = FALSE)


    p <- p +
        geom_point(size = Style_SymbolSize) +
        geom_errorbar(size = Style_ErrorBarSize, width = 0) +
        geom_errorbarh(size = Style_ErrorBarSize, height = 0)

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
        ylab(ylab)
    p

}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    bndOrder <- Bands %>% pull(Band)
    data <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder] %>%
        map(inner_join, select(Bands, Band, ID), by = "Band") %>%
        map(mutate, ID = as.factor(ID))

    field <- field_stars[bndOrder] %>%
        map(filter, Star %in% c(2, 3, 6, 7, 9)) %>%
        map(mutate, Group = as.factor(-1)) %>%
        map(mutate, ID = as.factor(-1))

    map2(data, field, PlotQUPerBand) %>% print
}