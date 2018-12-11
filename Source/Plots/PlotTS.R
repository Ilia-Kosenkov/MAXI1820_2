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

PlotTS <- function(.data, t, x, group, xlab, ylab, p, geom = geom_point) {
    x <- quo_squash(enquo(x))
    t <- quo_squash(enquo(t))
    group <- enquo(group)
    x_min <- as.character(x) %&% "_min"
    x_max <- as.character(x) %&% "_max"
    hasPlot <- !missing(p)

    if (quo_is_missing(group))
        group <- NULL

    if (!exists(x_min, .data))
        x_min <- NULL
    else
        x_min <- sym(x_min)

    if (!exists(x_max, .data))
        x_max <- NULL
    else
        x_max <- sym(x_max)

    if (missing(p))
        p <- ggplot() +
            DefaultTheme(
                textSz = Style_TickFontSz,
                titleSz = Style_LabelFontSz) +
            scale_color_manual(
                breaks = Style_GroupsBasic,
                values = Style_GroupColorsBasic,
                guide = FALSE) +
            scale_fill_manual(
                breaks = Style_GroupsBasic,
                values = Style_GroupColorsBasic,
                guide = FALSE) +
            scale_shape_manual(
                breaks = Style_GroupsBasic,
                values = Style_GroupShapesBasic,
                guide = FALSE)

    p <- p +
        geom(
            aes(x = !!t, y = !!x, col = !!group,
                fill = !!group, shape = !!group),
            .data)

    if (!is_empty(x_min) && !is_empty(x_max))
        p <- p +
            geom_errorbar(
                aes(x = !!t,
                    ymin = !!x_min, ymax = !!x_max, col = !!group),
                .data, width = 0, size = Style_LineSize)


    if (!hasPlot && !missing(xlab))
        p <- p + xlab(xlab)
    if (!hasPlot && !missing(ylab))
        p <- p + ylab(ylab)

    p
}

#if (get0("ShouldRun", ifnotfound = FALSE)) {
if (FALSE) {
    data <- tibble(t = seq(0, 100, by = 0.1)) %>%
        mutate(
            t = t + 0.08 * runif(n()),
            x = 0.7 * cos(2 * pi * 0.5 * t + 0.6) +
                0 * rnorm(n())) %>%
        mutate(x = x - mean(x)) %>%
        mutate(t2 = (t / 2) %% 1) %>%
        mutate(t3 = (t / 4) %% 1) %>%
        mutate(t4 = (t / 6) %% 1) %>%
        mutate(g2 = as.factor(1L), g3 = as.factor(4L), g4 = as.factor(2L))

    p <- PlotTS(data, t2, x, g2) %>%
        { PlotTS(data, t3, x, g3, p = .) } %>%
        { PlotTS(data, t4, x, g4, p = .) } %>% print

}