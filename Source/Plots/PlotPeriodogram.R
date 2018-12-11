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

PlotPeriodogram <- function(.data,
    levels = c(0.5, 0.75, 0.99, 0.999), n = NA) {

    fCol <- sym("F")
    pCol <- sym("PSDN")
    aCol <- sym("Amplitude")

    p <- ggplot(
            mutate(.data, Group = factor("0")),
            aes(x = !!fCol, y = !!pCol, col = Group)) +
        DefaultTheme(
            textSz = Style_TickFontSz,
            titleSz = Style_LabelFontSz) +
        scale_color_manual(
            limits = Style_GroupsBasic,
            values = Style_GroupColorsBasic,
            guide = FALSE) +
        scale_linetype_manual(
            limits = Style_GroupsBasic,
            values = Style_GroupLinesBasic,
            guide = FALSE)



    p <- p +
        geom_line(size = Style_LineSize)


    if (!all(is.na(levels))) {
        assert_that(
            all(levels >= 0.0 & levels <= 1.0),
            msg = "All `levels` should be in the [0, 1] range.")
        assert_that(is.count(n))

        lvlDat <- levels %>% sort %>%
            map(~list(p = .x, lvl = -log(1 - (.x) ^ (1 / n)))) %>%
            map(~append(.x,
                list(x = -Inf, xend = +Inf, y = .x$lvl, yend = .x$lvl))) %>%
            bind_rows %>%
            mutate(Group = as.factor(row_number()))

        p <- p +
            geom_segment(
                aes(x = x, y = y, xend = xend, yend = yend,
                    col = Group, linetype = Group),
                lvlDat,
            size = Style_LineSize,
            inherit.aes = FALSE)
    }


    xlim <- .data %>% pull(!!fCol) %>% range %>% Expand(factor = 0.06)
    xStep <- FancyStep(xlim, modifier = c(1, 5))
    xBreaks <- GenerateBreaks(xlim, xStep, 0.1 * xStep)

    ylim <- .data %>% pull(!!pCol) %>% range
    ylim2 <- .data %>% pull(!!aCol) %>% range
    a <- diff(range(ylim2)) / diff(range(ylim))
    y2Trnsf <- function(x) a * x
    y2Reverse <- function(x) x / a

    if (are_equal(diff(sign(xlim)), 2)) {
        xlim_1 <- Expand(c(xlim[1], 0), factor = -0.1, direction = c(0, 1))
        xlim_2 <- Expand(c(0, xlim[2]), factor = -0.1, direction = c(1, 0))
        x2lim_1 <- rev(1 / xlim_1)
        x2lim_2 <- rev(1 / xlim_2)

        x2Breaks_1 <- GenerateInverseBreaks(-rev(x2lim_1))
        x2Breaks_2 <- GenerateInverseBreaks(x2lim_2)

        x2Breaks <- list(
            Large = (c(-x2Breaks_1$Large, Inf, x2Breaks_2$Large)),
        Small = (c(-x2Breaks_1$Small, x2Breaks_2$Small)))

    }

    if (exists("lvlDat")) {
        ylim <- range(c(ylim, range(lvlDat$y))) %>% Expand(factor = 0.06)
        ylim2 <- y2Trnsf(ylim)
    }

    yStep <- FancyStep(ylim, modifier = c(1, 5))
    yBreaks <- GenerateBreaks(ylim, yStep, 0.1 * yStep)
    y2Step <- FancyStep(ylim2, modifier = c(1, 5))
    y2Breaks <- GenerateBreaks(ylim2, y2Step, 0.1 * y2Step)

    p <- p +
        coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
        scale_y_continuous(
            name = "PSD",
            breaks = yBreaks$Small,
            labels = rep("", length(yBreaks$Small)),
            limits = ylim,
            sec.axis = sec_axis(~.,
                name = "Amplitude",
                breaks = y2Breaks$Small %>% y2Reverse,
                labels = rep("", length(y2Breaks$Small)))) +
        scale_x_continuous(
            name = "$\\nu$,~d$^{-1}$",
            breaks = xBreaks$Small,
            labels = rep("", length(xBreaks$Small)),
            limits = xlim,
            sec.axis = sec_axis(~.,
                name = "$P$,~d",
                breaks = 1 / x2Breaks$Small,
                labels = rep("", length(x2Breaks$Small))))

    p %<>%
        GGPlotCustomTicksEx(
            side = "left",
            breaks = yBreaks$Large,
            labels = yBreaks$Large,
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        GGPlotCustomTicksEx(
            side = "right",
            breaks = y2Breaks$Large,
            labels = y2Breaks$Large,
            trnsf = y2Reverse,
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        GGPlotCustomTicksEx(
            side = "bot",
            breaks = xBreaks$Large,
            labels = xBreaks$Large,
            gp = gpar(fontsize = Style_TickFontSz)) %>%
    GGPlotCustomTicksEx(
            side = "top",
            breaks = 1 / x2Breaks$Large,
            labels = map_chr(x2Breaks$Large,
                function(x)
                    if(is.infinite(x)) "$\\infty$" else as.character(x)),
            gp = gpar(fontsize = Style_TickFontSz))

}

if (get0("ShouldRun", ifnotfound = FALSE)) {

    data <- tibble(t = seq(0, 100, by = 0.1)) %>%
        mutate(
            t = t + 0.08 * runif(n()),
            x = 0.7 * cos(2 * pi * 0.5 * t + 0.6) +
                0 * rnorm(n())) %>%
        mutate(x = x - mean(x))

    w <- seq(-2, 2, length.out = 601) * 2 * pi

    psd <- LSAPeriodogramEx(data, t, x, w) %>%
        EstimateDifference(data, t, x, w) %>%
        select(F, PSDN, Amplitude, FAP, Contrib, Tau,
            contains("phase"), everything()) %>%
        print(n = min(30, nrow(.)))

    PlotPeriodogram(psd, n = nrow(data)) %>%
        GGPlot2GrobEx() %>%
        GrobMarginSet(axisMar = margin(1, 1, 1, 1, "cm"),
            labsMar = margin(1, 1, 1, 1, "cm")) %>%
        GrobPlot

}