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

PlotQU <- function(data, q, u, qmin, qmax, umin, umax, group) {
    q <- enquo(q)
    u <- enquo(u)
    qmin <- enquo(qmin)
    qmax <- enquo(qmax)
    umin <- enquo(umin)
    umax <- enquo(umax)
    group <- enquo(group)

    qmin <-
        if (quo_is_missing(qmin)) sym(as.character(quo_squash(q)) %&% "_min")
        else qmin

    qmax <-
        if (quo_is_missing(qmax)) sym(as.character(quo_squash(q)) %&% "_max")
        else qmax

    umin <-
        if (quo_is_missing(umin)) sym(as.character(quo_squash(u)) %&% "_min")
        else umin

    umax <-
        if (quo_is_missing(umax)) sym(as.character(quo_squash(u)) %&% "_max")
        else umax

    ggplot(data, aes(
        x = !!q, y = !!u,
        xmin = !!qmin, xmax = !!qmax,
        ymin = !!umin, ymax = !!umax,
        col = !!group)) +
    geom_point(size = 1.5) +
    geom_errorbarh(size = 0.5) +
    geom_errorbar(size = 0.5)

}

#if (get0("ShouldRun", ifnotfound = FALSE)) {
if (FALSE) {
    data <- ReadPolLCData_2()
    data2 <- ReadPolLCData_2(
        pathTemp1 = file.path("Output", "Data", "pol_avg_all_"),
        pathTemp2 = file.path("Output", "Data", "pol_avg_all_2_")) %>%
        map(function(x) {
            levels(x$Group) <- as.numeric(levels(x$Group)) + 2
            x
        })
    plts <- data %>%
        map(PlotQU, Px, Py, group = Group) %>%
        map(~.x + scale_color_manual(
                guide = FALSE,
                limits = 1:4,
                values = brewer.pal(6, "Paired")[c(5, 1, 6, 2)])) %>%
        map2(data2,
                ~ .x +
                geom_point(data = .y, shape = 15, size = 3) +
                geom_errorbarh(data = .y, size = 2, height = 0) +
                geom_errorbar(data = .y, size = 2, width = 0)) %>%
        map(~.x + DefaultTheme()) %>%
        GGPlotPanelLabs(labels = names(data), hjust = 3, vjust = 2)

    fPath <- file.path("Output", "Plots", "qu.tex")

    tikz(fPath, width = 5, height = 5, standAlone = TRUE)
    tryCatch({
        plts %>% GGPlot2GrobEx() %>%
            GrobMarginSet(
                labsMar = margin(1, 1, 1, 1, "cm"),
                axisMar = margin(1, 1, 1, 1, "cm")) %>%
            walk(GrobPlot)
    }, finally = dev.off())

    Tex2Pdf(fPath, verbose = TRUE)
}