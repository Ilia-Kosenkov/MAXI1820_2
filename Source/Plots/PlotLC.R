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

PlotLC <- function(data, x, y, ymin, ymax, group) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    group <- enquo(group)

    ymin <-
        if (quo_is_missing(ymin)) sym(as.character(quo_squash(y)) %++% "_min")
        else ymin

    ymax <-
        if (quo_is_missing(ymax)) sym(as.character(quo_squash(y)) %++% "_max")
        else ymax

    ggplot(data,
        aes(x = !!x, y = !!y,
            ymin = !!ymin,
            ymax = !!ymax,
            col = !!group)) +
        geom_smooth(
            mapping = aes(x = !!x, y = !!y),
            formula = y ~ x,
            method = "lm",
            level = 0.68,
            color = "#000000") +
        geom_pointrange() +
        DefaultTheme()
}

if (get0("ShouldRun", ifnotfound = FALSE)) {

    data <- ReadPolLCData()

    grobs <- c("P", "Px", "Py", "A") %>%
        future_map(function(col) {
            data %>%
                map(PlotLC, MJD, !!sym(col)) %>%
            GGPlotPanelLabs(labels = names(data), hjust = 3, vjust = 2)
        })

    pth <- file.path("Output", "Plots")
    if (!dir.exists(pth))
        dir.create(pth, recursive = TRUE)

    tikz(file.path(pth, "light_curves.tex"),
        width = 6, height = 7, standAlone = TRUE)

    tryCatch({
            grobs %>%
                map(GGPlot2GrobEx) %>%
                map(~GrobsArrange(
                        .x, ncol = 1,
                        labsMar = margin(1, 1, 1, 1, "cm"),
                        axisMar = margin(1, 1, 1, 1, "cm"),
                        vGap = unit(0.1, "cm"))) %>%
                walk(GrobPlot)
    }, finally = dev.off())

    Tex2Pdf(file.path(pth, "light_curves.tex"), verbose = TRUE)
}