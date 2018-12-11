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

InvestigatePeriods <- function(.data, t, x, w) {
    t <- enquo(t)
    x <- enquo(x)

    tau <- LSAPhase(w, pull(.data, !!t))

    .data %<>% mutate(x__ = !!x - mean(!!x))

    psd <- LSAPeriodogramEx(.data, !!t, x__, w, tau) %>%
        EstimateDifference(.data, !!t, x__, w) %>%
        select(F, P, Contrib, PSDN, Amplitude, FAP, everything()) %>%
        arrange(desc(Contrib))
    bestP <- psd %>% pull(P) %>% head(1) %>% abs

    result <- list(
            psdPlot = PlotPeriodogram(psd, c(0.01, 0.1), n = nrow(.data)),
            lcPlot = PlotTS(
                mutate(
                    .data,
                    t__ = (!!t / bestP) %% 1,
                    g__ = as.factor(1L)),
                    t = t__, x = !!x, group = g__,
                    xlab = glue("{quo_text(t)} by {round(bestP, 2)}, d"),
                    ylab = glue("${quo_text(x)}$")),
            wndPlot = PlotTS(
                    psd,
                    F,
                    Wnd,
                    xlab = "$\\nu$,~d$^{-1}$",
                    ylab = "Window",
                    geom = geom_line),
            psd = psd)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    tic()

    data <- ReadFoldedData(path("Output", "Data", "Folded")) %>% flatten
    w <- seq(-2, 2, length.out = 601) * 2 * pi


    ff <- function(inp, col, err, path, path_tbl) {
        col <- enquo(col)
        err <- enquo(err)

        fs::dir_create(path)
        fs::dir_create(path_tbl)

        band <- inp %>% pull(Band) %>% head(1)
        by <- inp %>% pull(By) %>% head(1)

        input <- inp %>%
            FilterRange(!!err, quantile(pull(., !!err), c(0, 0.95))) %>%
            FilterRange(!!col, quantile(pull(., !!col), c(0.025, 0.975)))


        result <- InvestigatePeriods(input, MJD, !!col, w)


        path1 <- fs::path(path,
            glue("{quo_text(col)}_in_{band}_by_{by}_psd.tex"))
        tikz(path1, width = Style_WidthStdInch, height = Style_HeightStdInch,
            standAlone = TRUE)

        tryCatch({
        result$psdPlot %>% GGPlot2GrobEx %>%
                GrobMarginSet(
                    axisMar = Style_AxisMarEq, labsMar = Style_LabsMarEq) %>%
                GrobPlot
        result$wndPlot %>% GGPlot2GrobEx %>%
                GrobMarginSet(
                    axisMar = Style_AxisMarEq, labsMar = Style_LabsMarEq) %>%
                GrobPlot
            }, finally = dev.off())

        path2 <- fs::path(path,
            glue("{quo_text(col)}_in_{band}_by_{by}_lc.tex"))
        tikz(path2, width = Style_WidthStdInch, height = Style_HeightStdInch,
            standAlone = TRUE)
        tryCatch({
            result$lcPlot %>% GGPlot2GrobEx %>%
                GrobMarginSet(
                    axisMar = Style_AxisMarEq, labsMar = Style_LabsMarEq) %>%
                GrobPlot
            }, finally = dev.off())

        Tex2Pdf(path1, TRUE)
        Tex2Pdf(path2, TRUE)

        result$psd %>% head(30) %>%
            WriteFixed(
                fs::path(path_tbl,
                    glue("{quo_text(col)}_in_{band}_by_{by}.dat")),
                frmt = "%10.4f")
    }

    col <- quo(Py)
    err <- quo(SG)

    data %>% future_map(ff, col, err,
        fs::path("Output", "Plots", "PSD"),
        fs::path("Output", "Data", "PSD"))
    
        
    toc()
}