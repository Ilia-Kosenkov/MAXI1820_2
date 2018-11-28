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

FoldWithPeriod <- function(data, x, period = 17.0 / 24.0) {
    x <- enquo(x)

    x_p <- sym(quo_name(x) %&% "_p")

    data %>%
        mutate(!!x_p := !!x - min(!!x)) %>%
        mutate(!!x_p := (!!x_p / period)) %>%
        mutate(!!x_p := !!x_p - floor(!!x_p)) %>%
        arrange(!!x_p)
}

#if (get0("ShouldRun", ifnotfound = FALSE)) {
if (FALSE) {
    tic()

    src_1 <- Bands$Band
    src_2 <- c(16, 32, 80, NA)

    drPath <- path("Output", "Data", "Folded")
    dir_create(drPath)

    crossing(src_1, src_2) %>%
        future_pmap(~data_2[[..1]] %>%
            AverageBy(bandInfo = Bands %>% filter(Band == ..1),
                by = 1,
                by_obs = ..2) %>%
            FoldWithPeriod(MJD, 17.0 / 24.0) %>%
            select(JD, MJD, MJD_p, everything()) %>%
            WriteFixed(path(
                drPath,
                glue("folded_{..1}_by_{ifelse(is.na(..2), \"all\", ..2)}.dat")),
                frmt = c(
                        rep("%20.8f", 3),
                        rep("%15.8f", ncol(.) - 6),
                        "%8d", "%10.2f", "%8d")))

    toc()
}