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

AverageBy <- function(data, bandInfo, by = 1, by_obs = NA) {

    if (!all(is.na(by_obs)))
        selector <- function(col, rn) (rn - 1) %/% by_obs + 1
    else
        selector <- function(col, ...) as.integer(floor(col / by))

    ftrs <- data %>%
        mutate(MJD_ID__ = selector(MJD, row_number())) %>%
        SplitByGroups(MJD_ID__) %>%
        future_map(~Sigma_2(.x, bandInfo)) %>%
        bind_rows %>%
        arrange(JD) %>%
        mutate(MJD = JD - 2400000.5) %>%
        select(JD, MJD, everything())
}

if (FALSE) {

    dirPath <- file.path("Output", "Data")

    if (!dir.exists(dirPath))
        dir.create(dirPath, recursive = TRUE)

    Bands %>%
        SplitByGroups(ID) %>%
        future_map(function(grp) {
            fPath <- file.path(dirPath, glue("pol_avg_{grp$Band}.dat"))
            AverageBy(data_1[[grp$Band]], grp) %>%
                WriteFixed(
                    path = fPath,
                    frmt = c(
                        rep("%20.8f", 2),
                        rep("%15.8f", 7),
                        "%8d", "%10.2f", "%8d"))
    }) %>% print
}