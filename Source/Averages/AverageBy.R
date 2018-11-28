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

    selector_c <- function(rn, n)
        (rn - 1) %/% ifelse(is.na(by_obs), n, by_obs) + 1
    selector_d <- function(col) as.integer(floor(col / by))

    ftrs <- data %>%
        mutate(ID_1__ = selector_d(MJD)) %>%
        SplitByGroups(ID_1__) %>%
        map(mutate, ID_2__ = selector_c(row_number(), n())) %>%
        map(SplitByGroups, ID_2__) %>%
        flatten %>%
        future_map(~Sigma_2(.x, bandInfo)) %>%
        bind_rows %>%
        arrange(JD) %>%
        mutate(MJD = JD - 2400000.5) %>%
        mutate(Px_min = Px - SG, Px_max = Px + SG) %>%
        mutate(Py_min = Py - SG, Py_max = Py + SG) %>%
        mutate(P_min = P - SG, P_max = P + SG) %>%
        mutate(A_min = A - SG_A, A_max = A + SG_A) %>%
        select(JD, MJD,
               P, SG, P_min, P_max,
               Px, Px_min, Px_max,
               Py, Py_min, Py_max,
               A, SG_A, A_min, A_max,
               Cov, N, Ratio, Itt)
}

#if (get0("ShouldRun", ifnotfound = FALSE)) {
if (FALSE) {

    dirPath <- file.path("Output", "Data")

    if (!dir.exists(dirPath))
        dir.create(dirPath, recursive = TRUE)

    Bands %>%
        SplitByGroups(ID) %>%
        future_map(function(grp) {
            fPath <- file.path(dirPath, glue("pol_avg_all_0_{grp$Band}.dat"))
            AverageBy(data_0[[grp$Band]], grp, by = 1000) %>%
                WriteFixed(
                    path = fPath,
                    frmt = c(
                        rep("%20.8f", 2),
                        rep("%15.8f", ncol(.) - 5),
                        "%8d", "%10.2f", "%8d"))
    })

}