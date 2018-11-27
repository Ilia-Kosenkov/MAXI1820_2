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

AverageFieldStars <- function(data = field_stars, which = c(2, 3, 6, 7, 9)) {
    data %>%
        map(filter, Star %in% which) %>%
        map(mutate, W = 1 / SG ^ 2) %>%
        map(summarise,
            JD = mean(JD), MJD = mean(MJD),
            SW = sum(W),
            Px = as.numeric(Px %*% W / SW),
            Py = as.numeric(Py %*% W / SW),
            SG = sqrt(1 / SW)) %>%
        map(CalculatePolFromQU) %>%
        map(select, - SW) %>%
        map(select, JD, MJD, P, Px, Py, SG, A, SG_A, everything()) %>%
        map(CalculateMinMax, P, SG) %>%
        map(CalculateMinMax, Px, SG) %>%
        map(CalculateMinMax, Py, SG) %>%
        map(CalculateMinMax, A, SG_A)
}