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

GenerateInverseBreaks <- function(xlim, n = 6, modifiers = c(1, 2, 5)) {

    rng <- (1 / xlim) %>% rev %>% diff

    if (rng > 1) {

        grade <- xlim[1] %>% log10 %>% floor

        tol <- rev(1 / xlim) %>% diff %>% divide_by(6 * n)

        z <- seq(0, grade, by = -1L) %>% map(~modifiers * 10 ^ .x) %>%
            unlist %>%
            sort
        z <- sort(1 / UniqueTol(1 / z, tol))
        z <- z[WithinL(1 / z, 1 / xlim[2], 1 / xlim[1])]
        smlMod <- setdiff(1:9, modifiers)

        u <- z %>% Log10Floor %>% map(~smlMod * .x) %>%
            unlist %>% sort %>% unique

        return(list(Large = z, Small = u))
    }
    else {
        step <- FancyStep(xlim, modifier = c(1, 2, 5))
        return(GenerateBreaks(xlim, step, 0.1 * step))
    }

}
