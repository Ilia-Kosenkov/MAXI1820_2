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

EstimateDifference <- function(psd, .data, t, x, w) {
    t <- enquo(t)
    x <- enquo(x)

    dtT <- pull(.data, !!t)
    dtX <- pull(.data, !!x)

    modelCS <- function(lAmpl, lW, lTau, lPhase)
        sd(dtX - as.numeric(lAmpl  * cos(lW * (dtT - lTau) + lPhase)))

    psd %>%
        arrange(desc(PSDN)) %>%
        mutate(
               SD1 = sd(dtX),
               SD2 = pmap_dbl(select(., Amplitude, W, Tau, CSPhase),
                        ~modelCS(..1, ..2, ..3, ..4)),
               Contrib = (SD1 - SD2) / SD1) %>%
        select(-SD1, -SD2) %>%
        arrange(desc(Contrib))
}