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

AverageBy <- function(data, bandInfo, by = 1) {

    data %>%
        mutate(
            MJD_ID__ = as.integer(floor(MJD / by))) %>%
        GroupBy(MJD_ID__) %>%
        DoWith(slice(., seq_len(8 * n() %/% 8))) %>%
        DoWith(
            mutate(Sigma_2(., bandInfo), MJD = mean(.$MJD)),
            .export = list(bandInfo = bandInfo)) %>%
        Ungroup() %>%
        arrange(MJD_ID__) %>%
        select(MJD, JD, everything(), -MJD_ID__)
}

if (ShouldRun) {
    AverageBy(data_1$B, Bands %>% filter(Band == "R")) %>% print
}