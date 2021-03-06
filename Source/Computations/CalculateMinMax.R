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

CalculateMinMax <- function(.data, col, err, col_min, col_max, sgm = 1) {
    col_min <- enquo(col_min)
    col_max <- enquo(col_max)
    col <- quo_squash(enquo(col))
    if (quo_is_missing(col_min))
        col_min <- GetMinMaxNames(!!col)$min
    else
        col_min <- quo_squash(col_min)
    if (quo_is_missing(col_max))
        col_max <- GetMinMaxNames(!!col)$max
    else
        col_max <- quo_squash(col_max)

    err <- enquo(err)

    .data %>% mutate(!!col_min := !!col - sgm * !!err,
                     !!col_max := !!col + sgm * !!err)
}