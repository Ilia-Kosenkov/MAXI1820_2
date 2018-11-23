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

GetRange <- function(.data, col, err, col_min, col_max, .func = range) {
    col <- enquo(col)
    err <- enquo(err)
    col_min <- enquo(col_min)
    col_max <- enquo(col_max)

    if (quo_is_missing(err)) {
        if (quo_is_missing(col_min))
            col_min <- GetMinMaxNames(!!col)$min
        if (quo_is_missing(col_max))
            col_max <- GetMinMaxNames(!!col)$max

        transmute(.data, L = !!col_min, U = !!col_max) %>%
            map(.func) %>%
            unlist %>%
            .func
    }
    else
        transmute(.data, L = !!col - !!err, U = !!col + !!err) %>%
            map(.func) %>%
            unlist %>%
            .func
}