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
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE

ModifySegemnts <- function(.data, x, y, scale = 1, shift = 0) {
    x1 <- enquo(x)
    x2 <- sym(paste0(quo_name(x1), "_end"))
    y1 <- enquo(y)
    y2 <- sym(paste0(quo_name(y1), "_end"))

    .data %>%
        AsSegments(!!x1, !!y1) %>%
        mutate(
            Mdl__ = sqrt((!!x1 - !!x2) ^ 2 + (!!y1 - !!y2) ^ 2),
            Angle__ = atan2(!!y1 - !!y2, !!x1 - !!x2),
            Xc__ = 0.5 * (!!x1 + !!x2),
            Yc__ = 0.5 * (!!y1 + !!y2)) %>%
        mutate(Mdl__ = scale * (Mdl__ + shift)) %>%
        mutate(
            !!quo_name(x1) := Xc__ + 0.5 * Mdl__ * cos(Angle__),
            !!quo_name(x2) := Xc__ - 0.5 * Mdl__ * cos(Angle__),
            !!quo_name(y1) := Yc__ + 0.5 * Mdl__ * sin(Angle__),
            !!quo_name(y2) := Yc__ - 0.5 * Mdl__ * sin(Angle__)) %>%
        select(-Xc__, -Yc__, -Mdl__, -Angle__)
}