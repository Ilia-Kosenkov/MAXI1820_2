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

LinearScaleTicks <- function(plt, rng,
        side = "x", mirror = TRUE, brTrans = identity,
        n = 6, mod = c(1, 2, 5), ...) {
    step <- FancyStep(rng, n, mod)
    breaks <- GenerateBreaks(rng, step, 0.1 * step)

    if (side == "x") {
        plt <- plt +
            scale_x_continuous(
                breaks = breaks$Small,
                labels = rep(" ", length(breaks$Small)),
                sec.axis = if (mirror) dup_axis(name = "") else NULL)

        args <- append(list(
                plt = plt, side = "bot",
                breaks = breaks$Large,
                labels = brTrans(breaks$Large)),
            list(...))

        plt <- do.call("GGPlotCustomTicksEx2", args)

        if (mirror) {
            args$side <- "top"
            args$labels <- rep(" ", length(breaks$Large))
            args$plt <- plt
            plt <- do.call("GGPlotCustomTicksEx2", args)
        }
    }

    else if (side == "y") {
        plt <- plt +
            scale_y_continuous(
                breaks = breaks$Small,
                labels = rep(" ", length(breaks$Small)),
                sec.axis = if (mirror) dup_axis(name = "") else NULL)

        args <- append(list(
                plt = plt, side = "left",
                breaks = breaks$Large,
                labels = brTrans(breaks$Large)),
            list(...))

        plt <- do.call("GGPlotCustomTicksEx2", args)

        if (mirror) {
            args$side <- "right"
            args$labels <- rep(" ", length(breaks$Large))
            args$plt <- plt
            plt <- do.call("GGPlotCustomTicksEx2", args)
        }
    }

    plt
}