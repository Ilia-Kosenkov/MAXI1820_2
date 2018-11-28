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

# ADOPTED FROM [glue] package; vignettes

.sprintf_transformer <- function(text, envir) {
    m <- regexpr(":%.+$", text)
    if (m != -1) {
        format <- substring(regmatches(text, m), 2)
        regmatches(text, m) <- ""
        res <- eval(parse(text = text, keep.source = FALSE), envir)
        do.call(sprintf, list(glue("{format}"), res))
    } else {
        eval(parse(text = text, keep.source = FALSE), envir)
    }
}

glue_fmt <- function(...,  .envir = parent.frame()) {
    glue(..., .envir = .envir, .transformer = .sprintf_transformer)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    glue_fmt("{pi:%f}") %>% print
}