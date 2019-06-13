# MIT License
#
# Copyright(c) 2019 Ilia Kosenkov[ilia.kosenkov.at.gm@gmail.com]
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

nest_input <- function(data) {

    on_failure(is_tibble) <- function(call, env)
        paste(deparse(call[[2]]), "is not tibble")
    assert_that(is.data.frame(data) || is_tibble(data))

    names <- names(data)

    detections <-
        str_match_all(names, "^(.*)__([0-9]+)__([0-9]+)$") %>%
        discard(is_empty)

    if (is_empty(detections))
        return(data)

    matrix_cols <- detections %>%
        reduce(rbind) %>%
        `colnames<-`(vec_c("Source", "Column", "N", "M")) %>%
        as_tibble %>%
        mutate_at(vars(N, M), parse_integer) %>%
        group_by(Column) %>%
        Nest %>%
        mutate(data = map2(data, Column, ~ mutate(.x, Column = .y))) %>%
        pull(data) %>%
        map(arrange, M, N)

    quotes <- matrix_cols %>% map(function(x) {
        col_vars <- x %>% pull(Source) %>% syms
        dims <- x %>% summarise(N = max(N), M = max(M)) %>% flatten_int
        quo(pmap(list(!!!col_vars), function(...) {
            matrix(data = as.numeric(list(...)), nrow = dims[1], ncol = dims[2])
        }))
    }) %>% set_names(matrix_cols %>% map_chr(~pull(.x, Column) %>% first))

    deselects <- matrix_cols %>% map(pull, Source) %>% flatten_chr %>% syms %>%
        map(~quo(-!!.x))

    data %>% mutate(!!!quotes) %>% select(!!!deselects)
}