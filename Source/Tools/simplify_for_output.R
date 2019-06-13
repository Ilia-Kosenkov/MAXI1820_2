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

simplify_for_output <- function(data) {

    process_matrix <- function(input, name) {
        assert_that(is.string(name))
        dims <- input %>% map(dim)
        assert_that(vec_size(unique(dims)) == 1)
        dims <- dims %>% first

        indexes <- cross2(1:dims[1], 1:dims[2])
        names <- map(indexes, ~ glue("{name}__{.x[[1]]}__{.x[[2]]}"))

        type <- input[[1]] %>% type_of

        tibble_setup <- rep(list(vector(type, 0)), length(names)) %>% set_names(names)

        tibble <- tibble(!!!tibble_setup)

        input %>% map(as.vector) %>% map(set_names, names) %>% reduce(bind_rows, .init = tibble)
    }

    on_failure(is_tibble) <- function(call, env)
        paste(deparse(call[[2]]), "is not tibble")
    assert_that(is.data.frame(data) || is_tibble(data))

    which <- data %>% discard(is_atomic)
    

    which %>%
        walk2(names(.),
              ~ assert_that(
                type_of(.x) == "list",
                msg = glue("Cannot process nested column `{.y}` that is not a list")))

    types <- which %>%
        map(~map_chr(.x, class)) %>%
        walk2(names(.),
            ~ assert_that(
                all(.x == .x[1]),
                msg = glue("Classes of elements of nested column `{.y}` are not same"))) %>%
        map(first)

    result <- which %>% map2(names(.),
                   ~switch(class(.x[[1]]), 
                   "matrix" = process_matrix(.x, .y),
                   stop("Type is unsupported")))

    names <- names(which)
    exclude_cols <- names %>% syms %>% map(~quo(-!!.x)) 

    result %>% reduce(bind_cols, .init = data) %>% select(!!!exclude_cols)
}