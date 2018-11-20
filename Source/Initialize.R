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

.LoadLibs <- function()
{
    library(RColorBrewer)
    library(scales)
    library(foreach)
    library(gridExtra)
    library(glue)
    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(stringr)
    library(future)
    library(furrr)

    library(tikzDevice)

    library(RLibs)
    library(Dipol2Red)

    # experimental parallel
    library(doSNOW)
    library(foreach)

}

.Initialize <- function() {
    .LoadLibs()

    file.path("Source") %>%
        dir(pattern = ".R", full.names = TRUE, recursive = TRUE) %>%
        purrr::discard(str_detect, "Initialize\\.R") %>%
        walk(source)

    Bands <<- read_table(file.path("Input", "Bands.dat"), col_types = cols())
}

.ReadData_1 <- function(path = file.path("Input", "maxi1820")) {
    files <- dir(path, pattern = ".csv")

    Bands$Band %>%
        map_chr(~glue("maxi{.x}[0-9]*\\.csv")) %>%
        map(~files[str_detect(files, .x)]) %>%
        map(~map(.x, ~ (read_csv(file.path(path, .x), col_types = cols())))) %>%
        map(reduce, bind_rows) %>%
        map(set_names, c("JD", "Ref", "Obs")) %>%
        map(arrange, JD) %>%
        map(mutate, MJD = JD - 2400000.5) %>%
        set_names(Bands$Band)

}

.SetupCluster <- function() {
    library(parallel)
    library(doSNOW)

    cl <- Cluster$new(max(detectCores() - 2, 2))

    cl$Register()

    return(cl)
}


makeActiveBinding("ShouldRun",
                  function() getOption(".IsInitialized", FALSE),
                  .GlobalEnv)

.PlanCL <- function(...) {
    args <- list(...)

    if (length(args) == 2L) {
        x <- args[[1]]
        y <- args[[2]]
    }
    else if (length(args) == 1L) {
        x <- args[[1]][1]
        y <- args[[1]][2]
    }
    else stop("Wrong cluster dimensions")

    if (x == 0L)
        higher <- tweak(sequential)
    else
        higher <- tweak(cluster, workers = x)

    if (y == 0L)
        lower <- tweak(sequential)
    else
        lower <- tweak(cluster, workers = y)

    plan(list(higher, lower))

    future_map(seq_len(max(x, 1)),
            ~ future_map_int(seq_len(max(y, 1)), ~ Sys.getpid())) %>%
        set_names(future_map(seq_len(max(x, 1)), ~Sys.getpid())) %>% bind_cols
}

# Bypassing exporting issues
`%++%` <- RLibs::`%+%`

if (!ShouldRun) {
    .Initialize()
    assign("data_1", .ReadData_1(), .GlobalEnv)
    options(.IsInitialized = TRUE)
}
