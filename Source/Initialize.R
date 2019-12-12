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

.LoadLibs <- function() {
    library(RColorBrewer)
    library(scales)
    library(foreach)
    library(gridExtra)
    library(glue)
    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(stringr)
    library(tidyselect)

    library(future)
    library(furrr)

    library(grid)
    library(gridExtra)
    library(tikzDevice)

    library(RLibs)
    library(Dipol2Red)

    # experimental parallel
    #library(doSNOW)
    #library(foreach)
    library(fs)
    library(assertthat)
    library(tictoc)

    library(vctrs)
    library(zeallot)

}

cc <- vctrs::vec_c
len <- vctrs::vec_size

.Initialize <- function() {
    .LoadLibs()

    #file.path("Source") %>%
        #dir(pattern = ".R", full.names = TRUE, recursive = TRUE) %>%
        #purrr::discard(str_detect, "Initialize\\.R") %>%
    #walk(source)
    RLibs::source_all(fs::path("Source"), "Initialize\\.R")

    Bands <<- read_table(
            fs::path("Input", "Bands.dat"),
            col_types = cols()) %>%
        mutate(Freq = Const.c / (WL * 1e-8))
}

.ReadData_0 <- function(path = fs::path("Input", "maxi1820_0")) {
    files <- fs::dir_ls(path, glob = "*.csv")
    
    Bands$Band %>%
        map_chr(~glue("maxi?[0-9]+{tolower(.x)}[0-9]+\\.csv")) %>%
        map(~files[str_detect(files, .x)]) %>%
        map(~map(.x, ~ mutate(read_csv(.x, col_types = cols()),
            Path = !!.x
            ))) %>%
        map(bind_rows) %>%
        map(set_names, cc("JD", "Ref", "Obs", "Path")) %>%
        map(mutate, MJD = JD - 2400000.5) %>%
        map(arrange, MJD) %>%
        set_names(Bands$Band)

}

.ReadData_1 <- function(path = fs::path("Input", "maxi1820_1")) {
    files <- fs::dir_ls(path, glob = "*.csv")

    Bands$Band %>%
        map_chr(~glue("maxi?[0-9]+{tolower(.x)}[0-9]+\\.csv")) %>%
        map(~files[str_detect(files, .x)]) %>%
        map(~map(.x, ~ (read_csv(.x, col_types = cols())))) %>%
        map(reduce, bind_rows) %>%
        map(set_names, cc("JD", "Ref", "Obs")) %>%
        map(mutate, MJD = JD - 2400000.5) %>%
        map(arrange, JD) %>%
        set_names(Bands$Band)

}

.ReadData_2 <- function(path = fs::path("Input", "maxi1820_2")) {
    files <- fs::dir_ls(path, glob = "*.csv")

    Bands$Band %>%
        map_chr(~glue("maxi?{.x}[0-9]*\\.csv")) %>%
        map(~files[str_detect(files, .x)]) %>%
        map(~map(.x, ~ (read_csv(.x, col_types = cols())))) %>%
        map(reduce, bind_rows) %>%
        map(set_names, cc("JD", "Ref", "Obs")) %>%
        map(arrange, JD) %>%
        map(mutate, MJD = JD - 2400000.5) %>%
        set_names(Bands$Band)

}

.ReadData_3 <- function(path = fs::path("Input", "maxi1820_3")) {
    files <- fs::dir_ls(path, glob = "*.csv")

    Bands$Band %>%
        map_chr(~glue("maxi?[0-9]*{tolower(.x)}.*csv")) %>%
        map(~files[str_detect(files, .x)]) %>%
        map(~map(.x, ~ (read_csv(.x, col_types = cols())))) %>%
        map(reduce, bind_rows) %>%
        map(set_names, cc("JD", "Ref", "Obs")) %>%
        map(arrange, JD) %>%
        map(mutate, MJD = JD - 2400000.5) %>%
        set_names(Bands$Band)

}

.ReadFieldStars <- function(pth = fs::path("Input", "field_stars_pol.dat")) {
    data <- read.table(pth, header = TRUE, stringsAsFactors = FALSE) %>%
        as_tibble %>%
        set_names(c(
            "Star", "BandID",
            "Px", "Py", "P", "SG",
            "A", "SG_A", "N", "Phase", "JD")) %>%
        filter(Star >= 700) %>%
        mutate(Star = Star - 700L, MJD = JD - 2400000.5) %>%
        select(Star, JD, MJD, Px, Py, P, SG, A, SG_A, N, BandID) %>%
        group_split(BandID)

    nms <- data %>%
        map(~mean(pull(.x, BandID))) %>%
        map_chr(~Bands %>% filter(ID == .x) %>% pull(Band))

    data %>% set_names(nms) %>%
        map(select, - BandID) %>%
        map(CalculateMinMax, P, SG) %>%
        map(CalculateMinMax, Px, SG) %>%
        map(CalculateMinMax, Py, SG) %>%
        map(CalculateMinMax, A, SG_A)
}

if (!exists("ShouldRun", envir = .GlobalEnv))
    makeActiveBinding("ShouldRun",
                  function() getOption(".IsInitialized", FALSE),
                  .GlobalEnv)

if (exists("ShouldRun", envir = .GlobalEnv) &&
    !bindingIsLocked("ShouldRun", .GlobalEnv))
    lockBinding("ShouldRun", .GlobalEnv)

.GetChildWorkers <- function() {
    x <- future::nbrOfWorkers()

    if (x != 1L)
        wrks <- c(x, value(future({ .GetChildWorkers() })))
    else
        wrks <- x
    wrks
}

.GetTopology <- function() {
    #wrks <- .GetChildWorkers()
    #if (length(wrks) != 1)
        #wrks <- wrks[-length(wrks)]

    #wrks
    lifecycle::deprecate_warn("0.0", ".GetTopology()", "RLibs::get_topology()")
    RLibs::get_topology()
}

.PlanCL <- function(...) {
    lifecycle::deprecate_warn("0.0", ".PlanCL()", "RLibs::plan_cluster()")
    RLibs::plan_cluster(...)
    #args <- list(...) %>%
        #map_int(as_integer)
    #if (is_empty(args) || some(args, ~ .x == 0L))
        #stop("Cannot create empty cluster")
    #if (some(args, ~ .x == 1L) && length(args) != 1L)
        #stop("Cannot have 1-proc-sized clusters")


    #topology <- .GetTopology()

    #if (length(args) != length(topology) || !all(args == topology)) {
        #args %>% map(function(sz) {
            #if (sz == 1L)
                #return(tweak(future::sequential))
            #else
                #return(tweak(future::cluster, workers = sz))
            #}) %>% plan
    #}

    #message(glue("Cluster: [{glue_collapse(.GetTopology(), sep = \", \")}]"))

}

# Bypassing exporting issues
#`%&%` <- RLibs::`%+%`

if (!(get0("ShouldRun", ifnotfound = FALSE))) {
    .Initialize()
    assign("data_0", .ReadData_0(), .GlobalEnv)
    assign("data_1", .ReadData_1(), .GlobalEnv)
    assign("data_2", .ReadData_2(), .GlobalEnv)
    assign("data_3", .ReadData_3(), .GlobalEnv)

    assign("field_stars", .ReadFieldStars(), .GlobalEnv)
    options(.IsInitialized = TRUE)
}
