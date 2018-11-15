.Initialize <- function() {

    library(scales)
    library(foreach)
    library(gridExtra)
    library(glue)
    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(stringr)

    library(RLibs)
    library(Dipol2Red)

    file.path("Source") %>%
        dir(pattern = ".R", full.names = TRUE, recursive = TRUE) %>%
        discard(str_detect, "Initialize\\.R") %>%
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
        set_names(Bands$Band)


}

makeActiveBinding("ShouldRun",
                  function() rlang::`%||%`(getOption(".IsInitialized"), FALSE),
                  .GlobalEnv)



if (!ShouldRun) {
    .Initialize()

    assign("data_1", .ReadData_1(), .GlobalEnv)

    options(.IsInitialized = TRUE)
}