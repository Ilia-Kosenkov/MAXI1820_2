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


    file.path("Source") %>%
        dir(pattern = ".R", full.names = TRUE, recursive = TRUE) %>%
        discard(str_detect, "Initialize\\.R") %>%
        walk(source)
}

makeActiveBinding("ShouldRun",
                  function() rlang::`%||%`(getOption(".IsInitialized"), FALSE),
                  .GlobalEnv)

if (!ShouldRun) {
    .Initialize()
    options(.IsInitialized = TRUE)
}