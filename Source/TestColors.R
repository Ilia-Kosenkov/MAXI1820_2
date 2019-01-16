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

TestColors <- function(bandInfo = Bands) {
    root <- fs::path("Input", "Temp")
    bNames <- bandInfo %>% pull(Band)

    Average <- function(path) {
        files <- fs::dir_ls(path, regexp = "csv")
        data <- bNames %>%
            map(~files[str_detect(files, glue("{.x}\\.csv"))]) %>%
            map(function(fList) {
                map(fList, read_csv,
                    col_types = cols("d", "d", "d")) %>%
                bind_rows %>%
                set_names(c("JD", "Obs", "Ref")) %>%
                filter(!is.na(Obs)) %>%
                arrange(JD)

        }) %>%
        set_names(bNames)

        data %>%
            map(mutate,
                MJD = JD - 2400000.5,
                FJD = as.integer(floor(MJD))) %>%
            map(group_by, FJD) %>%
            map(summarise,
                JD = mean(JD), MJD = mean(MJD),
                Avg = mean(Obs), Med = median(Obs),
                Err = sd(Obs), N = n()) %>%
            map(select, JD, MJD, Avg, Med, Err, N)

    }

    softData <- suppressWarnings(Average(fs::path(root, "Soft")))
    hard2Data <- suppressWarnings(Average(fs::path(root, "Hard2")))

    data2 <- map2(softData, hard2Data, bind_rows)

    files <- fs::dir_ls(root, type = "file")

    data1 <- bNames %>%
        map(~files[str_detect(files,
            regex(as.character(glue("{.x}\\.txt")), TRUE))]) %>%
        set_names(bNames) %>%
        map(read_table2, col_types = cols("d", "d", "d")) %>%
        map(set_names, c("JD", "Avg", "Err")) %>%
        map(mutate,
                MJD = JD - 2400000.5,
                Med = Avg,
                N = 1L) %>%
        map(arrange, JD)

    data <- map2(data1, data2, bind_rows)

    uId <- data %>% map(pull, MJD) %>%
        map(floor) %>% unlist(use.names = FALSE)

    uIdTbl <- tibble(FJD = uId) %>%
        distinct(FJD) %>%
        mutate(N = 1L:n())

    data %<>%
        map(~mutate(.x, ID = filter(uIdTbl, FJD %in% floor(.x$MJD))$N))

    selector <- quos(
        MJD > 58400 ~ "hard-2",
        MJD > 58300 ~ "soft",
        MJD > 58222 ~ "trans",
        TRUE ~ "hard-1")

    data %<>%
        map(mutate, Type = as.factor(case_when(!!!selector)))

    plots <- data %>%
        map(ggplot,
            aes(x = MJD, y = Avg,
                ymin = Avg - Err, ymax = Avg + Err,
                group = Type,
                col = Type,
                symbol = Type)) %>%
        map(~.x + geom_pointrange() + geom_line()) %>%
        map2(names(data), ~ .x + DefaultTheme() + scale_y_reverse(name = .y))
    n <- nrow(bandInfo)

    pairs <- seq_len(n - 1L) %>%
        map(function(x) map(seq(x + 1L, n), ~ c(x, .x))) %>%
        flatten %>%
        map(~pull(slice(bandInfo, .x), Band))

    pairs 
        

}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    TestColors() %>% print
}