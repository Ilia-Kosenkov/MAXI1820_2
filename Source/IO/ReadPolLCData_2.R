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

ReadPolLCData_2 <- function(pathTemp1 = file.path("Output", "Data", "pol_avg_"),
                          pathTemp2 = file.path("Output", "Data", "pol_avg_2_"),
                          bandInfo = Bands) {

    data1 <- bandInfo %>% pull(Band) %>% map(~glue("{pathTemp1}{.x}.dat")) %>%
        map(read_table2, col_types = cols(), comment = "#")

    uniqueDates <- data1 %>% map(pull, JD) %>%
        unlist %>% UniqueTol(tol = 5e-1) %>% sort

    data1 %<>% map(mutate, ID = Intersect(JD, uniqueDates, tol = 5e-1)[[2]]) %>%
        set_names(bandInfo$Band) %>%
        map(select, ID, JD, MJD, everything()) %>%
        map(mutate, Group = 1L)


    maxID <- data1 %>% map_dbl(~max(.x$ID)) %>% max

    data2 <- bandInfo %>% pull(Band) %>% map(~glue("{pathTemp2}{.x}.dat")) %>%
        map(read_table2, col_types = cols(), comment = "#")

    uniqueDates <- data2 %>% map(pull, JD) %>%
        unlist %>% UniqueTol(tol = 5e-1) %>% sort

    data2 %<>% map(mutate, ID = Intersect(JD, uniqueDates, tol = 5e-1)[[2]]) %>%
        set_names(bandInfo$Band) %>%
        map(select, ID, JD, MJD, everything()) %>%
        map(mutate, Group = 2L) %>%
        map(mutate, ID = ID - min(ID) + maxID + 1)


    result <- map2(data1, data2, ~ bind_rows(.x, .y)) %>%
        map(arrange, ID) %>%
        map(mutate, Group = as.factor(Group))

}