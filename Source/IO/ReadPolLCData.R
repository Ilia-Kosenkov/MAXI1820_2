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

ReadPolLCData <- function(pathTemp1 = file.path("Output", "Data", "pol_avg_"),
                          pathTemp2 = file.path("Input", "PR_MAXI_BVR.txt"),
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

    data2 <- suppressWarnings(
        read_table2(pathTemp2, col_type = cols(), skip = 4, comment = "#")) %>%
        filter(!is.na(NO)) %>%
        set_names(c("NO", "BandID", "Px", "Py", "P", "SG", "A", "SG_A",
            "N", "Phase", "JD")) %>%
        mutate(MJD = JD - 2400000.5) %>%
        mutate(ID = NO - min(NO) + maxID + 1) %>%
        mutate(P_min = P - SG, P_max = P + SG) %>%
        mutate(Px_min = Px - SG, Px_max = Px + SG) %>%
        mutate(Py_min = Py - SG, Py_max = Py + SG) %>%
        mutate(A_min = A - SG_A, A_max = A + SG_A) %>%
        select(ID, JD, MJD,
               P, SG, P_min, P_max,
               Px, Px_min, Px_max,
               Py, Py_min, Py_max,
               A, SG_A, A_min, A_max, BandID, NO) %>%
        SplitByGroups(BandID)

    nms2 <- data2 %>%
        map(pull, BandID) %>%
        map(mean) %>%
        map_chr(~bandInfo %>% filter(ID == .x) %>% pull(Band))

    data2 %<>% set_names(nms2) %>%
        map(select, - BandID, - NO) %>%
        map(mutate, Group = 2L)

    result <- map2(data1, data2, ~ bind_rows(.x, .y)) %>%
        map(arrange, ID) %>%
        map(mutate, Group = as.factor(Group))

}