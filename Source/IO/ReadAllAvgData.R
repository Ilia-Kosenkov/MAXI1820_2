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

ReadAllAvgData <- function(
    dir_path = fs::path("Output", "Data"),
    pattern = "pol_avg_(?<id>[0-9]+)_(?<band>\\w)") {

    files <- fs::dir_ls(dir_path) %>%
        StrExtractNamedGroups(pattern) %>%
        select(Src, id, band) %>%
        future_pmap(~read_table2(..1, col_types = cols(), comment = "#") %>%
            mutate(Group = !!..2, Band = !!..3)) %>%
        bind_rows %>%
        #SplitByGroups(Band, .names = Band) %>%
        group_split(Band) %>%
        set_names(map_chr(., ~ pull(.x, Band)[1])) %>%
        map(arrange, MJD) %>%
        map(mutate, Group = as_factor(Group)) %>%
        map(nest_input)
}
