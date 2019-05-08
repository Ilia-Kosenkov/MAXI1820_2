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


Data2TeXTable <- function(data) {

    bnds <- Bands$Band

    tbl <- data %>%
        map(select, MJD, P, SG, A, SG_A) %>%
        map(mutate, Id = as_integer(floor(MJD / 10))) %>%
        map(mutate_at, vars(P, SG), ~ sprintf("%.2f", round(., 2))) %>%
        map(mutate_at, vars(A, SG_A), ~ sprintf("%.1f", round(., 1))) %>%
        map(separate, P, vec_c("P1", "P2")) %>%
        map(separate, SG, vec_c("SG1", "SG2")) %>%
        map(separate, A, vec_c("A1", "A2")) %>%
        map(separate, SG_A, vec_c("SG_A1", "SG_A2")) %>%
        map2(names(data), ~ rename_at(.x, vars(-MJD, - Id), function(item) paste(item, .y, sep = "_"))) %>%
        reduce(~full_join(.x, .y, by = "Id"))

    mjdExprs <- tbl %>% names %>% str_subset("^MJD") %>% map(sym) %>% { quo(pmap_dbl(list(!!!.), function(...) mean(flatten_dbl(list2(...)), na.rm = TRUE))) }
    tbl %<>%
        mutate(MJD = !!mjdExprs) %>%
        select(MJD, everything(), - matches("\\.\\w$"), - Id) %>%
        arrange(MJD) %>% 
        mutate(MJD = sprintf("%.4f", MJD))

    q <- bnds %>% map(~quo(ends_with(!!.x)))

    tbl %<>% select(MJD, !!!q)
        

    col_types <- vec_c("c", vec_repeat(vec_c("r", "@{$.$}l", "@{$~\\pm~$}r", "@{$.$}l"), 1L, 2L * vec_size(data))) #%>% print

    before <- bnds %>% map_chr(~glue("\\multicolumn{{8}}{{c}}{{${.x}$}}")) %>% glue_collapse(sep = " & ")
    before <- glue("    & {before} \\\\")
    before2 <- vec_c("MJD", vec_repeat(vec_c("\\multicolumn{4}{c}{$P$}", "\\multicolumn{4}{c}{$\\theta$}"), 1L, vec_size(data)), "") %>%
        glue_collapse(sep = " & ", last = " \\\\")

    after <- vec_c("   ", vec_repeat(vec_c("\\multicolumn{4}{c}{per cent}", "\\multicolumn{4}{c}{deg}"), 1L, vec_size(data)), "\\\\\r\n    \\hline") %>%
        glue_collapse(sep = " & ", last = " ")
    return(list(tbl, col_types, paste(before, before2, sep = "\r\n        "), after))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {

    dir <- fs::path("Output", "Tables") %T>% (fs::dir_create)
    path <- fs::path(dir, "average_pol.tex")
    
    avgPerNight <<- ReadAllAvgData(pattern = "pol_avg_all_(?<id>[0123]+)_(?<band>\\w)")
        
    avgPerNight %>% Data2TeXTable %->% c(tbl, layout, before, after)
    table_2_tex(tbl, path, before = before, after = after, 
                format = vec_c("%10s", vec_repeat("%3s", ncol(tbl) - 1)), col_layout = layout, print_header = FALSE)

    lines <- read_lines(path) %>% str_replace_all("(?:\\s*NA\\s*(?:&|\\\\)){4}", function(s) {
        format <- glue("%{nchar(s)}s")
        str <- "\\multicolumn{4}{c}{$-$}"
        str <- if_else(str_ends(s, "&"), paste(str, "&"), paste(str, "\\\\"))
        sprintf(format, str)
    })
    lines %>% write_lines(path)
}