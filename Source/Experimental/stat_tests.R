average_and_correct <- function(data, corr) {
    data %>% map(fsigma_2) -> avg
    corr %>% group_split(Filter) %>%
        set_names(map_chr(., ~ pull(.x, Filter))) %>%
        map(select, Px, Py, Angle) %>%
        map(as.list) -> corr

    map(set_names(names(avg)), ~ exec(correct_pol, !!!set_names(append(avg[.x], corr[[.x]]), NULL)))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    data("BandInfo", package = "Dipol2Red")

    average_and_correct(data_2, BandInfo) -> d_0
    average_and_correct(data_3, BandInfo) -> d_1

    set_names(cc("B", "V", "R")) %>%
        map(~bind_rows(d_0[[.x]], d_1[[.x]])) %>%
        map(h_test) %>% print
}
