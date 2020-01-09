average_and_correct <- function(data, corr) {
    data %>% map(fsigma_2) -> avg
    corr %>% group_split(Filter) %>%
        set_names(map_chr(., ~ pull(.x, Filter))) %>%
        map(select, Px, Py, Angle) %>%
        map(as.list) -> corr

    map(set_names(names(avg)), ~ exec(correct_pol, !!!set_names(append(avg[.x], corr[[.x]]), NULL))) %>%
        merge_and_enumerate
}

merge_and_enumerate <- function(data) {
    imap_dfr(data, ~ mutate(.x, Filter = as_factor(.y), ID = 1:n()))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    data("BandInfo", package = "Dipol2Red")

    average_and_correct(data_2, BandInfo) -> d_0
    average_and_correct(data_3, BandInfo) -> d_1

    h_test2(d_0, d_1, id = Filter) %>% print
}
