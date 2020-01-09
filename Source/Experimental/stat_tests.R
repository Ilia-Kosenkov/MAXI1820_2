average_and_correct <- function(data, corr) {
    data %>% map(fsigma_2) -> avg
    corr %>% group_split(Filter) %>%
        set_names(map_chr(., ~ pull(.x, Filter))) %>%
        map(select, Px, Py, Angle) %>%
        map(as.list) -> corr

    map(set_names(names(avg)), ~ exec(correct_pol, !!!set_names(append(avg[.x], corr[[.x]]), NULL))) %>%
        merge_and_enumerate
}

## TODO : move to RLibs
list_collapse <- function(data, to = Name, ptype = factor()) {
    imap_dfr(data, ~ mutate(.x, {{ to }} := vec_cast(.y, ptype)))
}

## TODO : move to RLibs
map_lo <- function(.x, .f, ...) {
    list_of(!!!map(.x, .f, ...))
}

average_and_correct_field <- function(data = AverageFieldStars()) {
    data %>%
        list_collapse(to = Filter) %>%
        select(Filter, JD, Px, Py, P, SG, A, SG_A, N) %>%
        mutate(
            S = (N - 1) * SG ^ 2,
            Q = map(S, ~ matrix(cc(.x, 5e-3, .x, 5e-3), byrow = TRUE, nrow = 2))) %>%
        select(-S)
}

merge_and_enumerate <- function(data) {
    map(data, mutate, ID = 1:n()) %>% list_collapse(to = Filter)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    #data("BandInfo", package = "Dipol2Red")

    average_and_correct(data_2, BandInfo) -> d_0
    #average_and_correct(data_3, BandInfo) -> d_1
    average_and_correct_field() -> d_1
    print(d_0)
    print(d_1)

    h_test2(d_0, d_1, id = Filter) %>% print

}
