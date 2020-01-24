parse_jet_data <- function(path = fs::path("Input", "jet.txt")) {
    path %>%
        read_table2(col_names = FALSE, col_types = cols()) %>%
        set_names(cc(
            "MJD",
            "RA_cr", "DEC_cr", "?_cr",
            "RA_ap", "DEC_ap", "?_ap",
            "RA_rc", "DEC_rc", "?_rc",
            "Type")) %>%
        mutate(Type = as_factor(Type)) %>%
        mutate_all(~na_if(., "-")) %>%
        mutate_at(vars(starts_with("?")), ~if (vec_is(., character())) parse_double(.) else .)


}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    parse_jet_data() %>% print(n = len(.))
}