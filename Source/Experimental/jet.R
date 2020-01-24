parse_jet_data <- function(path = fs::path("Input", "jet.txt")) {
    path %>%
        read_table2(col_names = FALSE,
            col_types = cols(
                col_double(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_character(),
                col_factor())) %>%
        set_names(cc(
            "MJD",
            "RA_cr", "DEC_cr", "?_cr",
            "RA_ap", "DEC_ap", "?_ap",
            "RA_rc", "DEC_rc", "?_rc",
            "Type")) %>%
        mutate_all(~na_if(., "-")) %>%
        mutate_at(vars(starts_with("?")), parse_double) %>%
        mutate_at(vars(starts_with("RA")), enframe, NULL, "Src") %>%
        mutate_at(
            vars(starts_with("RA")),
            ~ separate(.,
                col = Src,
                into = cc("H", "M", "S"), 
                sep = ":",
                remove = FALSE)) #%>%
        #select(RA_cr)


}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    parse_jet_data() %>% print(n = len(.))
}