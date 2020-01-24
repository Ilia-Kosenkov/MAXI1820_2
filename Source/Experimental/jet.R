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
        mutate_at(vars(starts_with("DEC")), enframe, NULL, "Src") -> tmp


    hms_parser <-
        (~separate(
            .x,
            col = Src,
            into = cc("H", "M", "S"),
            sep = ":")) %>>%
        (~mutate_all(.x, parse_double)) %>>%
        (~mutate(.x, Value = 15 * (H + M / 60 + S / 3600))) %>>%
        (~pull(.x, Value) / 180 * pi)
    dms_parser <-
        (~separate(
            .x,
            col = Src,
            into = cc("D", "M", "S"),
            sep = ":")) %>>%
        (~mutate_all(.x, parse_double)) %>>%
        (~mutate(.x, Value = D + M / 60 + S / 3600)) %>>%
        (~pull(.x, Value) / 180 * pi)

    tmp %>%
        mutate_at(vars(starts_with("RA")), hms_parser) %>%
        mutate_at(vars(starts_with("DEC")), dms_parser) %>%
        mutate(Id = 1:n())
}

test_compute <- function(data, from) {
    data %>%
        transmute(Id, MJD, A_1 = RA_cr, D_1 = pi / 2 - DEC_cr) -> center

    data %>%
        transmute(
            Id,
            A_2 = !!sym(glue_fmt_chr("RA_{from}")),
        D_2 = pi / 2 - !!sym(glue_fmt_chr("DEC_{from}"))) %>%
        filter(!is.na(A_2) & !is.na(D_2)) -> points

    func <- quo(
        (cos(D_2) * (1 - cos(D_1) ^ 2) - sin(D_1) * sin(D_2) * cos(D_1) * cos(A_2 - A_1)) /
        (sin(D_1) * sin(D_2) * sin(A_2 - A_1)))

    inner_join(center, points, by = "Id") %>%
        mutate(TgTh = 1 / !!func, Angle = atan(TgTh) * 180 / pi) %>%
        transmute(Id, MJD, !!sym(glue_fmt_chr("Angle_{from}")) := Angle)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    parse_jet_data() -> data

    data %>% test_compute(from = "rc") -> rc_data
    data %>% test_compute(from = "ap") -> ap_data

    full_join(rc_data, ap_data, by = "Id") %>%
        arrange(Id) %>%
        mutate(
            MJD = map2_dbl(MJD.x, MJD.y, ~ mean(cc(.x, .y), na.rm = TRUE)),
            .keep = "unused") %>%
        select(Id, MJD, everything()) -> result

    path_out <- fs::path("Output", "jet_angles.dat")
    report_out <- fs::path("Output", "jet_stats.dat")

    result %>% write_fixed(path_out, frmt = cc("%6d", "%12.4f", "%10.3f", "%10.3f"))

    result %>%
        summarise_at(
            vars(starts_with("Angle")),
            ~glue_fmt_chr("{mean(.x, na.rm = TRUE):%.2f}° ± {sd(.x, na.rm = TRUE):%.2f}°")) %>%
        write_fixed(report_out, frmt = vec_repeat("%30s", ncol(.)))
}