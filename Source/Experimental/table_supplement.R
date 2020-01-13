mjd_2_date <- function(dates) {
    # 0h Nov 17, 1858
    require(lubridate)

    mjd_origin <- ymd_hms("1858-11-17T00:00:00")


    format(mjd_origin + ddays(dates), "%Y-%m-%dT%H:%M:%S")
}

generate_data_for_csv <- function(data = ReadAllAvgData(pattern = "pol_avg_(?<id>[0-9]+)_(?<band>\\w)")) {
    data %>%
        list_collapse_dfr(to = Band) %>%
        transmute(
            MJD, Filter = fct_relevel(Band, "B", "V", "R"),
            Phase = fct_recode(Group, `RH` = "0", `HIM` = "1", `Soft` = "2", `DH` = "3"),
            Q = Px, U = Py, P, PErr = SG, A, AErr = SG_A,
            Date = mjd_2_date(MJD)) %>%
        arrange(Filter, MJD) %>%
        mutate_at(vars(Q, U, P, PErr), round, 3) %>%
        mutate_at(vars(A, AErr), round, 2) %>%
        mutate(MJD = round(MJD, 6))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    path <- fs::path("Output", "supplementary.csv")
    generate_data_for_csv() %T>%
        print(n = len(.)) %>%
        write_csv(path)
}