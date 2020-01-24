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
            Q = Px, U = Py, P, e_P = SG, A, e_A = SG_A,
            Date = mjd_2_date(MJD)) %>%
        arrange(Filter, MJD) %>%
        mutate_at(vars(Q, U, P, e_P), round, 3) %>%
        mutate_at(vars(A, e_A), round, 2) %>%
        mutate(MJD = round(MJD, 6))
}

print_as_fixed_width <- function(data, path = fs::path("Output", "supplementary.dat")) {
    write_smart(data, path,
                frmt = cc("%14.6f", "%8s", "%6s",
                    vec_repeat("%8.3f", 4L),
                    "%9.2f", "%8.2f",
                    "%22s"))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    path <- fs::path("Output", "supplementary.csv")
    generate_data_for_csv() %T>%
        print(n = len(.)) %>%
        print_as_fixed_width
        #write_csv(path)
}