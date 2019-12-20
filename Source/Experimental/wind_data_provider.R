wind_data <- function() {
    require(lubridate)

    # 0h Nov 17, 1858 
    mjd_zero <- ymd_hms("1858-11-17 00:00:00")

    tribble(~Time, ~ Telescope, ~ State, ~ He_I, ~ Ha,
            "2018-03-15 14:46", "Keck", "Hard", NA,     "PCyg",
            "2018-03-16 14:46", "VLT",  "Hard", "PCyg", "PCyg",
            "2018-03-17 05:26", "GTC",  "Hard", "PCyg", "PCyg",
            "2018-03-18 06:09", "GTC",  "Hard", "BW",   "BW",
            "2018-03-20 05:58", "GTC",  "Hard", "BW",   "BW",
            "2018-03-20 08:07", "VLT",  "Hard", "BW",   "BW",
            "2018-03-21 06:11", "GTC",  "Hard", "PCyg", "BW|PCyg",
            "2018-03-22 05:38", "GTC",  "Hard", NA,     "BW",
            "2018-03-22 07:53", "VLT",  "Hard", NA,     "BW",
            "2018-03-24 05:38", "GTC",  "Hard", NA,     "BW",
            "2018-03-26 04:29", "GTC",  "Hard", "PCyg", "BW",
            "2018-04-23 02:26", "SALT", "Hard", "PCyg", "BW|PCyg",
            "2018-05-13 02:03", "SALT", "Hard", NA,     NA,
            "2018-05-14 01:04", "SALT", "Hard", NA,     NA,
            "2018-05-17 00:53", "SALT", "Hard", NA,     NA,
            "2018-06-17 21:26", "TNG",  "Hard", NA,     NA,
            "2018-06-18 04:55", "TNG",  "Hard", NA,     NA,
            "2018-06-18 21:59", "TNG",  "Hard", NA,     NA,
            "2018-07-08 02:05", "GTC",  "Soft", NA,     NA,
            "2018-07-10 21:33", "GTC",  "Soft", NA,     NA,
            "2018-07-11 21:26", "GTC",  "Soft", NA,     NA,
            "2018-07-13 01:29", "GTC",  "Soft", NA,     NA,
            "2018-07-15 01:28", "GTC",  "Soft", NA,     NA,
            "2018-07-18 01:42", "GTC",  "Soft", NA,     NA,
            "2018-07-18 21:42", "GTC",  "Soft", NA,     NA,
            "2018-07-24 22:49", "GTC",  "Soft", NA,     NA,
            "2018-07-27 21:29", "GTC",  "Soft", NA,     NA,
            "2018-08-03 23:20", "GTC",  "Soft", NA,     NA,
            "2018-08-09 22:18", "GTC",  "Soft", NA,     NA,
            "2018-08-15 21:41", "GTC",  "Soft", NA,     NA,
            "2018-08-19 21:56", "GTC",  "Soft", NA,     NA,
            "2018-09-07 00:58", "VLT",  "Soft", NA,     NA,
            "2018-09-28 23:57", "VLT",  "Hard", "PCyg", "BW",
            "2018-09-29 23:51", "VLT",  "Hard", "PCyg", NA,
            "2018-10-12 21:35", "GTC",  "Hard", NA,     NA,
            "2018-10-21 21:19", "GTC",  "Hard", NA,     NA,
            "2018-11-04 19:45", "GTC",  "Hard", NA,     NA,
    ) %>%
        mutate(
            Time = ymd_hm(Time),
            MJD = as.duration(Time - mjd_zero) / as.duration(days(1))) %>% 
        mutate_at(vars(Telescope, State, He_I, Ha), as_factor)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    wind_data() %>% print
}