TestAavso <- function(dir = fs::path("Input", "Temp", "Raw")) {
    files <- fs::dir_ls(dir, regexp = "csv$")

    data <- files %>%
        map(read_csv, col_types = cols(
            "d", "d", "d", "d", "c", "c", "c", "c",
            "c", "c", "c", "i", "d", "c", "d", "d",
            "d", "c", "c", "c", "c", "c", "c", "c"
        )) %>%
        bind_rows %>%
        select(JD, Magnitude, Uncertainty, Band) %>%
        mutate(Band = as_factor(Band)) %>%
        rename(Mag = Magnitude, Err = Uncertainty) %>%
        group_by(Band) %>%
        nest %>%
        filter(map_lgl(data, ~ nrow(.x) > 1e4)) %>%
        mutate(data = map(data, ~ mutate(.x, MJD = JD - 2400000.5))) %>%
        mutate(data = map(data,
            ~ slice(.x, seq(1, nrow(.x),
                by = floor((nrow(.x) - 1) / 2e3))))) %>%
        unnest(data)

    #58195 - 58223
    #58224 - 58235
    #58316 - 58329
    #58413 - 58416
    selector <- quos(
        MJD >= 58195 & MJD <= 58221 ~ "hard-1",
        MJD >= 58222 & MJD <= 58235 ~ "trans",
        MJD >= 58316 & MJD <= 58329 ~ "soft",
        MJD >= 58413 & MJD <= 58416 ~ "hard-2",
        TRUE ~ "unobs")

    data %<>%
        mutate(Type = factor(case_when(!!!selector),
            c("hard-1", "hard-2", "soft", "trans", "unobs")))

    p <- data %>%
        ggplot(aes(
            x = MJD,
            y = Mag,
            ymin = Mag - Err,
            ymax = Mag + Err,
            color = Type,
            shape = Type
        )) +
        geom_point(size = 2L) + geom_linerange(size = 0.5) +
        scale_y_reverse() +
        facet_wrap(~Band, ncol = 1) +
        DefaultTheme() +
        scale_shape_manual(values = c(15, 16, 18, 17, 4)) +
        scale_color_manual(
            values = c(scales::hue_pal()(4), "#000000"),
            limits = c("hard-1", "hard-2", "soft", "trans", "unobs")) +
        xlim(c(58184.56, 58427.26))

    p
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    dir <- fs::path("Output", "TestColors") %T>% (fs::dir_create)
    pdf(fs::path(dir, "test_aavso.pdf"),
        width = 7, height = 5.33, onefile = TRUE)
    tryCatch(print(TestAavso()), finally = dev.off())
}