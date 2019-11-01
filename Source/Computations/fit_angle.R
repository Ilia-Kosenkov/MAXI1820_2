prepare_data <- function() {
    dt <-
        ReadAllAvgData(pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)") %>%
        bind_rows %>%
        mutate(Band = as_factor(Band))

    field <-
        AverageFieldStars() %>%
        imap(~mutate(.x, Band = .y)) %>%
        bind_rows %>%
        mutate(Band = as_factor(Band))

    subtract_ism(dt, field)
}

fit_data <- function(data) {
    reticulate::use_condaenv("r-tensorflow-gpu", required = TRUE)
    Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
    require(greta)
    n <- data %>% pull(Band) %>% unique %>% len

    y0 <- greta::uniform(-0.5, 0.5, n)
    angle <- greta::uniform(0, pi / 2, n)

    data %>%
        group_split(Band) %>%
        imap(~mutate(.x, Id = .y)) -> split_data

    for (i in seq_len(n)) {
        x <- pull(split_data[[i]], Px)
        y <- pull(split_data[[i]], Py)
        err <- pull(split_data[[i]], SG)

        greta::distribution(y) <- greta::normal(y0[i] + tan(angle[i]) * x, err)
    }

    mdl <- greta::model(y0, angle)

    samples <<- greta::mcmc(mdl, n_samples = 1e4)



}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    data <- prepare_data()
    data %>%
        filter(vec_in(Group, cc("0", "1", "2"))) %>%
        select(Px, Py, SG, Band) -> pre_fit_data

    pre_fit_data %>% fit_data
}