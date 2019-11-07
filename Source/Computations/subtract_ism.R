subtract_ism <- function(data, ism) {

    ism %<>%
        transmute(Px, Py, Band = fct_get(Band))

    joined_data <- left_join(data %>% mutate(Band = fct_get(Band)),
                             ism, by = "Band", suffix = cc("", "_ism"))
    joined_data %>%
        mutate(Px = Px - Px_ism,
               Py = Py - Py_ism) %>%
        select(- Px_ism, - Py_ism) %>%
        mutate(Band = as_factor(Band)) %>%
        mutate(P = sqrt(Px ^ 2 + Py ^ 2),
               A = 90 * atan2(Py, Px) / pi,
               SG_A = 90 / pi * atan2(SG, P))
}