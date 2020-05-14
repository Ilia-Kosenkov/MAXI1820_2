if (get0("ShouldRun", ifnotfound = FALSE)) {
    dt <- ReadAllAvgData(
            pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[vec_c("B", "V", "R")]
    field <- AverageFieldStars()[vec_c("B", "V", "R")]

    data <- dt %>%
        SubtractISM(field, .propagate_errors = FALSE) %>%
        vmap_pt(transmute, JD, Px, Py, SG, Group, Filter = Band) %>%
        mutate(Filter = as_factor(Filter)) %>%
        select(-JD) %>%
        filter(Group %vin% c("1", "2")) %>%
        pivot_wider(names_from = Group, values_from = c("Px", "Py", "SG")) %>%
        mutate(
            Px = Px_1 - Px_2,
            Py = Py_1 - Py_2,
            SG = sqrt(SG_1 ^ 2 + SG_2 ^ 2),
            P = sqrt(Px ^ 2 + Py ^ 2),
            A = 90 / pi * atan2(Py, Px),
            SG_A = 90 / pi * SG / P)

    avg <- data %>%
        summarise(A = mean(A), SG_A = sqrt((SG_A %.% SG_A) / 3))

    print(data)
    print(avg)
}