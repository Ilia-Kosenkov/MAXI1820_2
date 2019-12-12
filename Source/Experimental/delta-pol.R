test_delta_plot <- function() {
    require(sciplotr)
    avg_data <- ReadAllAvgData(pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")

    sub_id <- 3

    avg_data %>%
        map(select, JD, Px, Py, SG, Group, Band) %>%
        map(arrange, Group) %>%
        map(
            mutate,
            Px = Px - Px[sub_id],
            Py = Py - Py[sub_id],
            SG0 = SG, SG = sqrt(SG ^ 2 + SG[sub_id] ^ 2)) %>%
    #map(slice, - sub_id) %>%
        map(slice, 1:2) %>%
        map(mutate, Band = as_factor(Band)) %>%
        vec_rbind_uq %>%
        CalculateMinMax(Px, SG0) %>%
        CalculateMinMax(Py, SG0) -> subtr_data

    subtr_data %>%
        inner_join_safe(select(Bands, Band, ID), by = "Band") %>%
        mutate(ID = as_factor(ID)) -> subtr_data


    p <- PlotQU(subtr_data, Px, Py, group = ID, plotGrid = TRUE, isTex = TRUE)

    #path <- fs::path("Output", "Plots", glue_fmt("qu_statediff_{sub_id}.tex"))
    path <- fs::path("Output", "Plots", glue_fmt("qu_statediff_{sub_id}_only_1,2.tex"))

    tikz(path, width = 6.5, height = 6.5, standAlone = TRUE)
    tryCatch({ print(p) }, finally = dev.off())
    tex_2_pdf(path)

}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    test_delta_plot() %>% print
}