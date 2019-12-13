
## Move to RLibs
`%.%` <- function(e1, e2) {
    result <- vec_recycle_common(!!!vec_cast_common(e1, e2))
    return(sum(result[[1]] * result[[2]]))
}

rebin_aavso <- function(data, by = 1) {
    data %>%
        filter(!is.na(Err)) %>%
        mutate(
            Flux = 10 ^ (Mag / -2.5),
            FluxErr = Flux * log(10) / 2.5 * Err,
            W = 1 / FluxErr ^ 2,
            Id = vec_cast_integerish(floor((MJD - by * floor(MJD[1] / by) + 0.5 * by) / by))) %>%
        group_by(Filter, Id) %>%
        summarise(
            JD = mean(JD),
            MJD = mean(MJD),
            nFlux = (W %.% Flux) / sum(W),
            nFluxErr = sqrt(W ^ 2 %.% FluxErr ^ 2) / sum(W)) %>%
        mutate(Mag = -2.5 * log10(nFlux), Err = 2.5 / log(10) * nFluxErr / nFlux) %>%
        select(-nFlux, - nFluxErr) %>%
        ungroup
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    aavso_data %>%
        rebin_aavso %>%
    print

}