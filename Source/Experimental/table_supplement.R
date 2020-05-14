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
            Q = Px, U = Py, PD = P, e_PD = SG, PA = A, e_PA = SG_A) %>%
        arrange(Filter, MJD) %>%
        mutate_at(vars(Q, U, PD, e_PD), round, 3) %>%
        mutate_at(vars(PA, e_PA), round, 2) %>%
        mutate(MJD = round(MJD, 6)) %>%
        mutate(JD = MJD + 2400000.5, .before = 1) %>%
        select(-MJD)
}

print_as_fixed_width <- function(data, path = fs::path("Output", "supplementary.dat")) {
    write_smart(data, path,
                frmt = cc("%14.6f", "%8s", "%6s",
                    vec_repeat("%8.3f", 4L),
                    "%9.2f", "%8.2f",
                    "%22s"))
}

build_cds_files <- function(data) {
    require(rastrocat)
    gen <- ReadMeGen$new(
                         "J/MNRAS/??/??",
                         "Polarization of LMXB MAXI J1820+070",
                         cc("Kosenkov I.A.", "Veledina A.", "Berdyugin A.V.",
                         "Kravtsov V.", "Piirola V.", "Berdyugina S.V.",
                         "Sakanoi T.", "Kagitani M.", "Poutanen J."), 2020L)

    gen$FullTitle <- "Polarisation of low-mass X-ray binary MAXI J1820+070 during its 2018 outburst."
    gen$Description <- "Nightly average observed polarisation degrees (PD) and polarisation angles (PA), " %&%
        " obtained during the 2018 outburst of MAXI J1820+070. " %&%
        "Polarimetric measurements were performed using Dipol-2 polarimeter (Piirola et al. 2014) mounted on" %&%
        "the Tohoku 60 cm telescope (T60) at Haleakala observatory, Hawaii. " %&%
        "Dipol - 2 is a remotely operated  \"double - image\" CCD polarimeter, " %&%
        "which is capable of recording images in three(BVR) filters simultaneously. " %&%
        "The innovative design of the polarimeter, where the two orthogonally polarised images of the sky " %&%
        "overlap on the images of the source, allows to completely eliminate the sky polarisation at an instrumental stage " %&%
        "(even if it is variable), and to achieve unprecedentedly high, up to 10 ^ (-5), accuracy of target " %&%
        "polarimetric measurements (Piirola 1973, Berdyugin2019)."
    gen$Abstract <-
        "We describe the first complete polarimetric dataset of the entire outburst of a low-mass black hole X-ray binary system and discuss the constraints for geometry and radiative mechanisms it imposes. " %&%
            "During the decaying hard state, when the optical flux is dominated by the non-thermal component, the observed polarisation is consistent with the interstellar values in all filters. " %&%
            "During the soft state, the intrinsic polarisation of the source is small, ~ 0.15% in B and V, and is likely produced in the irradiated disc. " %&%
            "A much higher, ~ 0.5%, polarisation detected in the rising hard state and hard-intermediate state coincides in time with the detection of winds in the system. " %&%
            "Its polarisation angle is ~25 degrees. " %&%
            "We argue that this polarisation is produced by scattering of the non-thermal (hot flow) optical radiation in the optically thin outflow. " %&%
            "If scattering occurs in relativistic outflow (jet), the polarisation angle should coincide with the jet position angle, and for scattering in a non-relativistic outflow (wind) the polarisation angle should be offset by 90 degrees from the jet direction."

    #gen$set_bibcode_references(
        #cc("1973A&A....27..383P",
            #"2014SPIE.9147E..8IP",
            #"2019ASSL..460...33B"))

    gen$set_keywords("polarisation", "stars:black holes", "stars:individual:MAXI J1820+070", "X-rays:binaries")
    gen$set_adc_keywords("Polarization", "Binaries, X-ray", "Accretion", "X-ray sources")


    frmt <- tibble(
            Format = cc("F13.4", "A3", "A5", vec_repeat("F6.3", 4L), vec_repeat("F6.2", 2L)),
            Label = names(data),
            Units = cc("d", NA, NA, "%", "%", "%", "%", "deg", "deg"),
            Explanations = cc("Julian date", "[BVR] Optical filter", "Outburst phase", "Stokes Q", "Stokes U",
                "Polarisation degree", "Polarisation degree error", "Polarisation angle", "Polaristaion angle error"))
    gen$assign_multiple_datasets(
        frmt,
        tibble(Data = list(data), FileName = "maxij1820_pol.dat", Description = "Polarization of MAXIJ1820"))

    gen$set_remarks("Author's e-mail address" = c("ilia.kosenkov@utu.fi"))

    gen
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    path <- fs::path("Output", "supplementary.csv")
    generate_data_for_csv() -> data

    build_cds_files(data) -> gen
    gen$generate_readme() %>% write_lines(fs::path("Output", "ReadMe.txt"))

    gen$generate_data()[[1]] %>% write_lines(fs::path("Output", "maxij1820_pol.dat"))

    #  print_as_fixed_width
        #write_csv(path)
}