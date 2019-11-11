# MIT License
#
# Copyright(c) 2018 Ilia Kosenkov[ilia.kosenkov.at.gm@gmail.com]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files(the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

PlotAsSpectra <- function(data,
                          x, y, ymin, ymax,
                          group = Group,
                          xlab = quo_text(enquo(x)),
                          ylab = quo_text(enquo(y))) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    group <- enquo(group)

    data <- data %>% arrange(Group)
    ynms <- GetMinMaxNames(!!y)

    if (quo_is_missing(ymin))
        ymin <- ynms$min
    if (quo_is_missing(ymax))
        ymax <- ynms$max

    p <- ggplot(
            data,
            aes(x = !!x, y = !!y,
                ymin = !!ymin, ymax = !!ymax,
                group = !!group,
                shape = !!group,
                col = !!group,
                fill = !!group,
                alpha = !!group,
                linetype = !!group)) +
        DefaultTheme(textSz = Style_TickFontSz, titleSz = Style_LabelFontSz) +
        scale_color_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Colors,
            guide = FALSE) +
        scale_fill_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Colors,
            guide = FALSE) +
        scale_shape_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Shapes,
            guide = FALSE) +
        scale_alpha_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_Alphas,
            guide = FALSE) +
        scale_linetype_manual(
            limits = Style_GroupsCombined,
            values = Style_GC_LineTypes2,
            guide = FALSE)


    p <- p +
        geom_point(size = Style_SymbolSize) +
        geom_line(size = Style_LineSize) +
        geom_errorbar(size = Style_LineSize, width = 0)

    xrng <- data %>% pull(!!x) %>%
        range %>% Expand(factor = 0.12, direction = c(1, 1.75))

    yrng <- GetRange(data, col_min = !!ymin, col_max = !!ymax) %>%
        Expand(factor = 0.06, direction = c(2, 2))


    bands <- data %>% pull(Band) %>% unique

    data2 <- data %>%
        filter(Band == last(bands)) %>%
        select(!!x, !!y) %>%
        mutate(Id = row_number())

    wl <- bands %>%
        map_dbl(~data %>% filter(Band == .x) %>% slice(1) %>% pull(WL))

    p <- p +
        coord_cartesian(
            xlim = xrng, ylim = yrng, expand = FALSE, clip = "off") +
        xlab(xlab) +
        ylab(ylab) +
        GGCustomTextAnnotation(bands, wl, - Inf, vjust = -0.3, hjust = 0.6,
            gp = gpar(fontsize = Style_LabelFontSz)) +
        geom_text(aes(x = !!x, y = !!y, label = Id),
                  data2,
                  inherit.aes = FALSE,
                  position = position_nudge(x = 0.05 * diff(range(xrng))),
                  size = 7)


    p %>%
        LinearScaleTicks(
            rng = xrng, side = "x",
            gp = gpar(fontsize = Style_TickFontSz)) %>%
        LinearScaleTicks(
            rng = yrng, side = "y",
            gp = gpar(fontsize = Style_TickFontSz))



}

prepare_spectra_data <- function(
    avg_data = ReadAllAvgData(pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)"),
    field_data = field_stars %>% map(filter, Star %vec_in% cc(2, 3, 6, 7, 9)),
    band_info = Bands) {

    avg_data %>%
        vec_rbind_uq(.names_to = "Band") %>%
        transmute(MJD, Px, Py, P, SG, A, SG_A, Band = as_factor(Band), Group) -> pol

    field_data %>%
        imap(~mutate(.x, Band = as_factor(.y))) %>%
        AverageFieldStars() %>%
        vec_rbind_uq %>% 
        select(JD, MJD, Px, Py, P, SG, A, SG_A, Band, Star) -> field

    subtr <- subtract_ism(pol, field) %>%
        mutate(Band = fct_get(Band)) %>%
        left_join(band_info %>% select(WL, Band), by = "Band") %>%
        mutate(Band = factor(Band, levels = cc("B", "V", "R"))) %>%
        arrange(Band)
}

plot_as_spectra <- function(data) {
    require(sciplotr)
    col_pal <- cc(Style_GroupColors[cc(5, 8, 6, 7)], "#000000")
    shape_pal <- cc(Style_GroupShapes[cc(5, 8, 6, 7)], 0)
    data %>%
        transmute(WL, Band, Group, Obs_P = P, Err_P = SG, Obs_A = A, Err_A = SG_A) %>%
        pivot_longer(
            cols = c(-WL, - Band, - Group),
            names_to = c(".value", "Var"), names_pattern = "^(.*)_(.*)$") %>%
        mutate(Var = as_factor(Var) %>%
            fct_recode("$P_{BVR}$~(\\%)" := "P", "$\\theta_{BVR}$~(deg)" := "A")) -> data

    rng <- range(data$WL)
    offset <- cc(0, 50, 50, 0)
    data %>%
        filter(Band == "R") %>%
        mutate(
            Label = as.character(parse_integer(fct_get(Group)) + 1L),
            WL = lin(vec_repeat(1.04, n()), cc(0, 1), rng)) %>%
        select(WL, Label, Obs, Var) %>%
        mutate(WL = if_else(
                Var == fct_get(Var)[2],
                WL + offset[parse_integer(Label)],
                WL)) -> label_data

    rng <- data %>%
        filter(Var == fct_get(Var)[2]) %>%
        transmute(Low = Obs - Err, High = Obs + Err) %>%
        range

    data %>%
        filter(Var == fct_get(Var)[2], Group == 0) %>%
        mutate(
            Obs = lin(vec_repeat(-0.05, n()), cc(0, 1), rng),
            Label = fct_get(Band)) %>%
        select(WL, Label, Obs, Var) %>%
        vec_rbind(label_data) -> label_data


    data %>%
        ggplot_sci(
            aes(
                WL, Obs, ymin = Obs - Err, ymax = Obs + Err,
                group = Group, col = Group, shape = Group, fill = Group)) +
        theme_sci(
                ticks = -u_(5 ~ pt),
                text.size = Style_TickFontSz,
                title.size = Style_LabelFontSz,
                facet.lab.x = npc_(0.07),
                facet.lab.y = npc_(0.9)) +
        geom_line() +
        geom_errorbar(width = 0, size = 0.7) +
        geom_point(size = 2) +
        geom_text(aes(WL, Obs, label = Label),
            size = convertY(pt_(0.8 * Style_TickFontSz), "mm", TRUE),
            label_data, inherit.aes = FALSE) +
        scale_color_manual(values = col_pal, breaks = 0:3, guide = NULL) +
        scale_fill_manual(values = col_pal, breaks = 0:3, guide = NULL) +
        scale_shape_manual(values = shape_pal, breaks = 0:3, guide = NULL) +
        scale_x_sci(
            name = "$\\lambda$~(\\AA)", sec.axis = dup_axis_sci_weak(),
            expand = expansion(0.05, 0)) +
        scale_y_sci(name = NULL, sec.axis = dup_axis_sci_weak(),
            expand = expansion(0.12, 0)) +
        facet_sci(vars(Var), scales = "free_y", panel.labeller = ~RLibs::glue_fmt("({letters[.x$Id]})")) -> plt

    plt %>%
        postprocess_axes(
            axes_margin = mar_(0.25 ~ cm, 0.25 ~ cm, 0.5 ~ cm, 0.75 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1.1 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 1.1 ~ cm, 0 ~ npc)) -> tbl

    grid.newpage()
    grid.draw(tbl)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    tic()
    file_path <- fs::path("Output", "Plots", "pol_spec.tex")
    tikz(file_path, width = Style_WidthStdInch, height = Style_HeightStdInch, standAlone = TRUE)
    tryCatch({
             prepare_spectra_data() %>% plot_as_spectra
        },
        finally = dev.off())
    tex_2_pdf(file_path)
    toc()
    #bndOrder <- Bands %>% pull(Band)
    #avgData <- ReadAllAvgData(
    #pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)") %>%
    #bind_rows %>%
    #inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
    #mutate(Group = as_numeric(Group) + 1L)

        #stars <- c(2, 3, 6, 7, 9)
    #fData <- field_stars %>%
    #map2(names(field_stars), ~ mutate(.x, Band = .y)) %>%
    #map(filter, Star %in% stars) %>%
    #bind_rows %>%
    #inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
    #mutate(Group = -Star) %>%
    #mutate(Star = as.factor(Star))


        #avgFData <- AverageFieldStars() %>%
    #map2(names(.), ~ mutate(.x, Band = .y)) %>%
    #bind_rows %>%
    #inner_join(select(Bands, Band, ID, WL, Freq), by = "Band") %>%
    #mutate(Group = 0L)

        ##data <- bind_rows(avgData, fData, avgFData) %>%
    ##mutate(Group = as.factor(Group))

        #data2 <- ReadAllAvgData(
    #pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder]
    #field <- AverageFieldStars()[bndOrder]

        #data <- data2 %>% SubtractISM(field, .propagate_errors = FALSE) %>%
    #bind_rows %>%
    #inner_join(select(Bands, Band, ID, Freq, WL), by = "Band") %>%
    #mutate(Group =
    #forcats::fct_relabel(Group, ~as.character(as.numeric(.) + 1L)))

        ##stop()
    #cols <- c("P", "A")
    #labs <- c("$P_{BVR}$ (\\%)", "$\\theta_{BVR}$ (deg)")

        #drPath <- fs::path("Output", "Plots") %T>% (fs::dir_create)


        #future_pmap(list(cols, labs, seq_along(cols)),
    #function(x, y, i) {

        #fPath <- file.path(drPath, glue("spectra_subtr_{x}.tex"))

        #tikz(fPath,
    #width = Style_WidthWideInch,
    #height = Style_HeightStdInch,
    #standAlone = TRUE)

        #tryCatch({
    #PlotAsSpectra(data,
    #WL, !!sym(x),
    #group = Group,
    #xlab = "$\\lambda$ (\\AA)",
    #ylab = y) %>%
    #GGPlotPanelLabs(
    #letters[i], hjust = 5, vjust = 2,
    #gp = gpar(fontsize = Style_LabelFontSz)) %>%
    #GGPlot2GrobEx %>%
    #GrobMarginSet(
    #labsMar = Style_LabsMarStd,
    #axisMar = Style_AxisMarStd) %>%
    #GrobPlot
    #}, finally = dev.off())

        #Tex2Pdf(fPath, verbose = TRUE)
    #}, .progress = TRUE)
}