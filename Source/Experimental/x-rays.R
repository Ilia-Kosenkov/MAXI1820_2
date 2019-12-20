
read_x_rays <- function(
    path = fs::path("Input", "glcbin24.0h_regbg_v1.csv"),
    path_bat = fs::path("Input", "MAXIJ1820p070.lc.txt")) {
    levels <- cc("2-20" = "1", "2-4" = "2", "4-10" = "3", "10-20" = "4")
    path %>%
        read_csv(col_names = FALSE, col_types = cols()) %>%
        set_names(cc("MJD", "MJD_start", "MJD_end",
        reduce(map(1:4, ~ paste0(cc("C", "dC"), .x)), cc), "Unused")) %>%
        select(-Unused) %>% {
            reduce(1:4,
                ~ mutate(.x,
                    !!paste0("C_", .y) :=
                        map2(!!sym(paste0("C", .y)), !!sym(paste0("dC", .y)), cc)) %>%
                    select(-one_of(paste0(cc("C", "dC"), .y))),
                .init = .)
        } %>%
        pivot_longer(starts_with("C"), names_to = "BandId", names_prefix = "C_") %>%
        mutate(BandId = as_factor(BandId) %>%
               fct_recode(!!!levels) %>%
               fct_get) %>%
        mutate(Data = map_dbl(value, 1L), Err = map_dbl(value, 2L)) %>%
        select(-value, -MJD_start, -MJD_end)  -> maxi_data

    path_bat %>%
        read_table(col_names = FALSE, col_types = cols(), comment = "#") %>%
        set_names(cc("MJD", "Data", "Err", "Year", "Day", "StatErr", "SysErr", "Flag", "Expo", "Coded", "Dithread")) %>%
        select("MJD", "Data", "Err") %>%
        mutate(BandId = "15-50") -> bat_data

    bind_rows(maxi_data, bat_data) %>%
        mutate(BandId = as_factor(BandId) %>%
            fct_relevel("2-4", "4-10", "10-20", "2-20", "15-50")) %>%
        filter_range(MJD, cc(58190, 58450))

}

transform_x_ray <- function(data, extra_band = cc("2-4", "15-50")) {
    lower <- filter(data, BandId == "2-4")
    upper <- filter(data, BandId == "10-20")
    bind_cols(select(upper, - BandId), transmute(lower, lData = Data, lErr = Err)) %>%
        mutate(Err = abs(Data / lData) * sqrt(Err ^ 2 / Data ^ 2 + lErr ^ 2 / lData ^ 2),
               Data = Data / lData,
               BandId = as_factor("10-20 / 2-4")) %>%
        select(-starts_with("l")) %>%
        filter(Err < 1) -> hardness_ratio

    data %>%
        filter(BandId %vec_in% extra_band) %>%
        bind_rows(hardness_ratio)

        #bind_rows(
            #mutate(upper, BandId = fct_get(BandId)),
            #mutate(lower, BandId = fct_get(BandId))) %>%
        #mutate(BandId = as_factor(BandId)) -> result

    #left_join_condition(result, dates, .x$MJD >= .y$Lower, .x$MJD <= .y$Upper) %>%
        #select(-Lower, - Upper) %>%
        #mutate(Group = fct_recode(fct_explicit_na(Group, "No data"), !!!levels))
}

plot_x_ray <- function(
    data,
    dates_input = dates_range(),
    aavso = aavso_data,
    wind = wind_data()) {

    dates <- dates_input %>%
        pivot_longer(c(-Group)) %>%
        vec_repeat(each = 2) %>%
        transmute(Group, x = value,
                  y = vec_repeat(cc(0, Inf, Inf, 0), times = n() / 4),
                  y_hr = vec_repeat(cc(-Inf, Inf, Inf, - Inf), times = n() / 4))
    ### Very-very-very experimental
    require(sciplotr)
    rng <- data %>% pull(MJD) %>% range
    col_pal <- cc(Style_GroupColors[cc(5, 8, 6, 7)], "#000000")

    labels_data <- dates_input %>%
        transmute(
                  Label = fct_get(Group),
                  X = vec_cast(lin_unit(0.5 * (Lower + Upper), rng %>% expand_range(0.05), u_(0$npc, 1$npc)), list()),
                  Y = list(u_(0.5 ~ cm)))

    text_grob <- textGrob(
        labels_data$Label,
        grid:::unit.list.from.list(labels_data$X),
        grid:::unit.list.from.list(labels_data$Y),
        gp = gpar(fontsize = Style_TickFontSz))


    aavso_reb <- aavso %>% filter_range(MJD, rng) %>% rebin_aavso

    maxi_hr_name <- "hr"

    levels <- cc("2-4", "15-50", "10-20 / 2-4") %>%
            set_names(cc("MAXI 2$-$4 keV","BAT 15$-$50 keV", maxi_hr_name))

    #xr_wind <-
        #right_join_cnd(filter(data, BandId %==% "15-50"), wind, abs(.x$MJD - .y$MJD) < 0.5) %>%
        #transmute(
            #MJD = MJD__r,
            #MJD_end = MJD__r,
            #Pos = 10 ^ (log10(Data - Err) - 0.5),
            #Pos_inb = 10 ^ (log10(Data - Err) - 0.55),
            #Pos_end = 10 ^ (log10(Data - Err) - 2),
            #He_I, Ha,
            #HasWind)

    onir_wind <-
        right_join_cnd(
            filter(aavso_reb, Filter %vec_in% cc("CV", "V")),
            wind,
            abs(.x$MJD - .y$MJD) < 0.49) %>%
        transmute(
            MJD = MJD__r,
            MJD_end = MJD__r,
            Off = Mag + Err,
            He_I, Ha,
            HasWind) %>%
        mutate(Off = if_else(is.nan(Off), 0.5 * (lag(Off) + lead(Off)), Off)) %>%
        slice(unique_which_f(MJD)) %>%
        mutate(Pos = Off + 0.65, Pos_inb = Off + 0.7, Pos_end = Off + 1.5)
    
    data %<>% mutate(BandId = fct_recode(BandId, !!!levels))

    plt_1 <- data %>%
        filter(BandId != maxi_hr_name) %>%
        mutate(BandId = fct_drop(BandId)) %>%
        filter(Data - Err > 1e-4) %>%
        ggplot(aes(
                x = MJD, y = Data,
                ymin = Data - Err, ymax = Data + Err,
                shape = BandId)) +
        coord_sci(xlim = rng, clip = "off") +
        theme_sci(
            ticks = -u_(5 ~ pt),
            text.size = Style_TickFontSz,
            title.size = Style_TickFontSz,
            legend.key = element_blank(),
            legend.position = cc(1, 1),
            legend.justification = cc(1, 0.85),
            legend.text = element_text(size = 0.65 * Style_TickFontSz)) +
        geom_polygon(
            aes(x, y, fill = Group),
            alpha = 0.4,
            show.legend = FALSE,
            data = dates, inherit.aes = FALSE) +
        geom_pointrange(size = 0.3) +
        #geom_segment(
            #aes(x = MJD, xend = MJD_end, y = Pos_inb, yend = Pos_end, linetype = HasWind),
            #data = xr_wind,
            #size = 1, lineend = "butt", linejoin = "mitre",
            #inherit.aes = FALSE) +
        #geom_segment(
            #aes(x = MJD, xend = MJD_end, y = Pos, yend = Pos_inb),
            #data = xr_wind,
            #size = 1, lineend = "butt", linejoin = "mitre",
            #arrow = arrow(length = u_(5 ~ pt), ends = "first"),
            #inherit.aes = FALSE) +
        scale_fill_manual(values = col_pal) +
        scale_shape_manual(values = c(19, 1), guide = guide_legend(title = "")) +
        #scale_linetype_manual(values = c(1, 3), guide = NULL) +
        scale_x_sci(
            name = NULL, limits = rng,
            labels = function(x) rep(" ", len(x)),
            sec.axis = dup_axis_sci_weak()) +
        scale_y_log10_sci(
            name = "X-ray flux,\ncts~cm$^{-2}$~s$^{-1}$",
            sec.axis = dup_axis_sci_weak(),
            labels = function(x) as.character(RLibs::glue_fmt("{x:%g}"))) +
        annotation_custom(
            textGrob("(a)",
                x = npc_(0.03), y = npc_(0.88),
                gp = gpar(fontsize = Style_LabelFontSz)))

    plt_2 <- #aavso_data %>%
    #filter_range(MJD, rng) %>%
        #rebin_aavso %>%
        aavso_reb %>% 
        filter(Filter %vec_in% cc("CV", "V")) %>%
        mutate(Filter = fct_recode(Filter, `Johnson V` = "V", `Wide band V` = "CV")) %>%
        ggplot_sci(aes(x = MJD, y = Mag,
            ymin = Mag - Err, ymax = Mag + Err,
            shape = Filter)) +
        theme_sci(
            ticks = -u_(5 ~ pt),
            text.size = Style_TickFontSz,
            title.size = Style_TickFontSz,
            legend.key = element_blank(),
            legend.position = cc(1, 1),
            legend.justification = cc(1, 0.85),
            legend.text = element_text(size = 0.65 * Style_TickFontSz)) +
        geom_polygon(
            aes(x, y_hr, fill = Group),
            alpha = 0.4,
            show.legend = FALSE,
            data = dates, inherit.aes = FALSE) +
        geom_segment(
            aes(x = MJD, xend = MJD_end, y = Pos_inb, yend = Pos_end,
                linetype = HasWind, col = HasWind),
            data = onir_wind,
            size = 1, lineend = "butt", linejoin = "mitre",
            inherit.aes = FALSE) +
        geom_segment(
            aes(x = MJD, xend = MJD_end, y = Pos, yend = Pos_inb, col = HasWind),
            data = onir_wind,
            size = 1, lineend = "butt", linejoin = "mitre",
            arrow = arrow(length = u_(7 ~ pt), ends = "first"),
            inherit.aes = FALSE) +
        geom_pointrange(size = 0.3) +
        coord_sci(xlim = rng) +
        scale_x_sci(name = NULL, sec.axis = dup_axis_sci_weak()) +
        scale_y_rev_sci(name = "AAVSO V", sec.axis = dup_axis_sci_weak()) +
        scale_linetype_manual(values = c(1, 2), guide = NULL) +
        scale_color_manual(values = cc("#C00000", "#0F0F0F"), guide = NULL) +
        scale_fill_manual(values = col_pal) +
        scale_shape_manual(values = cc(1, 19), guide = guide_legend(title = "")) +
        annotation_custom(
            textGrob("(b)",
                x = npc_(0.03), y = npc_(0.88),
                gp = gpar(fontsize = Style_LabelFontSz)))

    data %>%
        filter(BandId == maxi_hr_name) %>%
        mutate(BandId = fct_drop(BandId)) -> hardness_ratio

    hardness_ratio %>%
        transmute(Lower = Data - Err, Upper = Data + Err) %>%
        pivot_longer(everything()) %>%
        summarise(Lower = quantile(value, 0.025), Upper = quantile(value, 0.975)) %>%
        flatten_dbl -> hr_rng

    hardness_ratio %<>%
        filter_range(Data, hr_rng) %>%
        mutate(Upp = Data + Err, Low = Data - Err) %>%
        clamp(Upp, hr_rng) %>%
        clamp(Low, hr_rng)

    hardness_ratio %>%
        ggplot(aes(
                x = MJD, y = Data,
                ymin = Low, ymax = Upp)) +
        coord_sci(xlim = rng) +
        theme_sci(
            ticks = -u_(5 ~ pt),
            legend.text = element_text(size = 0.75 * Style_TickFontSz),
            legend.title = element_text(size = Style_TickFontSz),
            text.size = Style_TickFontSz,
            title.size = Style_TickFontSz,
            legend.justification = cc(1.125, -0.05)) +
        geom_polygon(
                aes(x, y_hr, group = Group, fill = Group),
                alpha = 0.4,
                data = dates, inherit.aes = FALSE) +
        geom_pointrange(size = 0.2) +
        scale_fill_manual(values = col_pal, guide = guide_legend(title = "")) +
        scale_x_sci(sec.axis = dup_axis_sci_weak()) +
        scale_y_sci(name = "MAXI HR\n\\tiny{$\\frac{10-20~\\mathrm{keV}}{2-4~\\mathrm{keV}}$}", sec.axis = dup_axis_sci_weak()) +
        annotation_custom(
                textGrob("(c)",
                x = npc_(0.03), y = npc_(0.88),
                gp = gpar(fontsize = Style_LabelFontSz))) -> plt_3

    plt_1 %<>%
        postprocess_axes(
            axes_margin = mar_(0.75 ~ cm, 0.25 ~ cm, 0 ~ npc, 1.2 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1.8 ~ cm))

    pos <- get_grobs_layout(plt_1, "axis-t")[[1]]

    plt_1 <- gtable::gtable_add_grob(
            plt_1, text_grob, pos[3], pos[1],
            clip = "off",
            name = "top-labels")

    plt_2 %<>% postprocess_axes(
            axes_margin = mar_(0 ~ npc, 0.25 ~ cm, 0 ~ npc, 1.2 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1.8 ~ cm))

    plt_3 %<>% postprocess_axes(
            axes_margin = mar_(0 ~ npc, 0.25 ~ cm, 0.5 ~ cm, 1.2 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 0.7 ~ cm, 1.8 ~ cm))
    gridExtra::arrangeGrob(plt_1, plt_2, plt_3, ncol = 1,
        heights = u_(0.33 ~ null, 0.33 ~ null, 0.33 ~ null)) -> tbl

    grid.newpage()
    grid.draw(tbl)
}

plot_x_ray_hr <- function(data, dates = dates_range()) {
    require(sciplotr)
    col_pal <- cc(Style_GroupColors[cc(5, 8, 6, 7)], "#000000")


    data %>%
        filter(BandId %vec_in% cc("10-20 / 2-4", "2-20")) %>%
        pivot_wider(names_from = BandId, values_from = c(Data, Err)) -> temp

    temp %>% names %>% str_match("(?:(Data|Err)[^/]*?(/.*$|$))|.*") %>%
        as_tibble(.name_repair = "minimal") %>%
        set_names(cc("Source", "MesType", "DataType")) %>%
        mutate(
            DataType = case_when(is.na(DataType) ~ DataType, nzchar(DataType) ~ "HR", TRUE ~ "Cts"),
            MesType = if_else(MesType == "Err", "_err", "")) %>%
        mutate(Name = if_else(is.na(DataType) | is.na(MesType), Source, paste0(DataType, MesType))) %>%
        pull(Name) -> new_names


    temp %<>%
        set_names(new_names) %>%
        filter_range(HR, cc(-0.05, 0.4)) %>%
        filter_range(Cts, cc(0.1, Inf))


    left_join_condition(temp, dates, .x$MJD >= .y$Lower, .x$MJD < .y$Upper) %>%
        filter(!is.na(Group)) %>%
        group_by(Group) %>%
        group_map(~tibble(
                Group = pull(.y, Group),
                x = vec_repeat(quantile(cc(.x$HR - .x$HR_err, .x$HR + .x$HR_err), cc(0.025, 0.975)), each = 2L),
                y = cc(
                    quantile(cc(.x$Cts - .x$Cts_err, .x$Cts + .x$Cts_err), cc(0.025, 0.975)),
                    rev(quantile(cc(.x$Cts - .x$Cts_err, .x$Cts + .x$Cts_err), cc(0.025, 0.975)))))) %>%
        bind_rows -> boxes

    temp %>% 
        ggplot_sci(aes(
                x = HR, xmin = HR - HR_err, xmax = HR + HR_err,
                y = Cts, ymin = Cts - Cts_err, ymax = Cts + Cts_err)) +
            geom_point() +
            geom_errorbar() +
            geom_errorbarh() +
            geom_polygon(
                aes(x = x, y = y, group = Group, col = Group),
                    fill = "#FFFFFF",
                    size = 1.5,
                    alpha = 0,
                    data = boxes, inherit.aes = FALSE) +
            scale_color_manual(values = col_pal, drop = FALSE) +
            scale_y_log10_sci(name = "MAXI 2-20 keV, cts cm s^-2",
                sec.axis = dup_axis_sci_weak()) +
            scale_x_sci(name = "MAXI 10-20 keV / 2-4 keV",
                sec.axis = dup_axis_sci_weak()) -> plt

    grid.newpage()

    plt %>%
        postprocess_axes(
            axes_margin = u_(0.5~cm),
            strip_margin = NULL,
            text_margin = u_(1.25 ~ cm)) %>%
        grid.draw()

}

dates_range <- function(pattern = "data_") {
    paste0(pattern, 0:3) %>%
        map(get0, ifnotfound = NULL) %>%
        map(map, pull, JD) %>%
        map(range) %>%
        map(subtract, 2400000.5) %>%
        enframe(NULL) %>%
        transmute(Group = as_factor(cc("BrH", "HIM", "Soft", "DH")),
                  Lower = map_dbl(value, 1L),
                  Upper = map_dbl(value, 2L))
}

if (get0("ShouldRun", ifnotfound = FALSE)) {

    #read_x_rays() %>% transform_x_ray("2-20") %>% plot_x_ray_hr
    tic()
    file_path <- fs::path("Output", "Plots", "x-ray.tex")

    tikz(file_path,
        width = 8, height = 5,
        standAlone = TRUE)
    tryCatch(
        { read_x_rays() %>% transform_x_ray() %>% plot_x_ray() },
        finally = dev.off())


    tex_2_pdf(file_path)
    toc()
}