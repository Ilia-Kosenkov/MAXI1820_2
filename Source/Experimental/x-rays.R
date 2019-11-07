`%vec_in%` <- vctrs::vec_in

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
        vec_rbind(hardness_ratio)

        #bind_rows(
            #mutate(upper, BandId = fct_get(BandId)),
            #mutate(lower, BandId = fct_get(BandId))) %>%
        #mutate(BandId = as_factor(BandId)) -> result

    #left_join_condition(result, dates, .x$MJD >= .y$Lower, .x$MJD <= .y$Upper) %>%
        #select(-Lower, - Upper) %>%
        #mutate(Group = fct_recode(fct_explicit_na(Group, "No data"), !!!levels))
}

plot_x_ray <- function(data, dates_input = dates_range()) {
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
                  X = lin_unit(0.5 * (Lower + Upper), rng %>% expand_range(0.05), u_(0$npc, 1$npc)),
                  Y = u_(0.5 ~ cm))

    text_grob <- textGrob(labels_data$Label, labels_data$X, labels_data$Y, gp = gpar(fontsize = Style_TickFontSz))

    maxi_hr_name <- "\\small{MAXI HR}\n\\tiny{$\\frac{10-20~\\mathrm{keV}}{2-4~\\mathrm{keV}}$}"
    levels <- cc("2-4", "15-50", "10-20 / 2-4") %>%
                set_names(cc(
                          "\\small{MAXI 2$-$4 keV}\n\\small{cts~cm~s$^{-2}$}",
                          "\\small{BAT 15$-$50 keV}\n\\small{cts~cm~s$^{-2}$}", maxi_hr_name))

    data %<>% mutate(BandId = fct_recode(BandId,!!!levels))

    data %>%
        filter(BandId != maxi_hr_name) %>%
        mutate(BandId = fct_drop(BandId)) %>%
        group_split(BandId) %>%
        map(filter, Data - Err > 1e-4) %>%
        bind_rows %>%
        ggplot(aes(
                x = MJD, y = Data,
                ymin = Data - Err, ymax = Data + Err)) +
            coord_sci(xlim = rng, clip = "off") +
            theme_sci(
                text.size = Style_TickFontSz,
                title.size = Style_TickFontSz,
                facet.lab.x = npc_(0.94),
                facet.lab.y = npc_(0.88)) +
            geom_pointrange(size = 0.2) +
            geom_polygon(
                aes(x, y, fill = Group),
                alpha = 0.4,
                show.legend = FALSE,
                data = dates, inherit.aes = FALSE) +
            scale_fill_manual(values = col_pal) +
            scale_x_sci(
                name = NULL, limits = rng,
                labels = function(x) rep(" ", len(x)),
                sec.axis = dup_axis_sci_weak()) +
            scale_y_log10_sci(
                name = NULL,
                sec.axis = dup_axis_sci_weak(),
                labels = function(x) as.character(RLibs::glue_fmt("{x:%g}"))) +
            facet_sci(
                vars(BandId),
                scales = "free_y",
                panel.labeller = ~letters[.x$Id]) -> plt_1
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
        Clamp(Upp, hr_rng) %>%
        Clamp(Low, hr_rng)

    hardness_ratio %>% ggplot(aes(
                    x = MJD, y = Data,
                    ymin = Low, ymax = Upp)) +
                coord_sci(xlim = rng) +
                theme_sci(
                    legend.text = element_text(size = 0.75 * Style_TickFontSz),
                    legend.title = element_text(size = Style_TickFontSz),
                    text.size = Style_TickFontSz,
                    title.size = Style_TickFontSz,
                    legend.justification = cc(1.125, -0.05),
                    facet.lab.x = npc_(0.94),
                    facet.lab.y = npc_(0.88)) +
                geom_pointrange(size = 0.2) +
                geom_polygon(
                        aes(x, y_hr, group = Group, fill = Group),
                        alpha = 0.4,
                        data = dates, inherit.aes = FALSE) +
                scale_fill_manual(values = col_pal, guide = guide_legend(title = "")) +
                scale_x_sci(sec.axis = dup_axis_sci_weak()) +
                scale_y_sci(name = NULL, sec.axis = dup_axis_sci_weak()) +
                facet_sci(
                    vars(BandId),
                    scales = "free_y",
                    panel.labeller = ~letters[3]) -> plt_2

    plt_1 %<>%
        postprocess_axes(
            axes_margin = mar_(0.75 ~ cm, 0.25 ~ cm, 0 ~ npc, 1.2 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1.8 ~ cm))

    pos <- get_grobs_layout(plt_1, "axis-t-1-1")[[1]]

    plt_1 <- gtable::gtable_add_grob(
            plt_1, text_grob, pos[3], pos[1],
            clip = "off",
            name = "top-labels")

    plt_2 %<>% postprocess_axes(
            axes_margin = mar_(0 ~ npc, 0.25 ~ cm, 0.5 ~ cm, 1.2 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1.8 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 0.7 ~ cm, 0 ~ npc))
    gridExtra::arrangeGrob(plt_1, plt_2, ncol = 1, heights = u_(0.63 ~ null, 0.37 ~ null)) -> tbl

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

### TODO: move to {RLibs}
left_join_condition <- function(left, right, ...,
    .type = "first", .suffix = c("__l", "__r"), .enforce_suffix = FALSE) {
    cond <- enquos(...)

    if (vec_is(.type, character(), 1L)) {
        .type <- tolower(.type)
        if (.type == "first")
            selector <- function(x) head(x, 1)
        else if (.type == "last")
            selector <- function(x) tail(x, 1)
        else
            abort("Error", "maxi2_invalid_argument")
    }
    else
        selector <- as_function(.type)

    cond <- map(cond, function(cnd) {
        if (!quo_is_call(cnd))
            abort("Error", "maxi2_invalid_argument")

        expr <- quo_get_expr(cnd)
        expr <- expr(outer(!!expr[[2]], !!expr[[3]], !!expr[[1]]))
        quo_set_expr(cnd, expr)
    })


    cond %>%
        map(eval_tidy, list(.x = left, .y = right)) %>%
        reduce(`&`) -> match

    match
    match %>% apply(1, function(x) which(x)) -> indices

    indices %>% map_int(~ if(vec_is_empty(.x)) NA_integer_ else selector(.x)) -> indices
    right <- right[indices,]

    if (.enforce_suffix) {
        left <- set_names(left, paste0(names(left), .suffix[1]))
        right <- set_names(right, paste0(names(right), .suffix[2]))
    }
    else {
        common_names <- which(vec_in(names(right), names(left)))
        if (!vec_is_empty(common_names)) {
            left <- set_names(left,
                flatten_chr(map_at(
                    names(left),
                    which(vec_in(names(left), names(right)[common_names])),
                    paste0, .suffix[1])))
            right <- set_names(right,
                flatten_chr(map_at(
                    names(right),
                    common_names,
                    paste0, .suffix[2])))
        }

    }

    bind_cols(left, right)
}


if (get0("ShouldRun", ifnotfound = FALSE)) {

    #read_x_rays() %>% transform_x_ray("2-20") %>% plot_x_ray_hr

    file_path <- fs::path("Output", "Plots", "x-ray.tex")

    tikz(file_path,
        width = 8, height = 5,
        standAlone = TRUE)
    tryCatch(
        { read_x_rays() %>% transform_x_ray() %>% plot_x_ray() },
        finally = dev.off())

    tex_2_pdf(file_path)
}