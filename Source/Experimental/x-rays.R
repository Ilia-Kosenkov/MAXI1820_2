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

transform_x_ray <- function(data) {
    lower <- filter(data, BandId == "2-4")
    upper <- filter(data, BandId == "10-20")
    bind_cols(select(upper, - BandId), transmute(lower, lData = Data, lErr = Err)) %>%
        mutate(Err = abs(Data / lData) * sqrt(Err ^ 2 / Data ^ 2 + lErr ^ 2 / lData ^ 2),
               Data = Data / lData,
               BandId = as_factor("10-20 / 2-4")) %>%
        select(-starts_with("l")) %>%
        filter(Err < 1) -> hardness_ratio

    data %>%
        filter(BandId %vec_in% cc("2-4", "15-50")) %>%
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
                  X = lin_unit(0.5 * (Lower + Upper), rng, u_(0$npc + 1$cm, 1$npc - 1$cm)),
                  Y = u_(0.5 ~ cm))

    text_grob <- textGrob(labels_data$Label, labels_data$X, labels_data$Y)

    maxi_hr_name <- "HR 10-20 keV/2-4 keV"
    levels <- cc("2-4", "15-50", "10-20 / 2-4") %>%
                set_names(cc("MAXI 2-4 keV", "BAT 15-50 keV", maxi_hr_name))

    data %<>% mutate(BandId = fct_recode(BandId,!!!levels))
    
    data %>%
        filter(BandId != maxi_hr_name) %>%
        mutate(BandId = fct_drop(BandId)) %>%
        ggplot(aes(
                x = MJD, y = Data,
                ymin = Data - Err, ymax = Data + Err)) +
            coord_sci(xlim = rng, clip = "off") +
            theme_sci(
                facet.lab.x = npc_(0.95)) +
            geom_pointrange() +
            geom_polygon(
                aes(x, y, fill = Group),
                alpha = 0.4,
                show.legend = FALSE,
                data = dates, inherit.aes = FALSE) +
            scale_fill_manual(values = col_pal) +
            scale_x_sci(
                name = NULL, limits = rng,
                labels = function(x) rep(" ", len(x)),
                sec.axis = sciplotr:::dup_axis_sci_weak()) +
            scale_y_log10_sci(
                name = NULL,
                sec.axis = sciplotr:::dup_axis_sci_weak(),
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
                    legend.justification = cc(1.25, -0.1),
                    facet.lab.x = npc_(0.95)) +
                geom_pointrange() +
                geom_polygon(
                        aes(x, y_hr, group = Group, fill = Group),
                        alpha = 0.4,
                        data = dates, inherit.aes = FALSE) +
                scale_fill_manual(values = col_pal, guide = guide_legend(title = "State")) +
                scale_x_sci(sec.axis = sciplotr:::dup_axis_sci_weak()) +
                scale_y_sci(name = NULL, sec.axis = sciplotr:::dup_axis_sci_weak()) +
                facet_sci(
                    vars(BandId),
                    scales = "free_y",
                    panel.labeller = ~letters[3]) -> plt_2

    plt_1 %<>%
        postprocess_axes(
            axes_margin = mar_(1 ~ cm, 1 ~ cm, 0 ~ npc, 1 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1 ~ cm))

    pos <- get_grobs_layout(plt_1, "axis-t-1-1")[[1]]

    plt_1 <- gtable::gtable_add_grob(
            plt_1, text_grob, pos[3], pos[1],
            clip = "off",
            name = "top-labels")

    plt_2 %<>% postprocess_axes(
            axes_margin = mar_(0 ~ npc, 1 ~ cm, 1 ~ cm, 1 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1 ~ cm),
            text_margin = mar_(0 ~ npc, 0 ~ npc, 0.75 ~ cm, 0 ~ npc))
    gridExtra::arrangeGrob(plt_1, plt_2, ncol = 1, heights = u_(0.66 ~ null, 0.33 ~ null)) -> tbl

    grid.newpage()
    grid.draw(tbl)
}

dates_range <- function(pattern = "data_") {
    paste0(pattern, 0:3) %>%
        map(get0, ifnotfound = NULL) %>%
        map(map, pull, JD) %>%
        map(range) %>%
        map(subtract, 2400000.5) %>%
        enframe(NULL) %>%
        transmute(Group = as_factor(cc("BH", "HIM", "Soft", "DH")),
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

lin_unit <- function(x0, x, y) {
    dx <- x[2] - x[1]
    dy <- y[2] - y[1]

    map(x0, ~ y[1] + dy / dx * (.x - x[1])) -> result
    exec(grid::unit.c, !!!result)
}

if (get0("ShouldRun", ifnotfound = FALSE)) {

    read_x_rays() %>% transform_x_ray() %>% plot_x_ray
}