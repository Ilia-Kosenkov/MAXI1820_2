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
        select(-value, -MJD_start, -MJD_end) %>%
        filter_range(MJD, cc(58190, 58450)) -> maxi_data

    path_bat %>%
        read_table(col_names = FALSE, col_types = cols(), comment = "#") %>%
        set_names(cc("MJD", "Data", "Err", "Year", "Day", "StatErr", "SysErr", "Flag", "Expo", "Coded", "Dithread")) %>%
        select("MJD", "Data", "Err") %>%
        mutate(BandId = "15-50") -> bat_data

    bind_rows(maxi_data, bat_data) %>%
        mutate(BandId = as_factor(BandId))

}

transform_x_ray <- function(data, dates = dates_range()) {
    levels <- c("Rising Hard" = "0",
                "Decoupling" = "1",
                "Soft" = "2",
                "Decaying Hard" = "3")
    lower <- filter(data, BandId == "2-4")
    upper <- filter(data, BandId == "10-20")
    bind_cols(select(upper, - BandId), transmute(lower, lData = Data, lErr = Err)) %>%
        mutate(Err = abs(Data / lData) * sqrt(Err ^ 2 / Data ^ 2 + lErr ^ 2 / lData ^ 2),
               Data = Data / lData,
                BandId = "10-20 / 2-4") %>%
        select(-starts_with("l")) %>%
        filter(Err < 0.5) %>%
        bind_rows(
            mutate(upper, BandId = fct_get(BandId)),
            mutate(lower, BandId = fct_get(BandId))) %>%
        mutate(BandId = as_factor(BandId)) -> result

    left_join_condition(result, dates, .x$MJD >= .y$Lower, .x$MJD <= .y$Upper) %>%
        select(-Lower, - Upper) %>%
        mutate(Group = fct_recode(fct_explicit_na(Group, "No data"), !!!levels))
}

plot_x_ray <- function(data) {

    ### Very-very-very experimental
    require(sciplotr)
    rng <- data %>% pull(MJD) %>% range
    col_pal <- cc(Style_GroupColors[cc(5, 8, 6, 7)], "#000000")
    alpha_pal <- cc(rep(1, 4), 0.5)
    shape_pal <- cc(Style_GC_Shapes[cc(5, 8, 6, 7) - 4], 16)
    data %>%
        filter(BandId != "10-20 / 2-4") %>%
        mutate(BandId = fct_drop(BandId)) %>%
        ggplot(aes(
                x = MJD, y = Data,
                ymin = Data - Err, ymax = Data + Err,
                col = Group, fill = Group, alpha = Group, shape = Group)) +
            coord_sci(xlim = rng) +
            theme_sci(
                facet.lab.x = npc_(0.97)) +
            geom_pointrange() +
            scale_x_sci(
                name = NULL, limits = rng,
                labels = function(x) rep(" ", len(x)),
                sec.axis = sciplotr:::dup_axis_sci_weak()) +
            scale_y_log10_sci(
                name = NULL,
                sec.axis = sciplotr:::dup_axis_sci_weak(),
                labels = function(x) as.character(RLibs::glue_fmt("{x:%g}"))) +
            scale_color_manual(values = col_pal, drop = FALSE, guide = FALSE) +
            scale_fill_manual(values = col_pal, drop = FALSE, guide = FALSE) +
            scale_alpha_manual(values = alpha_pal, drop = FALSE, guide = FALSE) +
            scale_shape_manual(values = shape_pal, drop = FALSE, guide = FALSE) +
            facet_sci(
                vars(BandId),
                scales = "free_y",
                panel.labeller = ~letters[.x$Id]) -> plt_1
    data %>%
        filter(BandId == "10-20 / 2-4") %>%
        mutate(BandId = fct_drop(BandId)) %>%
        ggplot(aes(
                    x = MJD, y = Data,
                    ymin = Data - Err, ymax = Data + Err,
                    col = Group, fill = Group, shape = Group, alpha = Group)) +
                coord_sci(xlim = rng) +
                theme_sci(
                    facet.lab.x = npc_(0.97)) +
                geom_pointrange() +
                scale_x_sci(sec.axis = sciplotr:::dup_axis_sci_weak()) +
                scale_y_sci(name = NULL, sec.axis = sciplotr:::dup_axis_sci_weak()) +
                scale_color_manual(
                    values = col_pal, drop = FALSE,
                    guide = guide_legend(title = NULL)) +
                scale_fill_manual(values = col_pal, drop = FALSE,
                    guide = guide_legend(title = NULL)) +
                scale_shape_manual(values = shape_pal, drop = FALSE,
                    guide = guide_legend(title = NULL)) +
                scale_alpha_manual(values = alpha_pal, drop = FALSE,
                    guide = guide_legend(title = NULL)) +
                facet_sci(
                    vars(BandId),
                    scales = "free_y",
                    panel.labeller = ~letters[3]) -> plt_2
    plt_1 %<>%
        postprocess_axes(
            axes_margin = mar_(1 ~ cm, 1 ~ cm, 0 ~ npc, 1 ~ cm),
            strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 1 ~ cm))

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
        transmute(Group = as_factor(0:3),
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

    #dates_range() -> dates
    read_x_rays() %>% print# %>% transform_x_ray(dates) %>% plot_x_ray
}