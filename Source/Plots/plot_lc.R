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

PlotLC <- function(data, avg_data,
        x, y,
        ymin, ymax,
        group,
        xrng = NULL, yrng = NULL,
        xlab = quo_text(x),
        ylab = quo_text(y),
        isTex = FALSE,
        noXLabels = FALSE) {

    x <- ensym(x)
    y <- ensym(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    group <- ensym(group)

    ynms <- GetMinMaxNames(!!y)

    if (quo_is_missing(ymin))
        ymin <- ynms$min
    else
        ymin <- quo_squash(ymin)

    if (quo_is_missing(ymax))
        ymax <- ynms$max
    else
        ymax <- quo_squash(ymax)

    if (!is.null(yrng)) {
        data %<>%
            clamp(!!ymin, yrng) %>%
            clamp(!!ymax, yrng)

        avg_data %<>%
            clamp(!!ymin, yrng) %>%
            clamp(!!ymax, yrng)
    }

    tic(glue("Generated plot for {y}"))

    p <- ggplot(data,
        aes(x = !!x, y = !!y,
            ymin = !!ymin,
            ymax = !!ymax,
            col = !!group,
            fill = !!group,
            shape = !!group
            )) +
        scale_color_manual(
            limits = Style_Groups,
            values = Style_GroupColors,
            guide = FALSE) +
        scale_fill_manual(
            limits = Style_Groups,
            values = Style_GroupFills,
            guide = FALSE) +
        scale_shape_manual(
            limits = Style_Groups,
            values = Style_GroupShapes,
            guide = FALSE) +
        geom_point(size = Style_SymbolSizeSmall) +
        geom_errorbar(size = Style_ErrorBarSize,
            width = 0) +
        geom_point(data = avg_data %>%
                ModifyLevels(Group, function(g) as.numeric(g) + 4),
            size = Style_SymbolSize) +
        geom_errorbar(data = avg_data %>%
                ModifyLevels(Group, function(g) as.numeric(g) + 4),
            width = 0,
            size = Style_ErrorBarSizeLarge) +
        DefaultTheme(textSz = Style_TickFontSz, titleSz = Style_LabelFontSz) +
        coord_cartesian(xlim = xrng, ylim = yrng,
                        clip = "on",
                        expand = FALSE)

    toc()

    tic(glue("Decorated {y}"))
    if (is_null(xrng))
        xrng <- GGPlotGetRange(p)$x
    if (is_null(yrng))
        yrng <- GGPlotGetRange(p)$y


    p %<>%
        LinearScaleTicks(xrng, gp = gpar(fontsize = Style_TickFontSz),
            brTrans =
                if (noXLabels)
                    function(x) rep(" ", length(x))
                else identity) %>%
        LinearScaleTicks(
            yrng,
            side = "y", n = 4,
            brTrans = function(x)
                LabelsRep(x, isTex = isTex, sameDigitCount = TRUE),
            gp = gpar(fontsize = Style_TickFontSz))

    p <- p + ylab(ylab) + xlab(xlab)
    toc()
    p
}

prepare_lc_data <- function(
    data = ReadAllAvgData(),
    avg_data = ReadAllAvgData(pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")) {

    map2(
        data %>% map(mutate, Type = as_factor("N")),
        avg_data %>% map(mutate, Type = as_factor("A")),
        vec_rbind) %>%
    vec_rbind_uq(.names_to = "Band") %>%
    identity %>%
    transmute(MJD, Obs_P = P, Err_P = SG, Obs_A = A, Err_A = SG_A, Group, Band = as_factor(Band), Type) %>%
    pivot_longer(matches("^Obs|^Err"), names_pattern = "^(.*)_(.*)$", names_to = c(".value", "Var")) %>%
    mutate(Var = as_factor(Var), Band = fct_relevel(Band, "B", "V", "R"))
}

plot_lc <- function(data) {
    require(sciplotr)
    ids <- data %>%
        filter(Var == "P") %>%
        transmute(test = Obs < 1 & Obs > 0.2 & Err < 0.3) %>%
        pull(test) %>%
        which

    data %>%
        group_by(Var) %>%
        slice(ids) %>%
        ungroup %>%
        mutate(JoinedType = interaction(Group, Type)) -> prep_data


    limits <- cc(paste0(cc(0, 2, 3, 1), ".N"), paste0(cc(0, 2, 3, 1), ".A"))
    col_pal <- Style_GroupColors[1:len(limits)]
    shape_pal <- Style_GroupShapes[1:len(limits)]
    size_pal <- vec_repeat(cc(Style_SymbolSizeSmall, Style_SymbolSize), each = len(limits) / 2)

    list(
         var = cc("P", "A"),
         offset = cc(0L, 3L),
         lab = cc("$P_{{{.x$rows$Band}}}$~(\\%)", "$\\theta_{{{.x$rows$Band}}}$~(deg)")) %>%
         pmap(function(var, offset, lab) {

             prep_data %>%
                filter(Var == var) %>%
                ggplot_sci(aes(MJD, Obs, ymin = Obs - Err, ymax = Obs + Err,
                    col = JoinedType,
                    fill = JoinedType,
                    size = JoinedType,
                    shape = JoinedType)) +
                theme_sci(
                    ticks = -u_(5 ~ pt),
                    text.size = Style_TickFontSz,
                    title.size = Style_LabelFontSz,
                    facet.lab.x = npc_(0.07),
                    facet.lab.y = npc_(0.88)) +
                geom_errorbar(width = 0, size = 0.7) +
                geom_point() +
                scale_color_manual(values = col_pal, limits = limits, guide = NULL) +
                scale_fill_manual(values = col_pal, limits = limits, guide = NULL) +
                scale_shape_manual(values = shape_pal, limits = limits, guide = NULL) +
                scale_size_manual(values = size_pal, limits = limits, guide = NULL) +
                scale_x_sci(sec.axis = dup_axis_sci_weak(), minor_breaks_n = 20) +
                scale_y_sci(sec.axis = dup_axis_sci_weak(), name = NULL, breaks_n = 3, minor_breaks_n = 20) +
                facet_sci(vars(Band), scale = "fixed",
                          labeller = label_f(.f_left = ~RLibs::glue_fmt(lab)),
                          panel.labeller = ~RLibs::glue_fmt("({letters[.x$Id + offset]})"))
         }) %>%
    map(postprocess_axes,
        axes_margin = mar_(0.25 ~ cm, 0.25 ~ cm, 0.5 ~ cm, 1.1 ~ cm),
        strip_margin = mar_(0 ~ npc, 0 ~ npc, 0 ~ npc, 0.85 ~ cm),
        text_margin = mar_(0 ~ npc, 0 ~ npc, 1.1 ~ cm, 0 ~ npc)) -> grobs

    grid.newpage()
    exec(gridExtra::arrangeGrob, !!!grobs, ncol = 2, widths = u_(0.5 ~ npc, 0.5 ~ npc)) %>%
        grid.draw
}

if (get0("ShouldRun", ifnotfound = FALSE)) {
    tic()
    file_path <- fs::path("Output", "Plots", "lc_new.tex")
    tikz(file_path, width = 2 * Style_WidthStdInch, height = Style_HeightStdInch, standAlone = TRUE)
    tryCatch(prepare_lc_data() %>% plot_lc(), finally = dev.off())
    tex_2_pdf(file_path)
    toc()
    #if (FALSE) {
    #tic("Total")
    #grps <- c(0, 1, 2, 3)
    #bndOrder <- Bands %>% pull(Band)
    #data <- ReadAllAvgData()[bndOrder] %>%
        #map(filter, Group %in% grps) %>%
        #map(filter, SG_A <= 10, SG <= 0.2)#  %T>% map(~print(., n = vec_size(.)))

    #data_avg <- ReadAllAvgData(
        #pattern = "pol_avg_all_(?<id>[0-9]+)_(?<band>\\w)")[bndOrder] %>%
        #map(filter, Group %in% grps)

    #types <- c("P", "A")
    #typeLabs <- c("$P_{..3}$ (\\%)", "$\\theta_{..3}$ (deg) ")

    #xlim <- data %>%
        #map(pull, MJD) %>%
        #range %>%
        #expand_interval(factor = 0.06)

    #ylim <- types %>%
        #map(GetMinMaxNames) %>%
        #map(~map(data, select, !!.x[[1]], !!.x[[2]])) %>%
        #map(unlist) %>%
        #map(quantile, c(0.001, 0.991))

    #grobs <-
        #future_pmap(
            #list(types, ylim, typeLabs, c(0, 3)),
            #function(col, ylim, lab, offset) {
                #pmap(list(data, data_avg, names(data), c(rep(TRUE, length(data) - 1), FALSE)),
                    #~PlotLC(.x, .y,
                        #MJD, !!sym(col),
                        #xrng = xlim, yrng = ylim,
                        #group = Group,
                        #ylab = glue(lab),
                        #isTex = TRUE,
                        #noXLabels = ..4)) %>%
                #GGPlotPanelLabs(
                    #labels = letters[seq_len(length(data)) + offset],
                    #hjust = 3, vjust = 2,
                    #gp = gpar(fontsize = Style_LabelFontSz))
            #}, .progress = TRUE)

    #pth <- fs::path("Output", "Plots") %T>% (fs::dir_create)

    #tic("Pdfs saved")
    #future_map2(grobs, types,
        #function(g, tp) {
            #lPth <- fs::path(pth, glue("light_curve_{tp}.tex"))
            #tikz(lPth,
                #width = Style_WidthStdInch,
                #height = Style_HeightStdInch * 1.5,
                #standAlone = TRUE)
            #tryCatch({
                #g %>%
                    #GGPlot2GrobEx %>%
                    #GrobsArrange(
                        #ncol = 1,
                        #labsMar = Style_LabsMarStd,
                        #axisMar = Style_AxisMarStd,
                        #vGap = Style_VGapStd) %>%
                     #GrobPlot
            #}, finally = dev.off())

            #tex_2_pdf(lPth)
        #}, .progress = TRUE)
    #toc()
    #toc()
    #tic.clear()
    #tic.clearlog()
}
