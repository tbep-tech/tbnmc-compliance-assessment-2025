# caption for ra table
ratabcap <- function(segin) {
  namein <- targets %>%
    filter(bay_segment %in% !!segin) %>%
    pull(name)

  out <- paste0(
    "Demonstration of reasonable assurance assessment steps for ",
    namein,
    ". Green and red squares indicate outcomes of decision points outlined in the Consortium's reasonable assurance assessment framework."
  )

  return(out)
}

# header table with to, from, etc.
headertab <- function(dt = NULL, maxyr, fsz = 13) {
  if (is.null(dt)) {
    dt <- as.character(as.Date(as.numeric(Sys.Date()), origin = '1970-01-01'))
  }

  totab <- tibble(
    first = c('TO:', '', 'FROM:', 'DATE:', 'SUBJECT:', 'cc', '', '', '', ''),
    second = c(
      'Adam Blalock, FDEP',
      'Kevin J. McOmber, US EPA Region 4',
      'Ed Sherwood, TBEP Executive Director (NMC Facilitator)',
      dt,
      paste(
        maxyr,
        'Tampa Bay Nutrient Management Compliance Assessment Results'
      ),
      'Ken Weaver, Jessica Mostyn, Ben Ralys, Kevin O’Donnell, Lawrence Glenn, Kenneth Hayman (FDEP Tallahssee)',
      'Ramandeep Kaur, Jorge Perez, Lance Kautz, Jessica Pein, Erica Peck (FDEP Tampa)',
      'Craig Hesterlee, Jeaneanne M. Gettle, Wade Lehmann, Cindy Barger, Nancy Laurson, Felicia Burks, Johnnie Purify (EPA Region 4/HQ)',
      'Michele Duggan, Santino Provenzano (TBNMC)',
      'Ed Sherwood, Maya Burke, Marcus Beck (TBEP)'
    )
  )

  out <- flextable(totab) %>%
    width(j = 1, 1) %>%
    width(j = 2, 5.5) %>%
    fontsize(i = 1:4, size = fsz) %>%
    fontsize(i = 6:10, size = fsz * 0.8461538) %>%
    delete_part('header') %>%
    border_remove() %>%
    font(fontname = 'Arial', part = 'all') %>%
    valign(valign = 'top') %>%
    color(color = '#636363', part = 'all')

  return(out)
}

nmcstepstab <- function(fsz = 13) {
  totab <- tibble(
    col1 = c(
      '**Assessment Step**',
      '**I.** Determine annual bay segment specific chlorophyll-a FDEP threshold attainment as traditionally assessed using the Decision Matrix management strategy developed by the TBEP [@tbep0400].',
      NA,
      '**II.** Review data and determine if an anomalous event(s) influenced non-attainment of the bay segment specific chlorophyll-a threshold.',
      NA,
      '**III.** Determine if the chlorophyll-a thresholds have been exceeded for <2 consecutive years.',
      NA,
      '**IV.** Determine if the bay segment specific federally-recognized TMDL has been achieved using the hydrologically-adjusted compliance assessment outlined in NMC Decision Memo #11 (Appendix 2-11).',
      NA,
      '**V.** For a given year or for multiple years, compile and report entity-specific combined source loads in comparison to 5-yr annual average reasonable assurance allocation.'
    ),
    col2 = c(
      '**Result**',
      '**Yes**',
      '**No**',
      '**Yes**',
      '**No**',
      '**Yes**',
      '**No**',
      '**Yes**',
      '**No**',
      '**Compile & Report**'
    ),
    col3 = c(
      '**Action**',
      '**NMC Action 1**',
      '**NMC Action 1**',
      '**NMC Action 2**',
      '**Go to III**',
      '**NMC Action 2**',
      '**Go to IV**',
      '**NMC Action 3**',
      '**Go to V**',
      '**NMC Action 4**'
    )
  )

  out <- flextable(totab) %>%
    font(fontname = 'Arial', part = 'all') %>%
    fontsize(size = fsz) %>%
    delete_part('header') %>%
    border_inner() %>%
    border_outer() %>%
    align(j = 2:3, align = 'center') %>%
    merge_at(i = 2:3, j = 1) %>%
    merge_at(i = 4:5, j = 1) %>%
    merge_at(i = 6:7, j = 1) %>%
    merge_at(i = 8:9, j = 1) %>%
    width(j = 1, 3.5) %>%
    width(j = 2:3, 1.5) %>%
    set_table_properties(
      opts_pdf = list(arraystretch = 3)
    ) %>%
    colformat_md()

  return(out)
}

nmcactionstab <- function(fsz = 13) {
  totab <- tibble(
    col1 = c(
      "NMC Action 1 -",
      "NMC Action 2 -",
      "NMC Action 3 -",
      "NMC Action 4 -"
    ),
    col2 = c(
      "A report assessing attainment of bay segment specific chlorophyll-a thresholds using the EPCHC dataset, as traditionally assessed using the Decision Matrix management strategy developed by the TBEP [@tbep0400] will be delivered to FDEP and EPA (this report).",
      "A report of the anomalous event(s) or data which influenced the bay segment chlorophyll-a exceedence will be delivered to FDEP and EPA, upon review by NMC participants (this report).",
      "Consider re-evaluation of the bay segment assimilative capacity based on nonattainment of bay segment chlorophyll-a threshold while meeting federally-recognized TMDL.",
      "If federally-recognized TMDL not achieved, compile results of hydrologic evaluation for FDEP's review and identify potential further actions needed to achieve reasonable assurance for bay segment allocations."
    )
  )

  out <- flextable(totab) %>%
    width(j = 1, 1.5) %>%
    width(j = 2, 5) %>%
    colformat_md() %>%
    font(fontname = 'Arial', part = 'all') %>%
    delete_part('header') %>%
    border_remove() %>%
    valign(valign = 'top') %>%
    fontsize(size = fsz)

  return(out)
}

# hydro load table
hydrotab <- function(maxyr, noaa_key, fsz = 13) {
  levs <- c(
    'Old Tampa Bay',
    'Hillsborough Bay',
    'Middle Tampa Bay',
    'Lower Tampa Bay'
  )

  # get adjustment estimates
  hydroload <- try(anlz_hydroload(maxyr, noaa_key), silent = T)
  while (inherits(hydroload, 'try-error')) {
    hydroload <- try(anlz_hydroload(maxyr, noaa_key), silent = T)
  }

  # extra static content
  histest <- tibble::tibble(
    `Bay Segment` = levs,
    `1992 - 1994 Hydrology (95% Prediction Interval, million m3)` = c(
      '383 - 548',
      '753-1110',
      '524-756',
      '312-402'
    )
  )

  # format
  totab <- hydroload %>%
    left_join(histest, ., by = 'Bay Segment') %>%
    select(-Year, -`Adjusted?`, -`Compliance Load`) %>%
    mutate(`Bay Segment` = factor(`Bay Segment`, levels = levs)) %>%
    arrange(`Bay Segment`)

  out <- flextable(totab) %>%
    width(j = 1:4, width = 6.5 / 4) %>%
    bold(part = 'header') %>%
    colformat_double(digits = 2) %>%
    font(fontname = 'Arial', part = 'all') %>%
    valign(valign = 'top', part = 'header') %>%
    fontsize(size = fsz, part = 'all')

  return(out)
}

show_rathrplot <- function(
  datin,
  bay_segment = c('OTB', 'HB', 'MTB', 'LTB', 'BCBS', 'TCB', 'MR'),
  thr = c('chla', 'la'),
  trgs = NULL,
  yrrng = c(1975, 2019),
  labelexp = TRUE,
  txtlab = TRUE,
  thrs = FALSE,
  partialyr = FALSE
) {
  maxyr <- yrrng[2]

  # default targets from data file
  if (is.null(trgs)) {
    trgs <- targets
  }

  # yrrng must be in ascending order
  if (yrrng[1] >= yrrng[2]) {
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2019)')
  }

  # segment
  bay_segment <- match.arg(bay_segment)

  # wq to plot
  thr <- match.arg(thr)

  # colors
  cols <- c(
    "Annual Mean" = "red",
    "Management Target" = "blue",
    "+1 se (small exceedance)" = "blue",
    "+2 se (large exceedance)" = "blue"
  )

  # averages
  aves <- anlz_raavedat(datin, partialyr = partialyr)

  # axis label
  if (labelexp) {
    axlab <- ifelse(
      thr == 'chla',
      expression("Mean Ann. Chl-a (" ~ mu * "g\u00B7L"^-1 * ")"),
      ifelse(thr == 'la', expression("Mean Ann. Light Att. (m  "^-1 * ")"), NA)
    )
  }
  if (!labelexp) {
    axlab <- dplyr::case_when(
      thr == 'chla' ~ "Mean Ann. Chl-a (ug/L)"
    )
  }

  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  trgnum <- toln %>% dplyr::pull(!!paste0(thr, '_target'))
  smlnum <- toln %>% dplyr::pull(!!paste0(thr, '_smallex'))
  thrnum <- toln %>% dplyr::pull(!!paste0(thr, '_thresh'))

  # change label location if thrs is true
  if (!thrs) {
    num <- trgnum
  }
  if (thrs) {
    num <- thrnum
  }

  # threshold label
  if (labelexp) {
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "~ mu * g%.%L^{-1}")
    )
  }
  if (!labelexp) {
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "ug/L")
    )
  }

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)

  if (partialyr) {
    ttl <- paste0(ttl, '*')
  }

  # get data to plo
  toplo <- aves$ann %>%
    dplyr::filter(grepl(paste0('_', thr, '$'), var)) %>%
    mutate(var = 'yval') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(var, val)

  p <- ggplot(toplo) +
    geom_rect(
      xmin = 2022,
      xmax = maxyr,
      ymin = -Inf,
      ymax = Inf,
      fill = 'grey',
      alpha = 0.6
    ) +
    geom_point(
      data = toplo,
      aes(x = yr, y = yval, colour = "Annual Mean"),
      size = 3
    ) +
    geom_line(
      data = toplo,
      aes(x = yr, y = yval, colour = "Annual Mean"),
      linetype = 'solid',
      linewidth = 0.75
    ) +
    labs(y = axlab, title = ttl) +
    scale_x_continuous(breaks = seq(1975, maxyr, by = 5)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      legend.position = 'top', #c(0.85, 0.95),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = '#ECECEC'),
      legend.title = element_blank(),
      axis.text.y = element_text(colour = 'black', size = 14),
      axis.title = element_blank(),
      plot.title = element_text(size = 22, colour = 'black'),
      legend.text = element_text(size = 16, colour = 'black'),
      axis.text.x = element_text(
        colour = 'black',
        angle = 0,
        size = 14,
        hjust = 0.5
      )
    )

  # all targets/thresholds
  if (!thrs) {
    p <- p +
      geom_hline(aes(yintercept = trgnum, colour = 'Management Target')) +
      geom_hline(
        aes(yintercept = smlnum, colour = '+1 se (small exceedance)'),
        linetype = 'dashed'
      ) +
      geom_hline(
        aes(yintercept = thrnum, colour = '+2 se (large exceedance)'),
        linetype = 'dotted'
      ) +
      scale_colour_manual(
        values = cols,
        labels = factor(names(cols), levels = names(cols))
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(
            shape = c(19, NA, NA, NA),
            colour = cols,
            linetype = c('solid', 'solid', 'dashed', 'dotted'),
            size = c(0.75, 0.5, 0.5, 0.5)
          )
        )
      )
  }

  # thresholds only
  if (thrs) {
    p <- p +
      geom_hline(
        aes(yintercept = thrnum, colour = '+2 se (large exceedance)'),
        linetype = 'dotted'
      ) +
      scale_colour_manual(
        values = cols[c(1, 4)],
        labels = factor(names(cols[c(1, 4)]), levels = names(cols[c(1, 4)]))
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(
            shape = c(19, NA),
            colour = cols[c(1, 4)],
            linetype = c('solid', 'dotted'),
            size = c(0.75, 0.5)
          )
        )
      )
  }

  if (txtlab & !thrs) {
    p <- p +
      geom_text(
        aes(yrrng[1], num, label = trglab),
        parse = labelexp,
        hjust = 0.2,
        vjust = 1,
        colour = 'blue'
      )
  }

  if (txtlab & thrs) {
    p <- p +
      geom_text(
        aes(yrrng[1], max(yval), label = trglab),
        parse = labelexp,
        hjust = 0.2,
        vjust = 1,
        colour = 'blue'
      )
  }

  if (partialyr) {
    p <- p +
      labs(
        caption = paste0(
          '*Incomplete data for ',
          max(yrrng),
          ' estimated by five year average'
        )
      )
  }

  return(p)
}

# annual chlorophyll figure
show_rachlplot <- function(chldat, maxyr) {
  yrrng <- c(1975, maxyr)

  p1 <- show_rathrplot(
    chldat,
    bay_segment = "OTB",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p2 <- show_rathrplot(
    chldat,
    bay_segment = "HB",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p3 <- show_rathrplot(
    chldat,
    bay_segment = "MTB",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p4 <- show_rathrplot(
    chldat,
    bay_segment = "LTB",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p5 <- show_rathrplot(
    chldat,
    bay_segment = "BCBS",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p6 <- show_rathrplot(
    chldat,
    bay_segment = "TCB",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )
  p7 <- show_rathrplot(
    chldat,
    bay_segment = "MR",
    thr = "chla",
    yrrng = yrrng,
    thrs = T
  )

  p <- (guide_area() /
    (p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(ncol = 2))) +
    plot_layout(
      ncol = 1,
      guides = 'collect',
      heights = unit(c(1, 1), c("cm", "null"))
    )

  return(p)
}

anlz_raavedat <- function(datin, partialyr = FALSE) {
  # year month averages
  # long format, separate bay_segment for MTB into sub segs
  # mtb year month averages are weighted
  moout <- datin %>%
    dplyr::select(yr, mo, bay_segment, station, chla) %>%
    tidyr::gather('var', 'val', chla) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      bay_segment = dplyr::case_when(
        station %in% c(9, 11, 81, 84) ~ "MT1",
        station %in% c(13, 14, 32, 33) ~ "MT2",
        station %in% c(16, 19, 28, 82) ~ "MT3",
        TRUE ~ bay_segment
      )
    ) %>%
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(val = mean(val), .groups = 'drop') %>%
    drop_na() %>%
    dplyr::mutate(
      val = dplyr::case_when(
        bay_segment %in% "MT1" ~ val * 2108.7,
        bay_segment %in% "MT2" ~ val * 1041.9,
        bay_segment %in% "MT3" ~ val * 974.6,
        TRUE ~ val
      ),
      bay_segment = dplyr::case_when(
        bay_segment %in% c('MT1', 'MT2', 'MT3') ~ 'MTB',
        TRUE ~ bay_segment
      )
    ) %>%
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(
      val = sum(val),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      val = dplyr::case_when(
        bay_segment %in% 'MTB' ~ val / 4125.2,
        TRUE ~ val
      )
    ) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, mo, bay_segment)

  # add partial year
  if (partialyr) {
    # years to averge, last five complete
    maxyr <- max(moout$yr)
    yrfl <- c(maxyr - 5, maxyr - 1)

    # months to fill
    mofl <- moout %>%
      dplyr::filter(yr %in% maxyr) %>%
      dplyr::pull(mo) %>%
      unique %>%
      setdiff(1:12, .)

    # month averages
    moave <- moout %>%
      dplyr::filter(yr >= yrfl[1] & yr <= yrfl[2]) %>%
      dplyr::group_by(bay_segment, mo, var) %>%
      summarise(
        val = mean(val, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::filter(mo %in% mofl) %>%
      dplyr::mutate(yr = maxyr)

    # join missing months to
    moout <- moout %>%
      dplyr::bind_rows(moave) %>%
      dplyr::arrange(var, yr, mo, bay_segment)
  }

  # annual data
  anout <- moout %>%
    dplyr::group_by(yr, bay_segment, var) %>%
    dplyr::summarise(
      val = mean(val),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        TRUE ~ var
      )
    ) %>%
    tidyr::spread('var', 'val') %>%
    tidyr::gather('var', 'val', mean_chla) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, bay_segment)

  # mo dat to light attenuation
  moout <- moout %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla'
      )
    )

  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )

  return(out)
}

# chlorophyll boxplots all segments
show_chlboxplot <- function(chldat, maxyr) {
  yrrng <- c(1975, maxyr)
  txtcol <- 'black'
  thrthm <- theme(
    plot.background = element_rect(fill = NA, color = NA),
    axis.text.y = element_text(colour = txtcol, size = 12),
    axis.title = element_blank(),
    plot.title = element_text(size = 22, colour = txtcol),
    legend.text = element_text(size = 16, colour = txtcol),
    axis.text.x = element_text(
      size = 14,
      colour = txtcol,
      angle = 0,
      hjust = 0.5
    ),
    legend.position = 'top'
  )

  p1 <- show_raboxplot(
    chldat,
    bay_segment = "OTB",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p2 <- show_raboxplot(
    chldat,
    bay_segment = "HB",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p3 <- show_raboxplot(
    chldat,
    bay_segment = "MTB",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p4 <- show_raboxplot(
    chldat,
    bay_segment = "LTB",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p5 <- show_raboxplot(
    chldat,
    bay_segment = "BCBS",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p6 <- show_raboxplot(
    chldat,
    bay_segment = "TCB",
    yrrng = yrrng,
    yrsel = maxyr
  )
  p7 <- show_raboxplot(
    chldat,
    bay_segment = "MR",
    yrrng = yrrng,
    yrsel = maxyr
  )

  p <- (guide_area() /
    (p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(ncol = 2))) +
    plot_layout(
      ncol = 1,
      guides = 'collect',
      heights = unit(c(1, 1), c("cm", "null"))
    ) &
    thrthm

  return(p)
}

# chloropyll matrix
show_chlmatrix <- function(wqdat, maxyr) {
  out <- show_rawqmatrix(
    wqdat,
    param = 'chla',
    yrrng = c(1975, maxyr),
    txtsz = 5,
    abbrev = T
  ) +
    geom_hline(yintercept = 2021.5, size = 2, color = 'grey') +
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      axis.text.y = element_text(size = 14, colour = 'black'),
      axis.text.x = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 22, colour = 'black')
    )

  return(out)
}

show_rasitemap <- function(
  chldat,
  yrsel,
  mosel = c(1, 12),
  param = c('chla', 'la'),
  trgs = NULL,
  thrs = FALSE,
  partialyr = FALSE
) {
  # sanity check
  # default targets from data file
  if (is.null(trgs)) {
    trgs <- targets
  }

  # correct month entry
  if (any(!mosel %in% 1:12)) {
    stop('mosel not in range of 1 to 12')
  }
  if (length(mosel) == 2) {
    if (mosel[2] < mosel[1]) {
      stop('mosel must be in ascending order')
    }
  }
  if (length(mosel) > 2) {
    stop('mosel must be length 1 or 2')
  }

  # parameter
  param <- match.arg(param)

  # logical if full year
  fullyr <- sum(c(1, 12) %in% mosel) == 2

  prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

  # site averages
  tomap <- chldat %>%
    anlz_raavedatsite(partialyr = partialyr)

  # get site averages for selected year
  if (fullyr) {
    tomap <- tomap %>%
      anlz_attainsite(yrrng = yrsel, thr = param, trgs = trgs, thrs = thrs) %>%
      dplyr::mutate(
        met = factor(met, levels = c('yes', 'no'), labels = c('yes', 'no'))
      )

    # legend label
    leglab <- paste0('Target met\nin ', yrsel, '?')
    if (thrs) {
      leglab <- paste0('Below\nthreshold\nin ', yrsel, '?')
    }
  }

  # get monthly average range if not complete year
  if (!fullyr) {
    mos <- mosel
    if (length(mosel) == 2) {
      mos <- seq(mosel[1], mosel[2])
    }

    # tomap
    tomap <- tomap[['mos']] %>%
      dplyr::filter(yr %in% yrsel) %>%
      dplyr::filter(mo %in% !!mos) %>%
      dplyr::filter(grepl(paste0('_', param, '$'), var)) %>%
      dplyr::group_by(bay_segment, station, yr) %>%
      dplyr::summarize(val = mean(val, na.rm = TRUE), .groups = 'drop')

    molabs <- paste(month.abb[mosel], collapse = '-')

    # legend label
    leglab <- dplyr::case_when(
      param == 'chla' ~ "Chl-a ~ (mu * g%.% L^-1)",
      param == 'la' ~ "Light ~ Att. ~(m^-1)"
    )
  }

  # lat/long
  locs <- chldat %>%
    select(station, bay_segment, Latitude, Longitude) %>%
    summarise(
      Latitude = mean(Latitude, na.rm = TRUE),
      Longitude = mean(Longitude, na.rm = TRUE),
      .by = c('station', 'bay_segment')
    )

  # add station lat/lon
  tomap <- tomap %>%
    dplyr::left_join(locs, by = c('station', 'bay_segment')) %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

  # segment labels
  seglabs <- data.frame(
    Longitude = c(-82.7, -82.64, -82.58, -82.42, -82.73, -82.5, -82.53),
    Latitude = c(27.54, 27.81, 28, 27.925, 27.7, 27.5, 27.57),
    bay_segment = c('LTB', 'MTB', 'OTB', 'HB', 'BCBS', 'MR', 'TCB')
  ) %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

  transcol <- rgb(1, 1, 1, 0.5)

  if (!requireNamespace('ggspatial', quietly = TRUE)) {
    stop(
      "Package \"ggspatial\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  if (!requireNamespace('ggrepel', quietly = TRUE)) {
    stop(
      "Package \"ggrepel\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  p <- ggplot() +
    annotation_map_tile(
      zoom = 11,
      type = 'cartolight',
      cachedir = system.file("rosm.cache", package = "ggspatial"),
      progress = 'none'
    ) +
    annotation_scale(location = 'bl', text_cex = 1.5, ) +
    geom_sf(data = tbseglines, colour = 'black', inherit.aes = F, size = 1) +
    geom_text_repel(
      data = tomap,
      aes(label = round(val, 1), geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      inherit.aes = F
    ) +
    geom_label(
      data = seglabs,
      aes(label = bay_segment, geometry = geometry),
      stat = "sf_coordinates",
      inherit.aes = F,
      fill = transcol
    )

  if (fullyr) {
    # plot, this kills the message about coordinate systems
    suppressMessages({
      p <- p +
        geom_sf(
          data = tomap,
          aes(colour = met, fill = met),
          colour = 'black',
          inherit.aes = F,
          size = 3,
          pch = 21
        ) +
        scale_fill_manual(leglab, values = c('#2DC938', '#CC3231'), drop = F) +
        scale_colour_manual(
          leglab,
          values = c('#2DC938', '#CC3231'),
          drop = F
        ) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 7),
          legend.position = c(0.85, 0.3),
          legend.background = element_blank()
        ) +
        guides(
          colour = guide_legend(
            override.aes = list(colour = c('#2DC938', '#CC3231'))
          ),
          fill = guide_legend(
            override.aes = list(colour = c('#2DC938', '#CC3231'))
          )
        )
    })

    if (partialyr) {
      leglab <- paste0(leglab, '*')
      p <- p +
        labs(
          caption = paste0(
            '*Incomplete data for ',
            yrsel,
            ' estimated by five year average'
          )
        )
    }
  }

  if (!fullyr) {
    # plot, this kills the message about coordinate systems
    suppressMessages({
      p <- p +
        geom_sf(
          data = tomap,
          aes(fill = val),
          colour = 'black',
          inherit.aes = F,
          size = 3,
          pch = 21
        ) +
        scale_fill_gradient(
          parse(text = leglab),
          low = '#2DC938',
          high = '#CC3231'
        ) + #green, red
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 7),
          legend.background = element_blank()
        ) +
        labs(subtitle = paste(molabs, yrsel))
    })
  }

  suppressWarnings(print(p))
}

show_raboxplot <- function(
  chldat,
  param = c('chla', 'la'),
  yrsel = NULL,
  yrrng = c(1975, 2022),
  ptsz = 0.5,
  bay_segment = c('OTB', 'HB', 'MTB', 'LTB', 'BCBS', 'TCB', 'MR'),
  trgs = NULL,
  labelexp = TRUE,
  txtlab = TRUE,
  partialyr = FALSE
) {
  # parameter
  param <- match.arg(param)

  # segment
  bay_segment <- match.arg(bay_segment)

  # default targets from data file
  if (is.null(trgs)) {
    trgs <- targets
  }

  # select curyr as max of yrrng if null
  if (is.null(yrsel)) {
    yrsel <- max(yrrng)
  }

  # monthly averages
  aves <- anlz_raavedat(chldat, partialyr = partialyr) %>%
    .$'mos' %>%
    dplyr::filter(var %in% !!paste0('mean_', param)) %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::mutate(
      var = 'yval',
      mo = month(mo, label = T)
    )

  # create month labels for x axis, asterisks if partialyr is true
  if (partialyr) {
    # missing months of selected year
    mismo <- chldat %>%
      filter(yr == !!yrsel) %>%
      anlz_raavedat(partialyr = FALSE) %>%
      .[['mos']] %>%
      dplyr::select(mo) %>%
      unique() %>%
      pull(mo) %>%
      setdiff(1:12, .)
    molab <- levels(aves$mo)
    molab[mismo] <- paste0(molab[mismo], '*')
  }

  # yrrng must be in ascending order
  if (yrrng[1] >= yrrng[2]) {
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2018)')
  }

  # yrrng not in chldat
  if (any(!yrrng %in% aves$yr)) {
    yrrng[1] <- min(aves$yr, na.rm = TRUE)
  }

  # yrsel not in chldat
  if (!yrsel %in% chldat$yr) {
    stop(paste(
      'Check yrsel is within',
      paste(range(chldat$yr, na.rm = TRUE), collapse = '-')
    ))
  }

  # get lines to plot
  thrnum <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(!!paste0(param, '_thresh'))

  # axis label
  if (labelexp) {
    axlab <- ifelse(
      param == 'chla',
      expression("Mean Annual Chlorophyll-a (" ~ mu * "g\u00B7L"^-1 * ")"),
      ifelse(
        param == 'la',
        expression("Mean Annual Light Attenuation (m  "^-1 * ")"),
        NA
      )
    )
  }
  if (!labelexp) {
    axlab <- dplyr::case_when(
      param == 'chla' ~ "Mean Annual Chlorophyll-a (ug/L)",
      param == 'la' ~ "Mean Annual Light Attenuation (m-1)"
    )
  }

  # parameshold label
  if (labelexp) {
    trglab <- dplyr::case_when(
      param == 'chla' ~ paste(thrnum, "~ mu * g%.%L^{-1}"),
      param == 'la' ~ paste(thrnum, "~m", "^{-1}")
    )
  }
  if (!labelexp) {
    trglab <- dplyr::case_when(
      param == 'chla' ~ paste(thrnum, "ug/L"),
      param == 'la' ~ paste(thrnum, "m-1")
    )
  }

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)

  # toplo1 is all but current year
  toplo1 <- aves %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(!yr %in% yrsel)

  # toplo2 is current year
  toplo2 <- aves %>%
    dplyr::filter(yr %in% yrsel)

  # colors and legend names
  cols <- c("black", "red")
  names(cols) <- c('prior years', as.character(yrsel))

  p <- ggplot() +
    geom_boxplot(
      data = toplo1,
      aes(x = mo, y = val, colour = names(cols)[1]),
      outlier.colour = NA
    ) +
    geom_point(
      data = toplo1,
      aes(x = mo, y = val, group = yr, colour = names(cols)[1]),
      position = position_jitter(width = 0.2),
      size = ptsz
    ) +
    geom_point(
      data = toplo2,
      aes(x = mo, y = val, group = yr, fill = names(cols)[2]),
      pch = 21,
      color = cols[2],
      size = 3,
      alpha = 0.7
    ) +
    geom_hline(
      aes(yintercept = thrnum, linetype = '+2 se (large exceedance)'),
      colour = 'blue'
    ) +
    labs(y = axlab, title = ttl) +
    theme(
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = '#ECECEC'),
      legend.position = 'top', #c(0.85, 0.95),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = '#ECECEC'),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
    ) +
    scale_colour_manual(values = cols[1]) +
    scale_fill_manual(values = cols[2]) +
    scale_linetype_manual(values = 'dotted') +
    guides(linetype = guide_legend(override.aes = list(colour = 'blue')))

  if (txtlab) {
    p <- p +
      geom_text(
        aes(x = factor('Jan'), max(aves$val)),
        parse = labelexp,
        label = trglab,
        hjust = 0.2,
        vjust = 1,
        colour = 'blue'
      )
  }

  if (partialyr) {
    p <- p +
      scale_x_discrete(labels = molab) +
      labs(
        caption = paste0(
          '*Missing data estimated by five year average from ',
          yrsel
        )
      )
  }

  return(p)
}

anlz_raavedatsite <- function(chldat, partialyr = FALSE) {
  # year month averages
  # long format, separate bay_segment for MTB into sub segs
  # mtb year month averages are weighted
  moout <- chldat %>%
    dplyr::select(yr, mo, bay_segment, station, chla) %>%
    tidyr::gather('var', 'val', chla) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, station, yr, mo, var) %>%
    dplyr::summarise(val = mean(val)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, mo, bay_segment)

  # add partial year
  if (partialyr) {
    # years to averge, last five complete
    maxyr <- max(moout$yr)
    yrfl <- c(maxyr - 5, maxyr - 1)

    # months to fill
    mofl <- moout %>%
      dplyr::filter(yr %in% maxyr) %>%
      dplyr::pull(mo) %>%
      unique %>%
      setdiff(1:12, .)

    # month averages
    moave <- moout %>%
      dplyr::filter(yr >= yrfl[1] & yr <= yrfl[2]) %>%
      dplyr::group_by(bay_segment, station, mo, var) %>%
      summarise(val = mean(val, na.rm = TRUE)) %>%
      dplyr::filter(mo %in% mofl) %>%
      dplyr::mutate(yr = maxyr)

    # join missing months to
    moout <- moout %>%
      dplyr::bind_rows(moave) %>%
      dplyr::arrange(var, yr, mo, bay_segment)
  }

  # annual data
  anout <- moout %>%
    dplyr::group_by(yr, bay_segment, station, var) %>%
    dplyr::summarise(val = mean(val)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        TRUE ~ var
      )
    ) %>%
    tidyr::spread('var', 'val') %>%
    tidyr::gather('var', 'val', mean_chla) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, bay_segment)

  # mo dat to light attenuation
  moout <- moout %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        T ~ var
      )
    )

  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )

  return(out)
}

show_rawqmatrix <- function(
  epcdata,
  param = c('chla', 'la'),
  txtsz = 3,
  trgs = NULL,
  yrrng = c(1975, 2022),
  bay_segment = c('OTB', 'HB', 'MTB', 'LTB', 'RALTB'),
  asreact = FALSE,
  nrows = 10,
  abbrev = FALSE,
  plotly = FALSE,
  partialyr = FALSE,
  width = NULL,
  height = NULL
) {
  # sanity checks
  param <- match.arg(param)

  # default targets from data file
  if (is.null(trgs)) {
    trgs <- targets
  }

  # process data to plot
  avedat <- anlz_avedat(epcdata, partialyr = partialyr) %>%
    .$ann
  toplo <- avedat %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::filter(var %in% !!paste0('mean_', param)) %>%
    dplyr::filter(!(bay_segment %in% c('RALTB') & yr < 1991)) %>%
    dplyr::left_join(trgs, by = 'bay_segment') %>%
    dplyr::select(
      bay_segment,
      yr,
      var,
      val,
      thresh = !!paste0(param, '_thresh')
    ) %>%
    dplyr::mutate(
      bay_segment = factor(
        bay_segment,
        levels = c('OTB', 'HB', 'MTB', 'LTB', 'RALTB')
      ),
      outcome = dplyr::case_when(
        val < thresh ~ 'green',
        val >= thresh ~ 'red'
      )
    )

  # make raltb same as ltb, this is how its done for ra
  toploraltb <- toplo |>
    dplyr::filter(bay_segment == 'LTB') |>
    dplyr::filter(
      yr >= min(toplo |> filter(bay_segment == 'RALTB') |> pull(yr))
    ) |>
    dplyr::mutate(bay_segment = factor('RALTB'))
  toplo <- toplo |>
    dplyr::filter(bay_segment != 'RALTB') |>
    dplyr::bind_rows(toploraltb)

  if (abbrev) {
    toplo <- toplo %>%
      dplyr::mutate(
        outcometxt = dplyr::case_when(
          outcome == 'red' ~ 'R',
          outcome == 'green' ~ 'G'
        )
      )
  }
  if (!abbrev) {
    toplo <- toplo %>%
      dplyr::mutate(
        outcometxt = outcome
      )
  }

  # reactable object
  if (asreact) {
    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)

    colfun <- function(x) {
      out <- dplyr::case_when(
        x %in% c('R', 'red') ~ '#FF3333',
        x %in% c('G', 'green') ~ '#33FF3B'
      )

      return(out)
    }

    # make reactable
    out <- show_reactable(totab, colfun, nrows = nrows, txtsz = txtsz)

    return(out)
  }

  # add descriptive labels, Result
  lbs <- dplyr::tibble(
    outcome = c('red', 'green'),
    Result = c('Above', 'Below')
  )
  if (param == 'chla') {
    rndval <- 1
  }
  if (param == 'la') {
    rndval <- 2
  }
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    dplyr::mutate(
      val = paste0('Average: ', round(val, rndval)),
      thresh = paste0('Threshold: ', round(thresh, rndval))
    ) %>%
    tidyr::unite(segval, c('val', 'thresh'), sep = ', ') %>%
    dplyr::mutate(
      segval = paste0('(', segval, ')')
    ) %>%
    unite(Result, c('Result', 'segval'), sep = ' ')

  # ggplot
  p <- ggplot(toplo, aes(x = bay_segment, y = yr, fill = outcome)) +
    geom_tile(aes(group = Result), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = '#CC3231', green = '#2DC938')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )

  if (!is.null(txtsz)) {
    p <- p +
      geom_text(aes(label = outcometxt), size = txtsz)
  }

  if (partialyr) {
    p <- p +
      labs(
        caption = paste0(
          '*Incomplete data for ',
          max(yrrng),
          ' estimated\nby five year average'
        )
      )
  }

  if (plotly) {
    p <- show_matrixplotly(
      p,
      tooltip = 'Result',
      width = width,
      height = height
    )
  }

  return(p)
}

# get rdata from github
# try simple load, download if fail
rdataload <- function(flurl) {
  fl <- basename(flurl)
  obj <- gsub('\\.RData$', '', fl)

  # try simple load
  ld <- try(load(url(flurl)), silent = T)

  # return x if load worked
  if (!inherits(ld, 'try-error')) {
    out <- get(obj)
  }

  # download x if load failed
  if (inherits(ld, 'try-error')) {
    fl <- paste(tempdir(), fl, sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(obj)
    suppressMessages(file.remove(fl))
  }

  return(out)
}
