source(file.path("scripts", "00_v20_setup_safety_check.R"))

flow <- utils::read.csv(v20_path("source_data", "supplementary", "public_raw_reconstruction_flow_v20.csv"), stringsAsFactors = FALSE)
flow_fig <- flow |>
  dplyr::mutate(
    step_order = step_number,
    step = dplyr::case_when(
      step_number == 1 ~ "NHANES 2009-2014 participants",
      step_number == 10 ~ paste0("Final public raw-only analytic sample: N=", n_after, "; deaths=", sum(readRDS(file.path(RECON_DIR, "final_model4_public_raw_analytic_cohort_DO_NOT_UPLOAD.rds"))$death_allcause, na.rm = TRUE)),
      TRUE ~ exclusion_rule
    ),
    n_remaining = n_after,
    n_excluded = n_excluded,
    box_label = ifelse(step_number == 10, step, paste0(step, "\nN=", format(n_remaining, big.mark = ","))),
    excluded_label = ifelse(is.na(n_excluded) | n_excluded == 0, "", paste0("Excluded: ", format(n_excluded, big.mark = ","))),
    y = rev(seq_len(dplyr::n()))
  ) |>
  dplyr::select(step_order, step, n_remaining, n_excluded, box_label, excluded_label, y)
safe_write_csv(flow_fig, v20_path("source_data", "figures", "figure1_flowchart_source_v20.csv"))

box_xmin <- 0.12; box_xmax <- 0.72; excl_x <- 0.82
fig1 <- ggplot2::ggplot(flow_fig) +
  ggplot2::geom_rect(ggplot2::aes(xmin = box_xmin, xmax = box_xmax, ymin = y - 0.32, ymax = y + 0.32), fill = "white", color = "black", linewidth = 0.4) +
  ggplot2::geom_text(ggplot2::aes(x = (box_xmin + box_xmax) / 2, y = y, label = box_label), size = 3.0, lineheight = 0.95) +
  ggplot2::geom_segment(
    data = flow_fig |> dplyr::filter(step_order < max(step_order)),
    ggplot2::aes(x = (box_xmin + box_xmax) / 2, xend = (box_xmin + box_xmax) / 2, y = y - 0.34, yend = y - 0.66),
    arrow = ggplot2::arrow(length = grid::unit(0.10, "inches")),
    linewidth = 0.35
  ) +
  ggplot2::geom_segment(
    data = flow_fig |> dplyr::filter(excluded_label != ""),
    ggplot2::aes(x = box_xmax, xend = excl_x - 0.03, y = y, yend = y),
    linewidth = 0.25, color = "grey45"
  ) +
  ggplot2::geom_text(
    data = flow_fig |> dplyr::filter(excluded_label != ""),
    ggplot2::aes(x = excl_x, y = y, label = excluded_label),
    hjust = 0, size = 2.8, color = "grey20"
  ) +
  ggplot2::coord_cartesian(xlim = c(0.08, 1.10), ylim = c(0.4, max(flow_fig$y) + 0.6), clip = "off") +
  ggplot2::theme_void(base_size = 10) +
  ggplot2::theme(plot.margin = ggplot2::margin(10, 25, 10, 10))
ggplot2::ggsave(v20_path("figures", "main", "Figure_1_strobe_flowchart_v20.png"), fig1, width = 8, height = 8.8, dpi = 300, bg = "white")
ggplot2::ggsave(v20_path("figures", "main", "Figure_1_strobe_flowchart_v20.pdf"), fig1, width = 8, height = 8.8, bg = "white")

fig2_source <- utils::read.csv(v20_path("source_data", "figures", "figure2_forest_source_v20.csv"), stringsAsFactors = FALSE)
fig2_plot <- fig2_source |> dplyr::filter(!is.na(conf_low))
fig2 <- ggplot2::ggplot(fig2_plot, ggplot2::aes(x = HR, y = reorder(label, HR))) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = conf_low, xmax = conf_high), width = 0.18, color = "grey20", orientation = "y") +
  ggplot2::geom_point(size = 2.5, color = "black") +
  ggplot2::geom_text(ggplot2::aes(x = max(conf_high, na.rm = TRUE) * 1.25, label = side_label), hjust = 0, size = 3.0) +
  ggplot2::scale_x_log10(limits = c(0.65, max(fig2_plot$conf_high, na.rm = TRUE) * 2.5), breaks = c(0.75, 1, 1.5, 2, 3, 5)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(x = "Hazard ratio (log scale)", y = NULL, title = "Joint exposure and all-cause mortality") +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11), plot.margin = ggplot2::margin(8, 165, 8, 8))
ggplot2::ggsave(v20_path("figures", "main", "Figure_2_joint_exposure_forest_plot_v20.png"), fig2, width = 7, height = 3.6, dpi = 300, bg = "white")
ggplot2::ggsave(v20_path("figures", "main", "Figure_2_joint_exposure_forest_plot_v20.pdf"), fig2, width = 7, height = 3.6, bg = "white")

fig3_source <- utils::read.csv(v20_path("source_data", "figures", "figure3_sensitivity_source_v20.csv"), stringsAsFactors = FALSE)
fig3_plot <- fig3_source |> dplyr::filter(!is.na(HR)) |> dplyr::mutate(analysis = factor(analysis, levels = rev(analysis)))
fig3 <- ggplot2::ggplot(fig3_plot, ggplot2::aes(x = HR, y = analysis)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = conf_low, xmax = conf_high), width = 0.2, color = "grey20", orientation = "y") +
  ggplot2::geom_point(size = 2.3, color = "black") +
  ggplot2::geom_text(ggplot2::aes(label = paste0("N=", N, "; deaths=", deaths, "; ", display), x = max(conf_high, na.rm = TRUE) * 1.25), hjust = 0, size = 2.8) +
  ggplot2::scale_x_log10(limits = c(0.65, max(fig3_plot$conf_high, na.rm = TRUE) * 2.8), breaks = c(0.75, 1, 1.5, 2, 3, 5)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(x = "Hazard ratio (log scale)", y = NULL, title = "Sensitivity analyses") +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(plot.margin = ggplot2::margin(5.5, 190, 5.5, 5.5), plot.title = ggplot2::element_text(face = "bold", size = 10))
ggplot2::ggsave(v20_path("figures", "main", "Figure_3_sensitivity_forest_plot_v20.png"), fig3, width = 7, height = 4.8, dpi = 300, bg = "white")
ggplot2::ggsave(v20_path("figures", "main", "Figure_3_sensitivity_forest_plot_v20.pdf"), fig3, width = 7, height = 4.8, bg = "white")

safe_write_lines(c(
  "# v20 table and figure generation report",
  "",
  "Table 1, Table 2, and Table 3 were regenerated from v20 public raw-only results.",
  "Figure 1, Figure 2, and Figure 3 were regenerated from v20 source data.",
  "Supplementary Table S1 and S6 were regenerated from v20 public raw-only results.",
  if (file.exists(v20_path("tables", "supplementary", "Supplementary_Table_S2_PH_diagnostics_v20.csv"))) "Supplementary Table S2 was regenerated after PH diagnostics passed." else "Supplementary Table S2 was not finalized as pass-confirmed.",
  "No old frozen table or figure value was reused as a primary manuscript output."
), v20_path("reports", "v20_table_figure_generation_report.md"))

log_action("07 figures 1-3 regenerated.")
