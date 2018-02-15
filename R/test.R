visualize_byx_cnt <-
  function(data = NULL,
           x_char = NULL,
           num_top = 20,
           color_char,
           facet_char,
           color = "grey50") {

  }


visualize_byx_cnt <-
  function(data = NULL,
           x_char = NULL,
           num_top = 20,
           color_char,
           facet_char,
           color = "grey50") {
    # if (is.null(data)) {
    #   stop("`data` cannot be NULL.", call. = FALSE)
    # }
    # if (is.null(x_char)) {
    #   stop("`x_char` cannot be NULL.", call. = FALSE)
    # }

    # x_quo <- rlang::sym(x_char)
    # fill_quo <- rlang::sym(color_char)
    # if(!missing(facet_char)) {
    #   data_proc <-
    #     data %>%
    #     dplyr::count(!!x_quo, sort = TRUE) %>%
    #     dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    #     dplyr::mutate(!!x_quo := reorder(!!x_quo, n)) %>%
    #     dplyr::ungroup()
    # } else {
    #   facet_quo <- rlang::sym(facet_char)
    #   data_proc <-
    #     data %>%
    #     dplyr::count(!!facet_quo, !!x_quo, sort = TRUE) %>%
    #     dplyr::group_by(!!facet_quo) %>%
    #     dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    #     # dplyr::mutate(!!x_quo := drlib::reorder_within(!!x_quo, n, !!facet_quo)) %>%
    #     dplyr::mutate(!!x_quo := drlib::reorder_within(word, n, name)) %>%
    #     dplyr::ungroup()
    # }
    # viz <-
    #   data_proc %>%
    #   ggplot2::ggplot(ggplot2::aes_string(x = x_char, y = "n")) +
    #   ggalt::geom_lollipop(size = 2, point.size = 4)
    #
    # if (!missing(facet_char)) {
    #   viz <-
    #     viz +
    #     ggalt::geom_lollipop(ggplot2::aes_string(color = color_char), size = 2, point.size = 4) +
    #     ggplot2::scale_color_manual(values = color) +
    #     drlib::scale_x_reordered() +
    #     ggplot2::facet_wrap(as.formula(paste0("~", facet_char)), scales = "free") +
    #     temisc::theme_te_b_facet() +
    #     ggplot2::labs(subtitle = paste0("By ", stringr::str_to_title(facet_char)))
    # } else {
    #   viz <-
    #     viz +
    #     ggplot2::scale_color_manual(values = "grey80") +
    #     temisc::theme_te_b()
    # }
    # viz <-
    #   viz +
    #   ggplot2::coord_flip() +
    #   ggplot2::labs(x = NULL, y = NULL) +
    #   ggplot2::labs(title = "Count of Words") +
    #   ggplot2::theme(legend.position = "none") +
    #   ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    # viz
  }
visualize_ngrams_byx_corrs <-
  function(data,
           colname_size_point = "n",
           color_point = "grey50",
           shape_point = 21,
           colname_label,
           lab_feature,
           seed = 42) {
    set.seed(seed)
    viz <-
      data %>%
      ggraph::ggraph(layout = "fr") +
      ggraph::geom_edge_link(edge_width = 1)

    if (!misssing(colname_size_point)) {
      viz <-
        viz +
        ggraph::geom_node_point(
          ggplot2::aes_string(size = colname_size_point),
          fill = color_point,
          shape = shape_point
        )
    } else {
      viz <-
        viz +
        ggraph::geom_node_point(fill = color_point, shape = shape_point)
    }

    if (!misssing(colname_size_point)) {
      viz <-
        viz +
        ggraph::geom_node_text(ggplot2::aes_string(label = colname_label), repel = TRUE)
    }

    viz <-
      viz +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(title = "Network of Pairwise Correlations")

    if (!missing(lab_feature)) {
      lab_subtitle <- paste0("within Single ", lab_feature)
      viz <- viz = labs(subtitle = lab_subtitle)
    }
    viz
  }
