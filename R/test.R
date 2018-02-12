


tidy_data_unigrams <-
  function(data = NULL,
           colname_text = "text",
           colname_word = "word",
           rgx_pattern,
           rgx_replacement,
           rgx_unnest,
           rgx_ignore_custom) {
    colname_text_quo <- rlang::sym(colname_text)
    colname_word_quo <- rlang::sym(colname_word)

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        dplyr::mutate(out,
               text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }

    if (missing(rgx_unnest)) {
      out <-
        tidytext::unnest_tokens(out, !!colname_word_quo, !!colname_text_quo, token = "words")
    } else {
      out <-
        tidytext::unnest_tokens(out, !!colname_word_quo, !!colname_text_quo, token = rgx_unnest)
    }

    if (!missing(rgx_ignore_custom)) {
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word_quo, rgx_ignore_custom))
    }

    out <- dplyr::filter(out, stringr::str_detect(!!colname_word_quo, "[a-z]"))
    out
  }

tidy_data_bigrams <-
  function(data = NULL,
           colname_text = "text",
           colname_word1 = "first",
           colname_word2 = "second",
           rgx_pattern,
           rgx_replacement,
           rgx_ignore_custom) {

    colname_text_quo <- rlang::sym(colname_text)
    colname_word1_quo <- rlang::sym(colname_word1)
    colname_word2_quo <- rlang::sym(colname_word2)

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        dplyr::mutate(out,
               text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }
    out <- tidytext::unnest_tokens(out, bigram, !!colname_text_quo, token = "ngrams", n = 2)
    out <- tidyr::separate(bigram, into = c(colname_word1, colname_word2), sep = " ", remove = FALSE)
    out <- dplyr::anti_join(out, tidytext::stop_words, by = c(colname_word1 = "word"))
    out <- dplyr::anti_join(out, tidytext::stop_words, by = c(colname_word2 = "word"))
    if (!missing(rgx_ignore_custom)) {
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word1_quo, rgx_ignore_custom))
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word2_quo, rgx_ignore_custom))
    }

    out <- dplyr::filter(out, stringr::str_detect(!!colname_word1_quo, "[a-z]"))
    out <- dplyr::filter(out, stringr::str_detect(!!colname_word2_quo, "[a-z]"))
    out
  }
