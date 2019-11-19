bs_var_types <- c("all", "num", "fct_like", "fct", "fct_like_2_lvls", "non_fct", "chr", "lgl", NA_character_)

title_var_1   <- "Variable \n(pick one)"
title_var_n   <- "Variable \n(pick one or several)"
title_var_0_n <- "Variable \n(pick one, several or none)"

title_gr_1   <- "Groups variable \n(pick one)"
title_gr_n   <- "Groups variable \n(pick one or several)"
title_gr_0_n <- "Groups variable \n(pick one, several or none)"



tip_multiple_ctrl_letters  = str_c(
  "Hold 'Ctrl' key and left-click mouse\n",
  "to deselect or select several objects.\n",
  "Use letters on the keyboard to navigate \n",
  "quicker")

tip_single_ctrl_letters  = str_c(
  "Use letters on the keyboard to navigate quicker. \n",
  "Hold 'Ctrl' key and left-click mouse to deselect \n",
  "an object.")

tip_variable_types <- str_c(
  "Types of variables: \n",
  "   <int> whole numbers (integers);\n",
  '   <dbl>, <num> real numbers ("doubles");\n',
  "   <chr>, <char> character (text) variables;\n",
  "   <fct>, <fctr> factors (categorical variables);\n",
  # "   <ord> ordinal factors;\n",
  "   <lgl>, <lgcl>, <logi> logical values.\n",
  "Other types are also possible.\n",
  "Backticks (` `) - indicate non-standard names."
)

tip_header <- "Header is the first row if it contains column names."

# str_c(
#     "Use letters on the keyboard to navigate quicker. \n",
#     "Hold 'Ctrl' key and left-click mouse to \n",
#     "deselect an object.")
