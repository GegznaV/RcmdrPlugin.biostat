# Examples of TEMPLATES

# Checkboxes =================================================================
# Out of date
bs_check_boxes(
  frame,
  frame         = "options_frame",
  boxes         = c("case_", "regex_"),
  initialValues = c(0, 0),
  labels        = c("Match case", "Regex"),
  commands = list("case_"  = cmd_update_list,
                  "regex_" = cmd_update_list)
)

# OK
options <- bs_checkboxes(
    parent = frame,
    boxes  = c("case"  = "Match case",
               "regex" = "Regex"),
    values = c(0, 0),
    layout = "horizontal",
    commands = list("case"  = cmd_update_list,
                    "regex" = cmd_update_list)
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~