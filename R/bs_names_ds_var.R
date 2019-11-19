#' Tk widget to enter dataset and variable names.
#'
#' @param parent Tcl/Tk parent window or frame
#' @param get_var_name_fun Function that gets the value of interest
#'
#' @return Object of class `bs_names_ds_var`
#' @md
#' @noRd
#' @examples
#' if (FALSE) {
#'
#' top <- tktoplevel()
#' obj <- bs_names_ds_var(get_ds_name_fun = function() {"ds"})
#' tkgrid(obj$frame)
#'
#' }

bs_names_ds_var <- function(parent = top,
  get_ds_name_fun = active_dataset_0,
  get_var_name_fun = function() {"variable"}) {

  frame <- tk2frame(parent)

  update_ds <- function() {
    .ds <- get_ds_name_fun()
    if (get_values(ds2)) {
      tk_normalize(ds1)
      set_values(ds1, .ds)
      tk_disable(ds1)

    } else {
      tk_normalize(ds1)
      set_values(ds1, unique_obj_names(.ds))
    }
  }

  update_var <- function() {
    var_name <- get_var_name_fun()
    if (get_values(var2)) {
      tk_normalize(var1)
      set_values(var1, var_name)
      tk_disable(var1)

    } else {
      tk_normalize(var1)
      set_values(var1, unique_colnames(var_name))
    }
  }

  l1 <- tk_label_blue(frame, "Name for new variable")
  ds1 <- bs_entry(frame)
  ds2 <- bs_checkboxes(frame,
    default_value = TRUE,
    boxes = "Update (overwrite)",
    default_command = update_ds
  )

  l2 <- tk_label_blue(frame, "Name for new dataset")
  var1 <- bs_entry(frame)
  var2 <- bs_checkboxes(frame,
    default_value = FALSE,
    boxes = "Update (overwrite)",
    default_command = update_var
  )

  # tkgrid(frame, sticky = "w")
  tkgrid(l1, var1$frame, var2$frame, pady = c(5, 5))
  tkgrid(l2,  ds1$frame,  ds2$frame, pady = c(0, 5))

  tkgrid(l1, l2, sticky = "w", padx = c(0, 5))
  tkgrid(var1$frame, ds1$frame, padx = c(0, 5))
  tkgrid.configure(var2$frame, ds2$frame, padx = c(0, 2))

  update_ds()
  update_var()

  structure(list(
    frame = frame,
    dataset  = list(label = l1, entry = ds1,  checkbox = ds2,  update = update_ds),
    variable = list(label = l2, entry = var1, checkbox = var2, update = update_var)
  ),
    class = c("bs_names_ds_var", "bs_tk_widget", "list")
  )
}
