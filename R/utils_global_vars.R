if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data", ":=",
    "model_id", "cases", "mean_fit", "lci_fit", "uci_fit", 
    "group", "time", "model_id_label", 
    "lci", "uci", "variable",
    "data", "model", "final_label", "model_label",
    "ID",
    "exp_median",
    "label",
    "median",
    "observed",
    "variable_color",
    "variable_shape"
  ))
}
