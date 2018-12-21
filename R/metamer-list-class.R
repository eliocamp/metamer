
new_metamer_list <- function(list, history, preserve, minimize,
                             change,
                             signif,
                             preserve_original,
                             annealing,
                             perturbation,
                             keep = length(list)) {
  metamers <- list[seq_len(keep)]
  history <- history[seq_len(keep)]
  structure(metamers,
            class = c("metamer_list", "list"),
            preserve = preserve,
            change = change,
            signif = signif,
            preserve_original = preserve_original,
            minimize = minimize,
            annealing = annealing,
            perturbation = perturbation)
}


append_metamer <- function(old, new) {
  full <- append(as.list(old), as.list(new))
  new_metamer_list(full,
                   history = c(attr(old, "history"), attr(new, "history")),
                   preserve = attr(new, "preserve"),
                   change = attr(new, "change"),
                   signif = attr(new, "signif"),
                   preserve_original = attr(new, "preserve_original"),
                   minimize = attr(new, "minimize"),
                   annealing = attr(new, "annealing"),
                   perturbation = attr(new, "perturbation"))
}


.get_attr <- function(data, argument, call) {
  call <- as.list(call)[-1]

  if (argument %in% names(call)) {
    eval(call[[argument]])
  } else {
    attr(data, argument)
  }
}
