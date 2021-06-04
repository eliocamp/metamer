
new_metamer_list <- function(list, history, preserve, minimize,
                             change,
                             signif,
                             preserve_original,
                             annealing,
                             perturbation,
                             keep = length(list),
                             name,
                             K,
                             start_probability) {
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
            perturbation = perturbation,
            name = name,
            K = K,
            start_probability = start_probability)
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
                   K = attr(new, "K"),
                   start_probability  = attr(new, "start_probability"),
                   perturbation = attr(new, "perturbation"),
                   name = c(attr(old, "name"), attr(new, "name")))
}


.get_attr <- function(data, argument, call) {
  call <- as.list(call)[-1]

  if (argument %in% names(call)) {
    eval(call[[argument]])
  } else {
    attr(data, argument)
  }
}
