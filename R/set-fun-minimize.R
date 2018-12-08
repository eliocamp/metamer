
set_minimize <- function(object, minimize) {
  attr(object, "call.args")$minimize <- minimize
  object
}

get_minimize <- function(object) {
  attr(object, "call.args")$minimize
}

set_preserve <- function(object, preserve) {
  attr(object, "call.args")$preserve <- preserve
  object
}

get_preserve <- function(object) {
  attr(object, "call.args")$preserve
}
