vec_rbind_uq <- function(
    x,
    .ptype = NULL, .names_to = NULL, .name_repair =
        cc("unique", "universal", "check_unique")) {
    vctrs::vec_rbind(!!!x, .ptype = .ptype, .names_to = .names_to, .name_repair = .name_repair)
}

`%vec_in%` <- vctrs::vec_in