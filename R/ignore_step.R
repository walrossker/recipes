#' Ignore step(s) in an existing recipe
#'
#' Remove recipe steps by function/id name or by step number.
#'
#' @param recipe A recipe object. The step will be removed from the sequence of
#' operations for this recipe.
#' @param ignore Character name or integer identifying one or more steps to
#' remove. There are three options for character arguments: 1. step name (e.g.,
#' "normalize" without the prefix "step_"), 2. step id code (e.g., a custom id
#' name originally supplied to the `id` argument when the step was defined OR
#' the automatically generated code that is appended to the step name by
#' default), or 3. a full id or combination of both 1 and 2 separated by an
#' underscore. This last option describes the "full ids" automatically generated
#' by `step_` functions when nothing is supplied to the `id` argument.
#'
#' @details This function is useful when different models have different
#' pre-processing requirements since it enables ad hoc step removal from a
#' baseline recipe (e.g., a recipe that satisfies the pre-processing
#' preference for most but not all models).
#'
#' @return An updated version of `recipe` with all steps named `ignore` removed.
#'
#' @export
#'
#' @examples
#' rec <- recipe(Species ~ ., data = iris) %>%
#'   step_rm(Petal.Width, id = "rm_UDLut") %>% # example of a default id format
#'   step_rm(starts_with("Sepal"), id = "custom_id")
#'
#' ignore_step(rec, "rm")              # ignores all 'rm' steps
#' ignore_step(rec, "custom_id")       # ignores single step with matching id
#' ignore_step(rec, rec$steps[[1]]$id) # ignores the first step with its full id
#' ignore_step(rec, 1)                 # ignores the first step by integer index
ignore_step <- function(recipe, ignore){

  # Ensure arguments classes are valid
  stopifnot(`'recipe' arg must have class 'recipe'` = class(recipe) == "recipe",
            `'ignore' arg must be character or numeric` =
              is.character(ignore) | is.numeric(ignore))
  if(is.numeric(ignore)){
    stopifnot(`'ignore' arg must be an integer` = ignore %% 1 == 0)
  }

  # Remove duplicates
  ignore <- unique(ignore)

  # Avoid notes about visible bindings during R CMD check
  full_id <- id_code <- name <- step_number <- NULL

  if(is.character(ignore)){

    # Create tbl with the following columns:
    # 1. step_number  = numeric order in which each step is performed
    # 2. name         = name of function being performed in the step
    # 3. id_code      = step id (e.g., just the automatically generated string)
    # 4. full_id      = either the string supplied in step definition to arg `id`
    #                   OR step name and automatically generated id separated by
    #                   an underscore
    ids <- recipe$steps %>%
      purrr::map_dfr(function(step){
        tibble::tibble(
          full_id = step[["id"]],
          id_code = strsplit(full_id, "_(?=[^_]+$)", perl = TRUE) %>%
            purrr::map_chr(~ .x[length(.x)]),
          name = attributes(step)$class %>%
            setdiff("step") %>%
            gsub("^step_", "", .))
      }, .id = "step_number") %>%
      dplyr::mutate(step_number = as.integer(step_number))

    # Identify numbers of the steps that should be removed
    steps_to_remove <- ids %>%
      dplyr::filter(dplyr::if_any(c(full_id, id_code, name), ~ .x %in% ignore))

    # Check that all arguments match with at least one step
    ids_vec <- ids %>%
      select(-step_number) %>%
      unlist()
    if(!all(ignore %in% ids_vec)){
      stop("Not all strings supplied to `ignore` match with steps")
    }

    # Remove steps (when ignore is character)
    recipe$steps <- recipe$steps[-steps_to_remove$step_number]

  } else {

    # Check that all arguments match with one step
    if(any(!ignore %in% 1:length(recipe$steps))){
      stop("Not all integers supplied to `ignore` are valid step positions.")
    }

    # Remove steps (when ignore is integer)
    recipe$steps <- recipe$steps[-ignore]

  }

  recipe

}
