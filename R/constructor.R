new_birtms <- function(x = NULL,
                       type_parametric = '1PL',
                       type_response = 'dichotom',
                       type_dimensionality = 'unidimensional',
                       type_person_hierarchy = 'pooled', type_item_hierarchy = 'pooled',
                       person_covariables = '', item_covariables = '', situation_covariables = '') {
  stopifnot(brms::is.brmsfit(x))
  stopifnot(is.character(person_covariables))
  stopifnot(is.character(item_covariables))
  stopifnot(is.character(situation_covariables))

  type = list()
  type$parametric <- match.arg(type_parametric, c('1PL', '2PL', '3PL'))
  type$response <- match.arg(type_response, c('dichotom', 'polytom'))
  type$dimensionality <- match.arg(type_dimensionality, c('unidimensional', 'bifactor', 'twotier',
                                                          'multidimensional_unnested',
                                                          'multidimensional_compensatory',
                                                          'multidimensional_noncompensatory'))
  # ergeben sich die Eigenschaften des unnested multidimensinalen Modells aus dem compensatory?
  # können hier Kategorien zusammengefasst werden?
  # soll ein Typ higherorder eingeführt werden?
  type$person_hierarchy <- match.arg(type_person_hierarchy, c('pooled', 'multilevel'))
  type$item_hierarchy <- match.arg(type_item_hierarchy, c('nonpooled', 'pooled'))

  covariables <- list(person = person_covariables, item = item_covariables, situation = situation_covariables)

  structure(x,
            class = c('birtmsfit', 'brmsfit'),
            irt_type = type,
            covariables = covariables
            )
}
