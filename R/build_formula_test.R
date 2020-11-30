irt_data <- readRDS('irt_data.RDS')

variable_specs <- list(response = 'resp', item ='item', person = 'person',
                       regular_dimensions = c('knowledge', 'scientific_inquiry', 'testlet'),
                       person_covariables = 'intelligence',
                       item_covariables = c('map_alpha1', 'item'),
                       situation_covariables = 'teacher',
                       dif = 'gender', person_grouping = c('class', 'school')
                       )
model_specs <- list(response_type = 'dichotom', item_parameter_number = 1)
f <- build_formula(variable_specs, model_specs)

make_stancode(f, irt_data)

