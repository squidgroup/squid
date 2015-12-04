getEnvironmentInput <- function(module, step){
  
  return(
    span(
      # environment inputs
      getCheckboxInput(paste0("Mod",module,"Step",step,"_X_Shared"), Modules_VAR$share),
      getSelectInput(paste0("Mod",module,"Step",step,"_X_select"), Modules_VAR$Env_types),
      wellPanel(
        fluidRow(
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'auto' | input.Mod",module,"Step",step,"_X_select == 'ran'"),
            column(6, 
                   getNumericInput(paste0("Mod",module,"Step",step,"_X1_ran_V"), FullModel_VAR$ranV, paste0("Mod",module,"Step",step,"_error_ran_V"))
            ),
            column(6, 
                   conditionalPanel(
                     condition = paste0("input.Mod",module,"Step",step,"_X_select == 'auto'"),
                     getNumericInput(paste0("Mod",module,"Step",step,"_X1_ran_corr"), FullModel_VAR$ranCorr, paste0("Mod",module,"Step",step,"_error_ran_corr"))
                   )
            )
          ),
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'lin'"),
            column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_Intercept"), FullModel_VAR$linI, paste0("Mod",module,"Step",step,"_error_lin_Intercept"))),
            column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_Slope"), FullModel_VAR$linS, paste0("Mod",module,"Step",step,"_error_lin_Slope"))),
            conditionalPanel(
              condition = paste0("input.Mod",module,"Step",step,"_X_Shared == 0"),
              column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_V"), FullModel_VAR$ranV, paste0("Mod",module,"Step",step,"_error_lin_V")))
            )
          ),
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'cyc'"),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Amplitude"), FullModel_VAR$cycA, paste0("Mod",module,"Step",step,"_error_lin_Amplitude"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Period"), FullModel_VAR$cycP, paste0("Mod",module,"Step",step,"_error_lin_Period"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Hshift"), FullModel_VAR$cycH, paste0("Mod",module,"Step",step,"_error_lin_Hshift"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Vshift"), FullModel_VAR$cycV, paste0("Mod",module,"Step",step,"_error_lin_Vshift"))),
            conditionalPanel(
              condition = paste0("input.Mod",module,"Step",step,"_X_Shared == 0"),
              column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_V"), FullModel_VAR$ranV, paste0("Mod",module,"Step",step,"_error_cyc_V")))
            )
          )
        ),
        plotOutput(paste0("Mod",module,"Step",step,"_X1_plot"))
      )
    )
  )
  
}