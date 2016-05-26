getEnvironmentInput <- function(module, step, myDefault="Stochastic"){
  
  return(
    span(
      # environment inputs
      getCheckboxInput(paste0("Mod",module,"Step",step,"_X_Shared"), Modules_VAR$share),
      getSelectInput(paste0("Mod",module,"Step",step,"_X_select"), Modules_VAR$Env_types),
      wellPanel(
        div(info_msg(FullModel_VAR$Env_preview)),
        fluidRow(
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'auto' | input.Mod",module,"Step",step,"_X_select == 'sto'"),
            column(6, 
                   getNumericInput(paste0("Mod",module,"Step",step,"_X1_sto_V"), FullModel_VAR$stoV, paste0("Mod",module,"Step",step,"_error_sto_V"))
            ),
            column(6, 
                   conditionalPanel(
                     condition = paste0("input.Mod",module,"Step",step,"_X_select == 'auto'"),
                     getNumericInput(paste0("Mod",module,"Step",step,"_X1_sto_corr"), FullModel_VAR$stoCorr, paste0("Mod",module,"Step",step,"_error_sto_corr"))
                   )
            )
          ),
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'lin'"),
            column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_intercept"), FullModel_VAR$linI, paste0("Mod",module,"Step",step,"_error_lin_intercept"))),
            column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_slope"), FullModel_VAR$linS, paste0("Mod",module,"Step",step,"_error_lin_slope"))),
            conditionalPanel(
              condition = paste0("input.Mod",module,"Step",step,"_X_Shared == 0"),
              column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_lin_V"), FullModel_VAR$stoV, paste0("Mod",module,"Step",step,"_error_lin_V")))
            )
          ),
          conditionalPanel(
            condition = paste0("input.Mod",module,"Step",step,"_X_select == 'cyc'"),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_amplitude"), FullModel_VAR$cycA, paste0("Mod",module,"Step",step,"_error_lin_amplitude"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_period"), FullModel_VAR$cycP, paste0("Mod",module,"Step",step,"_error_lin_period"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Hshift"), FullModel_VAR$cycH, paste0("Mod",module,"Step",step,"_error_lin_Hshift"))),
            column(3, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_Vshift"), FullModel_VAR$cycV, paste0("Mod",module,"Step",step,"_error_lin_Vshift"))),
            conditionalPanel(
              condition = paste0("input.Mod",module,"Step",step,"_X_Shared == 0"),
              column(6, getNumericInput(paste0("Mod",module,"Step",step,"_X1_cyc_V"), FullModel_VAR$stoV, paste0("Mod",module,"Step",step,"_error_cyc_V")))
            )
          )
        ),
        plotOutput(paste0("Mod",module,"Step",step,"_X1_plot"))
      )
    )
  )
  
}