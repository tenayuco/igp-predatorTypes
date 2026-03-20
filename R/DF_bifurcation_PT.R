#' Function that creates the database of biocontrol. As it works on back and forth simulations it takes a lot of time
#' @param igp_combination the general parameters of all models
#' @param igp_times the integration times used by the solver (ODE)
#' @param pred_comb the combinations of predator types (LB.LB, PB.LB ...)
#' @param s_vec the vector of initial conditions for the IGP symmetry s
#' @param par_list the parameter list that will be swipped
#' @return 2 data frames. stored in a subfolder with the values of the parameters. One is raw, will all the values after 2000 steps. The other is averaged if the final value did not converged
#' @uses bif_backFor() function from bifurcationF.R
#' @examples

bifurcation_databaser <- function(igp_combinations, igp_times, pred_comb, bif_par_list, cond_par_list) {
  
 
    DF_COMB <- data.frame()

    ##this loops creates a data frame with the different conditions of s, and of ip
    for (comb in pred_comb) {
      chosenComb <- comb
      ipMax <- igp_combinations[[chosenComb]]$i_max[["maxIp"]]
      inMax <- igp_combinations[[chosenComb]]$i_max[["maxIn"]]

      if (is.na(ipMax)) {
        Ip_seq = 0
      } else {
        Ip_seq = signif(ipMax * c(0.1, 0.9), 2)
      } ###now we will remove high and low values
      if (is.na(inMax)) {
        In_seq = 0
      } else {
        In_seq = signif(inMax * c(0.1, 0.9), 2)
      }

      DF_I <- expand.grid(Ip = Ip_seq, In = In_seq, condPar = cond_par_list[["values"]])
      
      

      DF_I$combPred <- chosenComb
      DF_COMB <- rbind(DF_COMB, DF_I)
    }

    ###this one runs the simulation


    DF_BIFURCATION <- data.frame()

    for (i in seq(1, dim(DF_COMB)[1])) {
      chosenComb <- DF_COMB$combPred[i]
      Ip_var = DF_COMB$Ip[i]
      In_var = DF_COMB$In[i]
      par_var = DF_COMB$condPar[i]
      par_name <- cond_par_list[["name"]]

      print(chosenComb)

      #eqution characteristic
      chosenParms = igp_combinations[[chosenComb]]$igp_parms
      chosenInit = igp_combinations[[chosenComb]]$igp_init
      chosenModel = igp_combinations[[chosenComb]]$igp_model
      chosenParms[[par_name]] <- par_var
      chosenParms[["Ip"]] <- Ip_var
      chosenParms[["In"]] <- In_var

      DF_temp <- bif_backFor(
        model = chosenModel,
        minPar = bif_par_list[["min"]],
        maxPar = bif_par_list[["max"]],
        parSw = bif_par_list[["name"]],
        resolution = bif_par_list[["res"]],
        mod_parameters = chosenParms,
        mod_times = igp_times,
        mod_init = chosenInit,
        estCr = 1e-6
      )

      

#here it is just to put the hoverfly case together, iwthout detail from larval and adult, tom kae it comparable. 
   
      if ("Pl" %in% names(DF_temp)) {
        DF_temp$P <- DF_temp$Pl + DF_temp$Pa
      }
    DF_temp <- DF_temp %>%
        dplyr::select(-any_of(c("Pl", "Pa")))

### here I add the column Pl and Pa as 

      

      DF_temp <- merge(DF_COMB[i, ], DF_temp)

      DF_BIFURCATION <- rbind(DF_BIFURCATION, DF_temp)
    }
    names(DF_BIFURCATION)[which(names(DF_BIFURCATION)=="condPar")]<- par_name
    return(DF_BIFURCATION)
}

