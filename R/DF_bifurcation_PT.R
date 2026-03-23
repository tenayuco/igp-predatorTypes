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



simple_ass_coex <- function(simpleDF, ncrit = 6) {
  ### this is just if you have a lot of points (not convergent)
  DF_NORM <- simpleDF |>
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "P")))) |>
    dplyr::summarise_all(mean)

  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS

  ## we add the equilibri
  DF_NORM$EQR <- ""
  DF_NORM$EQN <- ""
  DF_NORM$EQP <- ""

  ###chacun a son critere de maximumm...
  DF_NORM$EQR[round(DF_NORM$R / max(DF_NORM$R), ncrit) > 0] <- "R"
  DF_NORM$EQN[round(DF_NORM$Nl / max(DF_NORM$Nl), ncrit) > 0] <- "N" ##here I assume no stage
  DF_NORM$EQP[round(DF_NORM$P / max(DF_NORM$P), ncrit) > 0] <- "P"

  DF_NORM <- DF_NORM |>
    tidyr::unite("EQ", EQR:EQP, sep = "", remove = T)

  DF_NORM$EQ[DF_NORM$EQ == ""] <- 0

  DF_NORM$Rnorm <- round(DF_NORM$R / max(DF_NORM$R), ncrit) #this is the max criteria for differences (1*10-3) this is, simple criteria to distinguish two ass

  DF_NORM <- DF_NORM |>
    dplyr::group_by(dplyr::across(-c(direccion, R, Nl, Na, P, Rnorm, EQ))) |> #we group by aevetyghin excepet for the
    dplyr::mutate(ASS = n_distinct(Rnorm))

  return(DF_NORM)
}


minMax_coex <- function(simpleDF, ncrit = 6) {


  DF_NORM <- simpleDF %>%
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "P")))) |>
    summarise_all(list(maxValue = max, minValue = min, meanValue = mean))

  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS

  ## we add the equilibri
  DF_NORM$EQR <- ""
  DF_NORM$EQN <- ""
  DF_NORM$EQP <- ""

  ###chacun a son critere de maximumm...BUT HERE I HAN CHEAITNG AND PUTTNG CPEXITE AONY IF THE MEAN IS GREEN (so it take the average of oscilaiotns)
  DF_NORM$EQR[
    round(DF_NORM$R_meanValue / max(DF_NORM$R_meanValue), ncrit) > 0
  ] <- "R"
  DF_NORM$EQN[
    round(DF_NORM$Nl_meanValue / max(DF_NORM$Nl_meanValue), ncrit) > 0
  ] <- "N" ##here I assume no stage
  DF_NORM$EQP[
    round(DF_NORM$P_meanValue / max(DF_NORM$P_meanValue), ncrit) > 0
  ] <- "P"

  DF_NORM <- DF_NORM %>%
    tidyr::unite("EQ", EQR:EQP, sep = "", remove = T)

  DF_NORM$EQ[DF_NORM$EQ == ""] <- 0

  DF_NORM$Rnorm <- round(DF_NORM$R_meanValue / max(DF_NORM$R_meanValue), ncrit) #this is the max criteria for differences (1*10-3)

  DF_NORM <- DF_NORM %>%
    dplyr::group_by(dplyr::across(
      -c(
        direccion,
        Rnorm,
        EQ,
        R_meanValue,
        Nl_meanValue,
        Na_meanValue,
        P_meanValue,
        R_maxValue,
        Nl_maxValue,
        Na_maxValue,
        P_maxValue,
        R_minValue,
        Nl_minValue,
        Na_minValue,
        P_minValue
      )
    )) |> #we group by aevetyghin excepet for the
    dplyr::mutate(ASS = n_distinct(Rnorm))

  ###NOW we going to save the min and max column and put them as worw (even if repetitive..)

  #done by deepseek
  DF_NORM_LONG <- DF_NORM |> 
    tidyr::pivot_longer(
      c(R_minValue,
      R_maxValue,
      Nl_minValue,
      Nl_maxValue,
      Na_minValue,
      Na_maxValue,
      P_minValue,
      P_maxValue),
      names_to =  "var_minmax",
      values_to = "value",
      
    ) |> 
    tidyr::separate(var_minmax, into = c("variable", "minMax"), sep = "_") %>%
    tidyr::spread(key = variable, value = value)

  DF_NORM_LONG <- DF_NORM_LONG %>%
   dplyr::select(type, S, direccion, K, EQ, ASS, minMax, Na, Nl, P, R)

  return(DF_NORM_LONG)
}

