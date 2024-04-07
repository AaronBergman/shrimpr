#' Regression Summary to DataFrame
#'
#' Takes a linear model object and returns a data frame summarizing the regression analysis,
#' including coefficients, standard errors, t values, p-values, and significance levels.
#' @param lm A linear model object.
#' @param thin A logical value indicating whether to return a thinner summary table. 
#' Default is TRUE.
#' @return A tibble summarizing the regression coefficients and statistics.
#' @export
#' @importFrom dplyr mutate across rename select if_else case_when
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
reg_df=function(lm,thin=T){sub=data.frame(summary(lm)$coefficients)%>%
  mutate(variable=rownames(.))%>%as_tibble%>%
  rename(estimate=Estimate,st_error=Std..Error,t_val=t.value,`p(>|t|)`=`Pr...t..`)%>%
  mutate(across(is.numeric,~signif(.x,3)))%>%
  mutate(data=as.character(summary(lm)$call)[3],
         model=name(summary(lm)$call)[1],
         r2=signif(summary(lm)$r.squared[1],3),
         regression=as.character(summary(lm)$call)[2],
         `p(>|t|)`=if_else(`p(>|t|)`<(10^-40),0,`p(>|t|)`),
         significance=case_when(`p(>|t|)`>=.05~"p>.05",`p(>|t|)`<.001~"p<.001",`p(>|t|)`>=.01&`p(>|t|)`<.05~'.01<p<.05',`p(>|t|)`>=.001&`p(>|t|)`<.01~".001<p<.01"),
         estimate=signif(estimate,3),t_val=round(t_val,3),r2=round(r2,3))%>%
  select(c(6,9,5,1,10,3,4,8))
if (thin==T){select(sub,-c(1,7))} 
else{sub}  }

#' Generate Equation String from Linear Model
#'
#' Generates a character string representing the equation of the linear model.
#' @param lm A linear model object.
#' @return A character string of the model's equation.
#' @export
OLSeq=function(lm){
  coefs_tib=named2tib(lm[[1]])
  terms=coefs_tib%>%mutate(str=paste0(signif(values,5),names))%>%
    mutate(str=str_replace_all(str,"\\(Intercept\\)",""))%>%
    .$str
  rhs=paste0(terms,collapse=" + ")
  outcomeVar=coefs_tib$call%>%as.character()%>%.[2]%>%str_split(' ~')%>%unlist%>%.[1]
  paste0(outcomeVar,' = ',rhs)
}

#' Regression Summary with Equation and Extremes
#'
#' Provides a summary of the regression, including the equation, minimum and maximum t values,
#' and estimates.
#' @param lm A linear model object.
#' @return A data frame summarizing key regression outputs and statistics.
#' @export
regDF=function(lm){
  sub=reg_df(lm)
  estimates=list(sub$estimate)#,collapse=" ")
  tvals=list(sub$t_val)#),collapse=" ")
  eq=OLSeq(lm)
  sub%>%select(-c(2:5))%>%.[1,]%>%
    mutate(
      output=eq,
      abstMin=min(abs(unlist(tvals))),
      abs_tMax=max(abs(unlist(tvals))),
      estimates=estimates,
      tvals=tvals)%>%
    mutate(output=str_replace(output,'NA',
                              str_extract(regression,'\\w+')))
}
