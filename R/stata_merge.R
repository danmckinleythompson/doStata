#' Run merges like you would in Stata 
#'
#' This function merges dataframes the same way it's done in
#' Stata. As in Stata, the user must define which of the
#' dataframes is unique on the merge variables. The default 
#' function returns the merged dataframe with a variable that 
#' indicates which of the original dataframes each observation
#' appears in.
#' @param master the primary dataframe
#' @param using the dataframe being merged into the master dataframe
#' @param by_vars a vector of variables on which the merge is being conducted
#' @param merge_type the type of Stata merge you want to run. Options 
#' that parse are "1:1", "m:1", and "1:m" where the choice for 
#' the master dataframe appears before the colon, the choice for the using 
#' dataframe appears after the colon, "1" means that dataframe is unique on 
#' the merging variables, and "m" means that dataframe is not unique on the 
#' merging variables.
#' @param merge_var a name for the merge indicator that is added to the new 
#' dataframe. Defaults to "merge". 
#' @param gen controls whether new dataframe contains the merge indicator. 
#' Defaults to TRUE. 
#' @param keep_cases a vector that contains the types of observations to 
#' keep in the new dataframe. 1 captures those only in master, 2 captures those
#' only in using, 3 captures those observations found in both dataframes.
#' @keywords join, merge
#' @export
#' @examples
#' data1 <- data.frame(a=1:10, b=rnorm())
#' data2 <- data.frame(a=1:10, c=rnorm())
#' stata_merge(data1, data2, "a")


#
stata_merge <- function(master, using, by_vars, merge_type="1:1", 
	merge_var="merge", gen=TRUE, keep_cases=c(1,2,3)){
	
	#
	require(tidyverse)

	# Check that the merge type is valid
	if(!(merge_type %in% c("1:1", "1:m", "m:1"))){
		warning('merge_type invalid')
		stop()
	}

	# Check that master data is conformable with the merge_type
	if(substr(merge_type,1,1)=="1"){
		if(count(unique(select_(master, .dots=by_vars)))!=count(master)){
			warning('master dataframe not unique')
			stop()
		} 
	}

	# Check that using data is conformable with the merge_type
	if(substr(merge_type,3,3)=="1"){
		if(count(unique(select_(using, .dots=by_vars)))!=count(using)){
			warning('using dataframe not unique')
			stop()
		}
	}

	#
	out = mutate(master, in_master=1) %>%
		full_join(mutate(using, in_using=1), by=c("candidate")) %>%
		mutate(
			in_using = ifelse(!is.na(in_using), 1, 0),
			in_master = ifelse(!is.na(in_master), 1, 0)
		)

	#
	out[, merge_var] = ifelse(out$in_using==1 & out$in_master==1, 3, 
		ifelse(out$in_using==1 & out$in_master==0, 2, 1))

	#
	out = out[out[, merge_var][[1]] %in% keep_cases, ]
	
	#
	if(!gen) out = out[, colnames(out)!=merge_var]
	out = select(out, -c(in_using, in_master))

	#
	return(out)
}