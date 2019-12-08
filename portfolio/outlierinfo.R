#outlierInfo: fornece uma analise sobre possiveis outliers em um dado
outlierInfo <- function(dt, var, labelName = "", showMessage = FALSE, removeOutlier = FALSE) {
	varName <- eval(substitute(var), eval(dt))

    tot <- sum(!is.na(varName)) #Total de itens nao null
    na1 <- sum(is.na(varName)) #Total de itens null
    m1 <- mean(varName, na.rm = T) #Media dos itens

    #Criando grafico com os outliers
	par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
    boxplot(varName, main = "Boxplot with outliers")
    hist(varName, main = "Hist with outliers", xlab = NA, ylab = NA)
    outlier <- boxplot.stats(varName)$out

    #Criando grafico sem os outliers
    varName <- ifelse(varName %in% outlier, NA, varName)
    boxplot(varName, main = "Boxplot without outliers")
    hist(varName, main = "Hist without outliers", xlab = NA, ylab = NA)
	title(paste(labelName, "Outlier Check"), outer = TRUE)
    na2 <- sum(is.na(varName))

    #Calculando dados com e sem outliers
	outliersCount = na2 - na1
	outliersPercent = ((na2 - na1) / tot * 100)
	meanoutliers <- mean(outlier)
	meanWithOutliers = m1
    meanWithoutOutliers = mean(varName, na.rm = T)

    #Remove os outliers conforme o parametro
	if (removeOutlier) {
		dt[as.character(substitute(var))] <- invisible(var_name)
		assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
	}

    #Apresenta informacoes sobre os dados conforme o paramtro
	if (showMessage) {
		message(labelName, " observations:")
		message("Outliers identified: ", outliersCount, " from ", tot, " observations")
		message("Proportion (%) of outliers: ", outliersPercent)
		message("Mean of the outliers: ", meanoutliers)
		message("Mean with outliers: ", meanWithOutliers)
		message("Mean without outliers: ", meanWithoutOutliers)
	}

    #Crio variavel de retorno
	results <- list()

    #Preencho os dados da variavel de retorno
    results$withoutOutliers <- varName[!is.na(varName)]
	results$withOutliers <- var
	results$outliers <- outlier
	results$outliersCount <- outliersCount
	results$outliersPercent <- outliersPercent
	results$meanoutliers <- meanoutliers
	results$meanWithOutliers <- meanWithOutliers
	results$meanWithoutOutliers <- meanWithoutOutliers

    #Retorno a variavel
	return(invisible(results));
}

# Inject outliers into data.
 dataOutliers <- data.frame(speed = c(18,19, 19, 20, 20, 20,23,22,25,25), dist = c(183,190, 186, 210, 220, 218,220,200,1200,1250)) # introduce outliers.
result <- outlierInfo(dataOutliers, dataOutliers$dist, "Carros - Distancia", TRUE)
