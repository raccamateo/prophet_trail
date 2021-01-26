
#usar boxplot para manejar los outliers

boxplot(df$variable)

#creamos un vector con los outliers

outliers <- boxplot(df$variable, plot = FALSE)$out

#reemplazamos los outliers por NA

df[df$variable %in% outliers, "variable"] = NA
