library(tidyverse)
library(readr)
library(readxl)
EPIresults <- read_excel("Documents/EPIresults.xlsx")
View(EPIresults)
epi2022_Afr <- EPIresults %>% filter((region == "Sub-Saharan Africa")| (country =="Tunisia") |  (country =="Egypt") | (country =="Algeria") | (country =="Morocco")|(country =="Sudan"))
View(epi2022_Afr)
index2022_data <- read_excel("~/Downloads/index2022_data.xls")
View(index2022_data)
econ_index2022 <- index2022_data[,-3]
econ_index_Afr2022 <- econ_index2022 %>% filter((Region == "Sub-Saharan Africa") | (Region == "Middle East and North Africa"))
econ_index_Afr2022 <- econ_index_Afr2022[-c(3,27:30,32,33,47,48,51,58,63,64), ]
View(econ_index_Afr2022)
econ_index_Afr2022[econ_index_Afr2022 == "São Tomé and Príncipe"] <- "Sao Tome and Principe"
FDIData_April2023 <- read_excel("~/Downloads/FDIData_April2023.xlsx", sheet = "China_flow2021")
epi_FDI_dataset <- merge(econ_index_Afr2022, FDIData_April2023, by = "Country Name", all = TRUE)
CPI2022_GlobalResultsTrends <- read_excel("Downloads/CPI2022_GlobalResultsTrends.xlsx", sheet = "Sheet1")
pap_dataset <- merge(epi_FDI_dataset, CPI2022_GlobalResultsTrends, by = "Country Name", all = TRUE)
pa_dataset <- pap_dataset %>% filter((Region.x == "Sub-Saharan Africa") | (Region.x == "Middle East and North Africa"))

a <- lm (log(FDI_2021+270) ~  log(GDP_2022) + EFREE_2022 + CPI_2022 + EPI.new , data = cleaned_data)
summary(a)
vif(a)
## log(GDP_2022)    EFREE_2022      CPI_2022       EPI.new 
## 1.188771      2.098506      2.394078      1.382094
ab <- lm (log(FDI_2021+270) ~ log(GDP_2022) + EFREE_2022 + CPI_2022 + ER_2017 , data = cleaned_data)
summary(ab)
abb <- lm (log(FDI_2021+270) ~ log(GDP_2022) + EFREE_2022 + CPI_2022 + EPI.new , data = cleaned_data)
summary(abb)
aba <- lm (log(FDI_2021+270) ~ gr_2022 + EFREE_2022 + CPI_2022 + ER_2017 , data = cleaned_data)
summary(aba)
ac <- lm (log(FDI_2012+ 815) ~ log(GDP_2012) + EFREE_2012 + CPI_2012 + ER_2013 , data = cleaned_data)
summary(ac)
aa <- lm (log(FDI_2021+270) ~  change_gr + change_FREE + change_CPI + change_ER , data = cleaned_data)
summary(aa)
b <- lm(change_FDI ~ delta + change_FREE + change_CPI + EPI.change, data = epi_FDI_dataset)
summary(b)
bb <- lm(change_FDI ~ change_gr + change_FREE + change_CPI + change_ER, data = cleaned_data)
summary(bb)
c <- lm(change_FDI ~ change_gr + change_FREE + change_CPI + EPI.change, data = cleaned_data)
summary(c)
d <-  lm( log(FDI_2021+270) ~ change_gr + change_FREE + change_CPI + EPI.change, data = cleaned_data)
summary(d)
e <-  lm( FDI_2021 ~ EPI.new, data = cleaned_data)
summary(e)
stargazer(ab,aba, abb, ac, bb, c, type = "html",
          title = "Table 3.Regression Models Summary ",
          align = TRUE,
          header = FALSE,
          ci = FALSE,  # Don't include confidence intervals  # Include standard errors
          star.cutoffs = c(0.1,0.05, 0.01, 0.001), omit.stat = c("f"),  # Set significance levels for stars
          out = "e_table.html")  # Save the table to an HTML file

epi_FDI_dataset <- epi_FDI_dataset %>%
  mutate(change_FDI = FDI_2021 -  FDI_2012)
epi_FDI_dataset <- epi_FDI_dataset %>%
  mutate(change_GDP = GDP_2022 -  GDP_2012)
epi_FDI_dataset <- epi_FDI_dataset %>%
  mutate(change_FREE = EFREE_2022 -  EFREE_2012)
epi_FDI_dataset <- epi_FDI_dataset %>%
  mutate(change_CPI = CPI_2022 -  CPI_2012)
epi_FDI_dataset <- epi_FDI_dataset %>%
  mutate(delta = GDP_2022/GDP_2012)
epi_FDI_dataset <- merge(epi_FDI_dataset, Gdp_gr2012_22, by = "iso", all = TRUE)

# Install openxlsx package
install.packages("openxlsx")

# Load the openxlsx package
library(openxlsx)
# Write the data frame to an Excel file
write.xlsx(cleaned_data, "cleaned_data.xlsx")

# Set up the plotting area to have 1 row and 2 columns
par(mfrow = c(1, 2))

# First scatterplot
plot(cleaned_data$GDP_2022, cleaned_data$FDI_2021,  main = "Scatterplot 1", xlab = "GDP_2022", ylab = "FDI_2021", col = "blue", pch = 19)

LGDP_22 <- log(cleaned_data$GDP_2022)
# Second scatterplot
plot(LGDP_22, cleaned_data$FDI_2021 , main = "Scatterplot 2", xlab = "LGDP_22", ylab = "FDI_2021", col = "red", pch = 19)

LFDI_21 <- log (cleaned_data$FDI_2021 + 270)

plot(LGDP_22, LFDI_21 , main = "Scatterplot 2", xlab = "LGDP_22", ylab = "FDI_2021", col = "red", pch = 19)
plot(cleaned_data$change_GDP, cleaned_data$change_FDI,  main = "Scatterplot 1", xlab = "GDP", ylab = "FDI", col = "blue", pch = 19)
plot(cleaned_data$change_gr, cleaned_data$change_FDI,  main = "Scatterplot 1", xlab = "GDP", ylab = "FDI", col = "blue", pch = 19)
rel_fdi <- (cleaned_data$FDI_2021 - cleaned_data$FDI_2012)/cleaned_data$FDI_2012
plot(cleaned_data$change_gr, rel_fdi,  main = "Scatterplot 1", xlab = "GDP", ylab = "FDI", col = "blue", pch = 19)
plot(cleaned_data$EPI.change, rel_fdi,  main = "Scatterplot 1", xlab = "EPI", ylab = "FDI", col = "blue", pch = 19)
plot(cleaned_data$change_ER, rel_fdi,  main = "Scatterplot 1", xlab = "ER", ylab = "FDI", col = "blue", pch = 19)
plot(cleaned_data$EPI.new, LFDI_21 , main = "Scatterplot 2", xlab = "EPI_2022", ylab = "FDI_2021", col = "red", pch = 19)

# Create scatterplot with ggplot2 and add a fitted line
#quite flat
ggplot(cleaned_data, aes(x = EPI.new, y = LFDI_21)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI_2022") +
  ylab("LFDI_21")

g1 <-ggplot(cleaned_data, aes(x = EPI.new, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EPI_2022") +
  ylab("FDI_2021")

g2 <- ggplot(cleaned_data, aes(x = EPI.old, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EPI_2012") +
  ylab("FDI_2012")

LFDI_12 <- log (cleaned_data$FDI_2012 + 815)

ggplot(cleaned_data, aes(x = EPI.old, y = LFDI_12)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI_2012") +
  ylab("LFDI_12")

ggplot(cleaned_data, aes(x = EPI.old, y = rel_fdi)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI_2012") +
  ylab("FDI_rel")

ggplot(cleaned_data, aes(x = ER_2017, y = LFDI_21)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("ER_2017") +
  ylab("LFDI_21")

g3 <- ggplot(cleaned_data, aes(x = ER_2017, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("ER_2017") +
  ylab("FDI_2021")

g4 <- ggplot(cleaned_data, aes(x = ER_2013, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("ER_2013") +
  ylab("FDI_2012")

LFDI_12 <- log (cleaned_data$FDI_2012 + 815)

ggplot(cleaned_data, aes(x = ER_2013, y = LFDI_12)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("ER_2013") +
  ylab("LFDI_12")

ggplot(cleaned_data, aes(x = ER_2017, y = rel_fdi)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("ER_2017") +
  ylab("FDI_rel")

#EPI.new and others
ggplot(cleaned_data, aes(x = EPI.new, y = CPI_2022)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EPI_2022") +
  ylab("CPI_2022") #relatively strong relationship

ggplot(cleaned_data, aes(x = EPI.new, y = EFREE_2022)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EPI_2022") +
  ylab("EFREE_2022")

ggplot(cleaned_data, aes(x = EPI.new, y = gr_2022)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EPI_2022") +
  ylab("Growth_2022")

#changes
ggplot(cleaned_data, aes(x = EPI.change, y = change_FREE)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI.change") +
  ylab("Efree_change")

ggplot(cleaned_data, aes(x = EPI.change, y = change_CPI)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI.change") +
  ylab("change_CPI")

ggplot(cleaned_data, aes(x = EPI.change, y = change_gr)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("EPI.change") +
  ylab("change_gr")

ggplot(cleaned_data, aes(x = change_ER, y = change_gr)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("change_ER") +
  ylab("change_gr")

ggplot(cleaned_data, aes(x = change_ER, y = change_CPI)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  ggtitle("Scatterplot with Fitted Line") +
  xlab("change_ER") +
  ylab("change_CPI")

ggplot(cleaned_data, aes(x = change_ER, y = change_FREE)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("change_ER") +
  ylab("change_FREE")

ggplot(cleaned_data, aes(x = ER_2017, y = EFREE_2022)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("ER_2017") +
  ylab("EFREE_2022")

ggplot(cleaned_data, aes(x = ER_2013, y = EFREE_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("ER_2013") +
  ylab("EFREE_2012")

ggplot(cleaned_data, aes(x = ER_2017, y = CPI_2022)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("ER_2017") +
  ylab("CPI_2012")

g5 <- ggplot(cleaned_data, aes(x = gr_2022, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("gr_2022") +
  ylab("FDI_2021")

g6 <- ggplot(cleaned_data, aes(x = gr_2012, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("gr_2012") +
  ylab("FDI_2012")

g7 <- ggplot(cleaned_data, aes(x = GDP_2012, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("GDP_2012") +
  ylab("FDI_2012")

g8 <- ggplot(cleaned_data, aes(x = GDP_2022, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("GDP_2022") +
  ylab("FDI_2021")

g9 <- ggplot(cleaned_data, aes(x = EFREE_2022, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EFREE_2022") +
  ylab("FDI_2021")

g10 <- ggplot(cleaned_data, aes(x = EFREE_2012, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("EFREE_2012") +
  ylab("FDI_2012")

g11 <- ggplot(cleaned_data, aes(x = CPI_2022, y = FDI_2021)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("CPI_2022") +
  ylab("FDI_2021")

g12 <- ggplot(cleaned_data, aes(x = CPI_2012, y = FDI_2012)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "red") +  # Fitted line
  xlab("CPI_2012") +
  ylab("FDI_2012")


grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12, nrow = 3)


# Reset the plotting layout to default
par(mfrow = c(1, 1))

cor_matrix <- cor(cleaned_data[,-c(1,2,3)], use = "complete.obs", method = "pearson")
print(cor_matrix)

stargazer(epi2022results_all, type = "html", summary = FALSE, title = "Table 4. EPI: Summary Statistics", out = "REEsites.html")

