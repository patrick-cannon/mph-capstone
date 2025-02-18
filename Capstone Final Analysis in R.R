---
  title: "Capstone Working Document NEW"
author: "Patrick Cannon"
date: "3/5/2022"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# open the haven package to read an xpt
library(tidyverse)
library(package = "haven")
library(odds.n.ends)
library(table1)
library(DiagrammeR)
```

```{r}

# create a temporary file to store the zipped file
# before you open it
temp <- tempfile(fileext = ".zip")
# use download.file to put the zipped file in the temp file
# this will take a couple of minutes depending on computer speed
download.file(url = "http://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip",
              destfile = temp)
# unzip it and read it
brfss.2019 <- read_xpt(file = temp)
```

```{r}
# Memory loss - outcome
brfss.2019$CIMEMLOS

# physical activity
brfss.2019$EXERANY2

# deaf or hearing loss
brfss.2019$DEAF

# age
brfss.2019$`_AGE_G`

# gender
brfss.2019$`_SEX`

# race
brfss.2019$`_RACEGR3`

# alternate race var
brfss.2019$`_RACEG21`


```

```{r}
brfss.2019.cleaned <- brfss.2019 %>%
  select(CIMEMLOS, EXERANY2, DEAF, `_AGE_G`, `_SEX`, `_RACEGR3`) %>%
  mutate(CIMEMLOS = recode_factor(CIMEMLOS, 
                                  "1" = "Yes", 
                                  "2" = "No",
                                  "7" = NA_character_,
                                  "9" = NA_character_)) %>%
  mutate(EXERANY2 = recode_factor(EXERANY2,
                                  "1" = "Yes", 
                                  "2" = "No",
                                  "7" = NA_character_,
                                  "9" = NA_character_)) %>%
  mutate(DEAF = recode_factor(DEAF,
                              "1" = "Yes", 
                              "2" = "No",
                              "7" = NA_character_,
                              "9" = NA_character_)) %>%
  mutate(`_AGE_G` = recode_factor(`_AGE_G`,
                                  "1" = "18-24", 
                                  "2" = "25-34",
                                  "3" = "35-44",
                                  "4" = "45-54",
                                  "5" = "55-64",
                                  "6" = "65 plus")) %>%
  mutate(`_SEX` = recode_factor(`_SEX`,
                                "1" = "Male", 
                                "2" = "Female")) %>%
  mutate(`_RACEGR3` = recode_factor(`_RACEGR3`,
                                    "1" = "White, Non-Hispanic", 
                                    "2" = "Black, Non-Hispanic",
                                    "3" = "Other Race, Non-Hispanic",
                                    "4" = "Multiracial, Non-Hispanic",
                                    "5" = "Hispanic",
                                    "9" = NA_character_)) %>%
  drop_na()
```

```{r}
label(brfss.2019.cleaned$EXERANY2) <- "Any Exercise"
label(brfss.2019.cleaned$DEAF) <- "Deaf"
label(brfss.2019.cleaned$`_SEX`) <- "Sex"
label(brfss.2019.cleaned$`_RACEGR3`) <- "Race"

table1 <- table1(~ EXERANY2 + DEAF + `_SEX` + `_RACEGR3` | CIMEMLOS, data = brfss.2019.cleaned, caption = "Table 1: Descriptive Statistics")
table1
```

```{r}
write.table(table1, file = "Table 1.txt", sep = ",", quote = FALSE, row.names = F)
```

```{r figure 1}
# Figure 1
grViz("digraph flowchart {

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize=10] 
      # notes indicate how many boxes you will have in your diagram. Since I have two sets of exclusions, I will have three nodes.
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      
      # edge definitions with the node IDs are used to indicate how the rectangle boxes flow from each other. This is a simple flow chart but I could create branching instead as below
      tab1 -> tab2 -> tab3
}
      #This set of code provides the text in each rectangle box.
      [1]: 'Total BRFSS observations n=418,268'
      [2]: 'Excluding 309,997 individuals who did not receieve the optional cognitive decline module and with missing data, n=108,271'
      [3]: 'Excluding 3,403 individuals with influential points upon analysis n=104,868'
      ")

```


```{r}
summary(brfss.2019.cleaned)
```

```{r}
# MLR with cleaned data
brfss.2019.regression.multiple <- glm(CIMEMLOS ~ EXERANY2 + DEAF + `_SEX` + `_RACEGR3`, data = brfss.2019.cleaned, family = 'binomial')
summary(brfss.2019.regression.multiple)
```
```{r}
odds.n.ends(brfss.2019.regression.multiple)
```

```{r}
car::vif(brfss.2019.regression.multiple)
```

```{r}
model_influence <- influence.measures(model = brfss.2019.regression.multiple)
```

```{r}
model_influence_live <- data.frame(model_influence$infmat)
```

```{r}
model_influence_live %>%
  filter(cook.d > 4/108271 & hat > 2 * 7 / 108271)
```

```{r}
model_influence_live <- model_influence_live %>%
  rownames_to_column()

brfss.2019.cleaned.diag <- brfss.2019.cleaned %>%
  rownames_to_column() %>%
  merge(x = model_influence_live, by = 'rowname')

brfss.2019.cleaned.diag
```

```{r}
brfss.2019.cleaned.inf <- brfss.2019.cleaned.diag %>%
  subset(cook.d < 4/108271 | hat < 2 * 7 / 108271) %>%
  select(CIMEMLOS, EXERANY2, DEAF, `_AGE_G`, `_SEX`, `_RACEGR3`)
```

```{r}
summary(brfss.2019.cleaned.inf)
```

```{r}
# new table 1
label(brfss.2019.cleaned.inf$EXERANY2) <- "Any Exercise"
label(brfss.2019.cleaned.inf$DEAF) <- "Deaf"
label(brfss.2019.cleaned.inf$`_SEX`) <- "Sex"
label(brfss.2019.cleaned.inf$`_RACEGR3`) <- "Race"

table1.inf <- table1(~ EXERANY2 + DEAF + `_SEX` + `_RACEGR3` | CIMEMLOS, data = brfss.2019.cleaned.inf, caption = "Table 1: Descriptive Statistics")
table1.inf
```

```{r}
# write table to disk for export
write.table(table1.inf, file = "Table 1 Final.txt", sep = ",", quote = FALSE, row.names = F)
```

```{r}
# MLR with influential points removed
brfss.2019.regression.multiple.inf <- glm(CIMEMLOS ~ EXERANY2 + DEAF + `_SEX` + `_RACEGR3`, data = brfss.2019.cleaned.inf, family = 'binomial')
summary(brfss.2019.regression.multiple.inf)
odds.n.ends(brfss.2019.regression.multiple.inf)
```

```{r}
# multicollinearity assumption
vif.table <- car::vif(brfss.2019.regression.multiple.inf)
```

```{r}
# write to disk for export
write.table(vif.table, file = "VIF table.txt", sep = ",", quote = FALSE, row.names = F)
```