###Data Quality Analysis

combined = read_csv("combined_data.csv")
combined$dataset = 0
combined$dataset[combined$academic22 == 1] = "academic22"
combined$dataset[combined$ces20 == 1] = "ces20"
combined$dataset[combined$officials20 == 1] = "officials20"
combined$dataset[combined$public19 == 1] = "public19"
combined$dataset[combined$public22 == 1] = "public22"
combined$dataset[combined$poc23 == 1] = "poc23"
table(combined$dataset)

#switch NONRESPONSE to NA
combined = combined %>%
  mutate(open_ext = ifelse(open_ext == "NONRESPONSE", NA, open_ext)) %>%
  mutate(responsible = ifelse(responsible == "NONRESPONSE", NA, responsible)) %>%
  mutate(responsible_done = ifelse(responsible_done == "NONRESPONSE", NA, responsible_done))

#open_ext percentage of nonresponses
table1 = combined %>%
  group_by(dataset) %>%
  summarize(non_responses = sum(is.na(open_ext)))

print(table1)

combined$nonresponse = 0
combined$nonresponse[combined$open_ext == "NONRESPONSE"] = 1

table(combined$academic22)
343/1699

table(combined$ces20)
167/1000

table(combined$officials20)
194/1347

table(combined$poc23)
197/1635

table(combined$public19)
210/2499

table(combined$public22)
408/2315

sum(table1$non_responses)/10495

#open_ext average character length
combined = combined %>%
  mutate(open_ext_chr = str_length(open_ext))

table2 = combined %>%
  group_by(dataset) %>%
  summarize(avg_open_ext = mean(open_ext_chr, na.rm = TRUE))

print(table2)

sum(combined$open_ext_chr, na.rm = TRUE)/10495

#responsible percentage of nonresponses
table3 = combined %>%
  group_by(dataset) %>%
  summarize(non_responses = sum(is.na(responsible)))

print(table3)

table(combined$academic22)
264/1699

table(combined$ces20)
1000/1000

table(combined$officials20)
1347/1347

table(combined$poc23)
184/1635

table(combined$public19)
2499/2499

table(combined$public22)
527/2315

(264+184+527)/5649

#responsible average character length
combined = combined %>%
  mutate(responsible_chr = str_length(responsible))

table4 = combined %>%
  group_by(dataset) %>%
  summarize(avg_responsible = mean(responsible_chr, na.rm = TRUE))

print(table4)

sum(combined$responsible_chr, na.rm = TRUE)/5649

#responsible_done percentage of nonresponses
table5 = combined %>%
  group_by(dataset) %>%
  summarize(non_responses = sum(is.na(responsible_done)))

print(table5)

table(combined$academic22)
685/1699

table(combined$ces20)
1000/1000

table(combined$officials20)
1347/1347

table(combined$poc23)
543/1635

table(combined$public19)
2499/2499

table(combined$public22)
701/2315

(685+543+701)/5649

#responsible_done average character length
combined = combined %>%
  mutate(responsible_done_chr = str_length(responsible_done))

table6 = combined %>%
  group_by(dataset) %>%
  summarize(avg_responsible_done = mean(responsible_done_chr, na.rm = TRUE))

print(table6)

sum(combined$responsible_done_chr, na.rm = TRUE)/5649

#add democracy to the combined dataset here
conjoint_AS22$ID = conjoint_AS22$ResponseId
conjoint_AS22 = unique(conjoint_AS22[c("ID", "democracy")])
conjoint_PS22$ID = conjoint_PS22$ResponseId
conjoint_PS22 = unique(conjoint_PS22[c("ID", "democracy")])
conjoint = bind_rows(conjoint_AS22, conjoint_PS22)
combined = merge(combined, conjoint[, c("ID", "democracy")], by = "ID", all.x = TRUE)

combined$democracy[combined$democracy == ""] = NA

#democracy percentage of nonresponses
table7 = combined %>%
  group_by(dataset) %>%
  summarize(non_responses = sum(is.na(democracy)))

print(table7)

table(combined$academic22)
279/1699

table(combined$ces20)
1000/1000

table(combined$officials20)
1347/1347

table(combined$poc23)
1635/1635

table(combined$public19)
2499/2499

table(combined$public22)
227/2315

sum(279, 227)/4014

#democracy average character length
combined = combined %>%
  mutate(democracy_chr = str_length(democracy))

table8 = combined %>%
  group_by(dataset) %>%
  summarize(avg_democracy = mean(democracy_chr, na.rm = TRUE))

print(table8)

sum(combined$democracy_chr, na.rm = TRUE)/4014

#add open_mod to the dataset
public19 = read_csv("/Users/kendr/Box/Extremism project/Expert and public survey/Data/2019 Public Sample.csv")
public19$ID = public19$ResponseId
public19 = unique(public19[c("ID", "open_mod")])

official20 = read_csv("/Users/kendr/Box/Extremism project/Expert and public survey/Data/2020 Elected Official Sample.csv")
official20$ID = official20$ResponseId
official20 = unique(official20[c("ID", "open_mod")])
conjoint1 = bind_rows(public19, official20)

combined = merge(combined, conjoint1, by = "ID", all.x = TRUE)
combined$open_mod[combined$open_mod == ""] = NA

#open_mod percentage of nonresponses
table9 = combined %>%
  group_by(dataset) %>%
  summarize(non_responses = sum(is.na(open_mod)))

print(table9)

table(combined$academic22)
1699/1699

table(combined$ces20)
1000/1000

table(combined$officials20)
181/1347

table(combined$poc23)
1635/1635

table(combined$public19)
21/2499

table(combined$public22)
2315/2315

sum(181, 21)/3846

#democracy average character length
combined = combined %>%
  mutate(open_mod_chr = str_length(open_mod))

table10 = combined %>%
  group_by(dataset) %>%
  summarize(avg_open_mod = mean(open_mod_chr, na.rm = TRUE))

print(table10)

sum(combined$open_mod_chr, na.rm = TRUE)/3846
