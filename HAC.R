# R version 3.6.0 (2019-04-26)
library(tidyverse) # v. 1.2.1
library(rvest) # v. 0.3.2
library(magick) # v. 2.0
library(tesseract) # v. 4.0
library(extrafont) # v. 0.17
rus <- tesseract("rus")
theme_set(theme_bw()+theme(text = element_text(size = 14, family = "Roboto Medium")))
                        
# collecting data from RAS website ----------------------------------------
links <- c(paste0("http://www.ras.ru/members/personalstaff1724/fullmembers.aspx?ml=", 0:32),
           paste0("http://www.ras.ru/members/personalstaff1724/correspondentmembers.aspx?acmem=", 0:32),
           paste0("http://www.ras.ru/members/personalstaff1724/foreignmembers.aspx?afmem=", 0:32),
           paste0("http://www.ras.ru/members/personalstaff1724/honorarymembers.aspx?ahmem=", 0:32))
results <- tibble(name = NA, year = NA, type = NA, link = NA)

sapply(links, function(url){
  source <- read_html(url)
  source %>% 
    html_nodes(".list1 > li") %>%
    html_text() %>% 
    tibble(text = .) %>% 
    separate(text, c("name", "year"), sep = "Дата избрания ") ->
    temp_result
  
source %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    tibble(link = .) %>% 
    filter(str_detect(link, "/win/db/show_per")) %>% 
    cbind(temp_result) %>% 
    select(name, year, link) %>% 
    mutate(type = url) ->
    temp_result
  
  results <<- rbind(results, temp_result)
})

results %>% 
  slice(-1) %>% 
  mutate(year = as.double(year),
         link = paste0("http://www.ras.ru", link),
         type = case_when(str_detect(type, "fullmembers") ~ "академик",
                          str_detect(type, "correspondentmembers") ~ "член-корреспондент",
                          str_detect(type, "foreignmembers") ~ "иностранный член",
                          str_detect(type, "honorarymembers") ~ "почетный член")) %>% 
  rowwise() %>% 
  mutate(second_name = unlist(str_split(name, " "))[3],
         sex = ifelse(str_detect(second_name, "[вч]на$"), "f", "m")) %>% 
  distinct()  %>%  
  ungroup() %>% 
  mutate(id = 1:n()) ->
  results

write_csv(results, "ran.csv", na = "")



# collect list of HAC orders ----------------------------------------------
url <- "http://arhvak.minobrnauki.gov.ru/web/guest/121"

source <- read_html(url)
source %>% 
  html_nodes("a") %>% 
  html_text() %>% 
  tibble(text = .) ->
  texts

source %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  tibble(link = .) ->
  links

HAC_orders <- data.frame(text = texts, link = links)
rm(texts, links, source, url)

HAC_orders %>% 
  filter(str_detect(text, "[Оо] выдаче диплом")) %>% 
  slice(-c(1:4)) %>% 
  mutate(type = str_extract(text, "доктор|кандидат"),
         time = str_extract(text, "\\D\\d{1,2}\\D.*\\D201[:digit:]( г.)?"),
         time = str_remove(time, "^ "),
         time = str_remove(time, " г."),
         time = str_replace(time, "\\.05\\.", " мая "),
         time = str_replace(time, "\\.04\\.", " апреля "),
         file_name = str_extract(link, "/[a-zA-Z0-9-%_+\\.]*/[a-zA-Z0-9-]*$"),
         file_name = str_remove(file_name, "/[a-zA-Z0-9-]*$"),
         file_name = str_remove(file_name, "/"),
         file_name = ifelse(is.na(file_name), str_extract(link, "10\\d\\d\\-nk.pdf"), file_name)) %>% 
  filter(!str_detect(time, "№")) %>% 
  mutate(id = 1:n()) ->
  HAC_orders

file_name <- sapply(HAC_orders$file_name, URLdecode)
HAC_orders$file_name <- file_name

writeLines(HAC_orders$link, "links.txt")


# download all HAC orders -------------------------------------------------
# R
# sapply(1:901, function(i){
#   download.file(HAC_orders$link[i], paste0("pdfs/", i, ".pdf"))
# })
#
# сервер обрывает содинение :(
# 
# BASH
# aria2c -d pdfs/aria/ -i links.txt
#
# R correction
# data.frame(file_name = list.files(paste0(getwd(), "/pdfs/aria/"))) %>% 
#   left_join(HAC_orders) %>% 
#   select(file_name, id) %>% 
#   na.omit() ->
#   results
# 
# file.rename(paste0(getwd(), "/pdfs/aria/", results$file_name),
#             paste0(getwd(), "/pdfs/aria/", results$id, ".pdf"))
 
HAC_orders %>%
  slice(as.double(str_remove(list.files("pdfs/"), ".pdf"))) ->
  HAC_orders

write_csv(HAC_orders, "HAC_orders.csv")

# extract pics from HAC pdf -----------------------------------------------
sapply(HAC_orders$id, function(y){
  pdf <- image_read_pdf(path = paste0("pdfs/", y, ".pdf"), density = 150)
  pdf <- pdf[-c(1:2)]
  sapply(seq_along(pdf), function(x){
    image_write(pdf[x], paste0("pics/", y, "_", x, ".jpeg"), format = "jpeg", quality = 50)  
  })
  gc()
})


# extract info from HAC pics ----------------------------------------------

sapply(list.files("pics/"), function(i){
  print(i)
  text <- ocr(paste0("pics/", i), engine = rus)    
  text <- unlist(str_split(text, "\\n"))
  name1 <- text[str_which(text, "сов[ес]т (на|при)")]
  name2 <- text[str_which(text, "сов[ес]т (на|при)") + 1]
  name3 <- text[str_which(text, "сов[ес]т (на|при)") + 2]
  scie_field <- paste(text[str_which(text, "СПИСОК")+1:3], collapse = " ")
  id <- str_remove(str_extract(i, "^.*_"), "_")
  write_csv(data.frame(
    ifelse(length(name1) == 0, NA, name1), 
    ifelse(length(name2) == 0, NA, name2), 
    ifelse(length(name3) == 0, NA, name3), 
    ifelse(length(scie_field) == 0, NA, scie_field), 
    i, 
    id), 
            "HAC_people_2.csv", append = TRUE, col_names = FALSE)
})


# format extracted from HAC data -----------------------------------------------
HAC_links <- read_csv("HAC_orders.csv")
HAC <- read_csv("HAC_people.csv", col_names = FALSE)
colnames(HAC) <- c("l_name", "name", "s_name", "sci_field", "file", "id")

sapply(seq_along(HAC$sci_field), function(i){
  HAC$sci_field[i] <<- ifelse(is.na(HAC$sci_field[i]), 
                              HAC$sci_field[i-1],
                              HAC$sci_field[i])
})

left_join(HAC, HAC_links) %>% 
  rename(order_text = text) %>% 
  mutate(sci_field = ifelse(is.na(sci_field), lag(sci_field),sci_field),
         sci_field2 = str_extract(sci_field, "ди(п|н)лом[-—–\\| а-яА-Я]*(\\(|\\{)"),
         sci_field2 = str_remove(sci_field2, "ди(п|н)лом ?(кандидата|доктора) "),
         sci_field2 = str_remove_all(sci_field2, "\\(|\\{|\\|"),
         sci_field2 = str_remove_all(sci_field2, "^\\s*"),
         sci_field2 = str_replace(sci_field2, "[иы]х наук", "ие науки"),
         l_name_2 = str_extract(l_name, "^\\S*\\s*(—)?\\s*\\S*"),
         l_name_2 = str_remove(l_name_2, " сов(е|с)т"),
         l_name_2 = str_remove(l_name_2, "^.{1,2}(\\.|,)?\\s{1,2}"),
         l_name_2 = str_remove(l_name_2, "— ?`?'?"),
         name_2 = str_extract(name, "^([А-Я][а-я]* ){1,2}"),
         s_name_2 = str_extract(s_name, "^([А-Я][а-я]* ){1,2}"),
         name_2 = ifelse(is.na(s_name_2), name_2, paste(name_2, s_name_2)),
         name_2 = str_remove(name_2, "\\s*$"),
         sex = ifelse(str_detect(name_2, "(в|ч)на$"), "f", "m")) %>% 
  select(-s_name_2) %>% 
  filter(!(is.na(name)&is.na(l_name)&is.na(s_name))) ->
  HAC

write_csv(HAC, "HAC.csv")
  
# collect data about RAS departments --------------------------------------

url <- "http://www.ras.ru/presidium/headquarters/presidiummembers.aspx"

source <- read_html(url)

source %>% 
  html_nodes("a") %>%
  html_text() %>% 
  tibble(person = .) %>% 
  slice(37:79) %>%
  mutate(department = "президиум") ->
  presidium

df <- read_csv("RAS_departments.csv")
results <- df[0,]

sapply(1:nrow(df), function(i){
  source <- read_html(df$link[i])
  source %>% 
    html_nodes("font.clsFnt8:nth-child(3)") %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    tibble(department = df$department[i],
           link = .) %>% 
    rbind(results) ->>
    results
})

results %>% 
  slice(-n()) %>% 
  mutate(link = paste0("http://www.ras.ru/win/db/", link)) %>% 
  rbind(df) ->
  results

persons <- tibble(person = NA, department = NA)
sapply(seq_along(results$link), function(i){
  source <- read_html(results$link[i])  
  source %>% 
    html_nodes("tbody") %>% 
    html_text() ->
    text  
  text <- unlist(str_split(text, "\\n"))
  text <- text[str_which(text, "член Отделения")-1]
  tibble(person = text, 
         department = results$department[i]) %>% 
    rbind(persons) ->>
    persons
})

persons %>% 
  filter(!is.na(person)) %>% 
  mutate(person = str_remove(person, "^\\s*"),
         person = str_remove(person, "\\s*$")) %>% 
  filter(person != "") %>% 
  rbind(presidium) %>% 
  mutate(sex = ifelse(str_detect(person, "(в|ч)на$"), "f", "m")) ->
  persons

write_csv(persons, "RAS_dep_persons.csv")

# merge academics and deps ------------------------------------------------
deps <- read_csv("RAS_dep_persons.csv")
all_acad <- read_csv("RAS.csv")

all_acad %>%
  filter(type != "иностранный член") %>% 
  full_join(deps) ->
  all

write_csv(all, "RAS_aggregated.csv", na = "")


# RAS proffesors ----------------------------------------------------------
url <- "http://www.ras.ru/about/awards/professorofras/list.aspx"

source <- read_html(url)

source %>% 
  html_nodes("a") %>% 
  html_text() %>% 
  .[str_detect(., "Постановление")] %>% 
  tibble(or = .) %>% 
  mutate(year = str_extract(or, "\\.\\d{4}"),
         year = str_remove(year, "\\."),
         sci_field = str_extract(or, "Отделения.*$"),
         sci_field = str_remove(sci_field, "Отделения "),
         sci_field = str_remove(sci_field, '\\)'),
         sci_field = str_remove(sci_field, '"'),
         sci_field = str_remove(sci_field, '\\s$'),
         sci_field = str_replace(sci_field, "ых ", "ые "),
         sci_field = str_replace(sci_field, "их ", "ие "),
         sci_field = str_replace(sci_field, "наук", "науки")) %>% 
  select(-or) ->
  labels

source %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  tibble(link = .) %>% 
  filter(str_detect(link, "presidium/|docid")) %>% 
  mutate(link = paste0("http://www.ras.ru", link)) %>% 
  slice(-1) %>% 
  cbind(labels) %>% 
  filter(!str_detect(link, "docid")) ->
  profesors_source

results <- tibble(name = NA, sci_field = NA, year = NA, link = NA, sex = NA)
sapply(seq_along(profesors_source$link), function(i){
  source <- read_html(profesors_source$link[i])
  source %>% 
    html_nodes("tr > td > p") %>% 
    html_text() %>% 
    tibble(name = .) %>% 
    mutate(name = str_extract(name, "\\S*\\s\\S*\\s\\S*"),
           name = ifelse(str_detect(name, "доктор"), NA, name)) %>% 
    na.omit() %>% 
    mutate(sci_field = profesors_source$sci_field[i],
           year = profesors_source$year[i],
           link = profesors_source$link[i],
           sex = ifelse(str_detect(name, "(в|ч)не$"), "f", "m")) %>% 
    rbind(results) ->>
    results
})

results <- results[-nrow(results),]

write_csv(results, "RAS_proffesors.csv")

# directors ---------------------------------------------------------------
source <- "https://ru.wikipedia.org/wiki/%D0%98%D0%BD%D1%81%D1%82%D0%B8%D1%82%D1%83%D1%82%D1%8B_%D0%A0%D0%90%D0%9D"

source <- read_html(source)

source %>% 
  html_nodes("ul > li > a") %>% 
  html_text() %>% 
  tibble(text = .) %>% 
  slice(56:738) ->
  titles

rmv <- str_which(titles$text, "РАН")

titles <- titles[rmv,]

source %>% 
  html_nodes("ul > li > a") %>% 
  html_attr("href") %>% 
  tibble(url = .) %>% 
  slice(56:738) %>% 
  slice(rmv) ->
  urls

urls_rmv <- str_which(urls$url, "/w/", negate = TRUE)

urls <- paste0("https://ru.wikipedia.org", urls$url[urls_rmv])
titles <- titles$text[urls_rmv]

results <- tibble(director = NA, url = NA, institute = NA)
sapply(seq_along(urls), function(i){
  source <- read_html(urls[i])
  source %>% 
    html_nodes("tr") %>% 
    html_text() ->
    text
  tibble(director = text[str_which(text, "Директор")], url = urls[i], institute = titles[i]) %>% 
    rbind(results) ->>
    results
})

results %>% 
  mutate(director2 = str_remove_all(director, "ак. |акад. |\n|Директор|чл(ен)?.-корр. РАН |Годы|—|\\[1\\]|"),
         director3 = str_extract(director2, "[А-Я][а-я]{0,}\\.?,? [А-Я][а-я]{0,}\\.? [А-Я][а-я]{0,}\\.?")) %>% 
  write_csv(., "RAS_directors.csv", na = "")


# plots -------------------------------------------------------------------

## 1 Доля женщин по разным направлениям в ВАК
HAC <- read_csv("HAC.csv")
HAC %>% 
  na.omit() %>% 
  mutate(sci_field2 = str_remove(sci_field2, "(-|—) "),
         sci_field2 = str_replace(sci_field2, "ч?изико", "физико"),
         sci_field2 = str_remove(sci_field2, "диплом[цу]кандидата "),
         sci_field2 = str_remove(sci_field2, "диплом.*ора "),
         sci_field2 = str_remove(sci_field2, "диплом.*ата"),
         sci_field2 = str_remove(sci_field2, "диплом.*ага"),
         sci_field2 = ifelse(str_detect(sci_field2, "едиц"), "медицинские науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "физи"), "физико-математические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "[хд]ниче"), "технические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "минерал"), "геолого-минералогические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "фил[ао]с"), "философские науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "илолог"), "филологические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "хозяй"), "сельскохозяйственные науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "графич"), "географические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "дагог"), "педагогические науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "ветер"), "ветеринарные науки", sci_field2),
         sci_field2 = ifelse(str_detect(sci_field2, "хими"), "химические науки", sci_field2)) %>% 
  count(sci_field2, sex, type) %>% 
  spread(sex, n) %>% 
  rowwise() %>% 
  mutate(total = f+m,
         ratio = f/total,
         conf_l = binom.test(f, total)$conf.int[1],
         conf_h = binom.test(f, total)$conf.int[2]) %>% 
  rev() %>%
  ggplot(aes(reorder(sci_field2, desc(sci_field2)), ratio, color = type, ymin = conf_l, ymax = conf_h)) +
  geom_pointrange(position = position_dodge(width = -0.5))+
  geom_text(y = 1.05, aes(label = round(ratio, 2)), position = position_dodge(width = -0.8))+
  coord_flip()+
  labs(x = "",
       color = "",
       y = "доля женщин среди кандидатов и докторов наук, защитившихся в последние 7 лет",
       caption = "данные автоматически собраны из приказов ВАК: arhvak.minobrnauki.gov.ru, 27.04.2019")+
  ylim(0, 1.05) +
  theme(plot.caption = element_text(hjust = 1))

## 2 Доля женщин с основания РАН в принципе

RAS <- read_csv("RAS_aggregated.csv")
RAS %>% 
  count(sex, year) %>% 
  spread(sex, n, fill = 0) %>% 
  mutate(f_c = cumsum(f),
         m_c = cumsum(m)) %>% 
  gather(sex, value, f_c:m_c) %>% 
  ggplot(aes(year, value, color = sex)) +
  geom_line(size = 2)+
  labs(x = "год",
       y = "кумулятивной суммы (логорифмированная шкала)",
       caption = "данные автоматически собраны с сайта ras.ru, 27.04.2019",
       color = "") + 
  scale_color_manual(labels = c("женский", "мужской"),
                     values = c("#E69F00", "#009E73"))+
  theme(legend.position = c(0.2, 0.9),
        legend.direction = "horizontal")+
  scale_y_log10()

## 3 Доля женщин в классах набора в РАН за последние N лет
RAS %>% 
  filter(type != "почетный член") %>% 
  count(sex, year, type) %>% 
  spread(sex, n, fill = 0) %>% 
  mutate(total = f+m,
         ratio = f/total) %>% 
  filter(year >= 1925) %>% 
  ggplot(aes(year, ratio, color = type)) +
  geom_smooth(se = FALSE, size = 2)+
  geom_point()+
  scale_color_discrete(labels = c("члены-коресспонденты", "академики"))+
  theme(legend.position = c(0.3, 0.9),
        legend.direction = "horizontal")+
  labs(x = "год",
     y = "доля женщин в каждом приеме в академию",
     caption = "данные автоматически собраны с сайта ras.ru, 27.04.2019",
     color = "")

## 4 Доля женщин в нынешнем составе по категориям: профессора, членкоры, академики
prof <- read_csv("RAS_proffesors.csv")
prof %>% 
  mutate(type = "профессор") %>% 
  select(name, type, year, sex, sci_field) %>%
  mutate(sci_field = str_replace(sci_field, "е ", "х "),
         sci_field = str_replace(sci_field, "науки", "наук"),
         sci_field = paste("Отделение", sci_field)) %>% 
  filter(sci_field != "Отделение NA") %>% 
  rename(department = sci_field) %>% 
  rbind(RAS[,c("name", "type", "year", "sex", "department")]) %>% 
  filter(!is.na(department)) %>% 
  count(type, sex) %>% 
  spread(sex, n) %>% 
  rowwise() %>% 
  na.omit() %>% 
  mutate(type = factor(type, levels = c("профессор", "член-корреспондент", "академик")),
         total = f+m,
         ratio = f/total,
         conf_l = binom.test(f, total)$conf.int[1],
         conf_h = binom.test(f, total)$conf.int[2]) %>% 
  ggplot(aes(type, ratio, ymin = conf_l, ymax = conf_h)) +
  geom_pointrange(size = 1.5)+
  labs(x = "",
       y = "доля женщин в РАН",
       caption = "данные автоматически собраны с сайта ras.ru, 27.04.2019")+
  geom_text(y = 0.01, aes(label = round(ratio, 2)))+
  ylim(0, 0.26)

## 5 Доля женщин в нынешнем составе РАН по отделениям

RAS %>% 
  filter(type != "почетный член",
         department != "президиум",
         !is.na(department)) %>% 
  mutate(department = str_remove(department, "Отделение "),
         department = str_remove(department, " РАН"),
         department = ifelse(department == "нанотехнологий и информационных технологий",
                             "нанотехнологий\nи информационных технологий",
                             department),
         department = ifelse(department == "энергетики, машиностроения, механики и процессов управления",
                             "энергетики, машиностроения,\nмеханики и процессов\nуправления",
                             department),
         department = ifelse(department == "глобальных проблем и международных отношений",
                             "глобальных проблем\nи международных отношений",
                             department)) %>% 
  group_by(department) %>% 
  mutate(tot = n()) %>% 
  group_by(department, sex) %>% 
  mutate(tot_ratio = n()/tot) %>% 
  ungroup() %>% 
  mutate(department = reorder(department, tot_ratio)) %>% 
  count(sex, type, department) %>% 
  spread(sex, n, fill = 0) %>% 
  rowwise() %>% 
  mutate(total = f+m,
         ratio = f/total,
         conf_l = binom.test(f, total)$conf.int[1],
         conf_h = binom.test(f, total)$conf.int[2]) %>% 
  arrange(ratio) %>% 
  ggplot(aes(department, ratio, color = type, ymin = conf_l, ymax = conf_h)) +
  geom_pointrange(position = position_dodge(width = -0.7), size = 1.5)+
  labs(x = "отделение",
       y = "доля женщин в РАН",
       caption = "данные автоматически собраны с сайта ras.ru, 27.04.2019",
       color = "")+
  coord_flip()+
  theme(legend.position = c(0.75, 0.89))+
  geom_text(y = -0.03, aes(label = round(ratio, 2)), position = position_dodge(width = -0.8))+
  ylim(-0.03, 0.46)
  
## 6 Протекающий трубопровод: доля женщин по уровням иерархии
data.frame(cat = factor(c("Россия", "университеты", "кандидаты наук", "доктора", "профессора",
                               "члены-корреспонденты", "директора институтов",
                               "академики"), levels = c("Россия", "университеты", "кандидаты наук", "доктора", "профессора",
                                                        "члены-корреспонденты", "директора институтов",
                                                        "академики")),
                source = factor(c("Статистический ежегодник\n'Народное хозяйство СССР',\n1965 г.", "Статистический ежегодник\n'Народное хозяйство СССР',\n1965 г.",
                                  "ВАК, 2013-2019 гг.", "ВАК, 2013-2019 гг.",
                                  "Сайт РАН, 2018 г.", "Сайт РАН, 2018 г.", "Википедия, 2010 г.",
                                  "Сайт PАН, 2018 г."), levels = c("Статистический ежегодник\n'Народное хозяйство СССР',\n1965 г.",
                                                                   "ВАК, 2013-2019 гг.", "Сайт РАН, 2018 г.", "Википедия, 2010 г.", "Сайт PАН, 2018 г.")),
                m = c("106.1 млн. ч.",     "226.1 тыс. ч.", "5596 ч.",  1461,  401,    852,   209,    442),
                f = c(125.8,     177.7, 4284,  1003,  110,    72,    17,     21),
                f_number = c(125800000,     177700, 4284,  1003,  110,    72,    17,     21),
                m_number = c(106100000, 226100, 5596, 1461,  401,    852,   209,    442),
                total = c("231.9 млн. ч.", "403.9 тыс. ч.", "9880 ч.",  "2464 ч.",  "511 ч.",    "924 ч.",   "226 ч.",    "463 ч.")) %>% 
  rowwise() %>% 
  mutate(ratio = f_number/(f_number+m_number),
         total_number = f_number + m_number,
         conf_l = binom.test(f_number, total_number)$conf.int[1],
         conf_h = binom.test(f_number, total_number)$conf.int[2]) ->
  df


df %>% 
  mutate(annotation = paste0(f, " из ", total)) %>% 
  ggplot(aes(x=cat, y=ratio, ymin = conf_l, ymax = conf_h)) + 
  geom_pointrange() +
  geom_text(aes(y = 0.57, label = annotation))+
  labs(y = "доля женщин",
       x = "",
       color = "") +
  facet_wrap(~source, nrow = 1, scales = "free_x") +
  theme(strip.background=element_rect(fill="white"))+
  geom_hline(data = data.frame(yint = 0.41, source = "Википедия, 2010 г."), 
             aes(yintercept = yint), color = "red", size = 1, linetype = 2)+
  geom_text(y = 0.01, aes(label = round(ratio, 2)))+
  ylim(0, 0.57)
  
## 7 Потерянные исследовательницы: если предположить отсутствие дискриминации, сколько женщин должно быть на каждом уровне иерархии? 

## 8 ОНИТ и Отделение физиологических наук на выборах 2016 года: сколько избиралось, сколько избрали

## 9 Диаграмма по науке и по академикам в разных странах
academy = read.csv("foreign.csv", encoding = "UTF-8")
academy = select(academy, `Страна`, `Всего`, `Женщины`, `Процент.женщин.в.науке`)
academy = na.omit(academy)
colnames(academy) = c("country", "total", "w", "per_sci")
academy = academy %>% filter(country != "Новая Зеландия")
per_sci <- gsub(",", ".", academy$per_sci)
academy$per_sci = per_sci

academy %>%
  rowwise() %>% 
  mutate(percent = w/total,
         conf_l = binom.test(w, total)$conf.int[1],
         conf_h = binom.test(w, total)$conf.int[2]) %>% 
  ggplot() +
  geom_pointrange(aes(x = reorder(country, -percent), y = percent, ymin = conf_l, ymax = conf_h,
                      colour="национальные академии"), 
                  shape = 16, size = 1) +
  geom_point(aes(x = country, y = as.numeric(per_sci),
                 colour="научная сфера"), shape = 17, size = 4) +
  scale_colour_manual(name="",
                      values=c(`национальные академии`="black", `научная сфера`="#E69F00")) +
  labs(x = "",
       y = "доля женщин",
       caption = "данные: The Interacademy Partnership",
       color = "") +
  theme(legend.position="right") +
  coord_flip()+
  geom_text(aes(x = reorder(country, -percent), label = round(percent, 2)), y = 0.01)+
  geom_text(aes(x = reorder(country, -percent), label = round(as.numeric(per_sci), 2)), y = 0.55, color = "#E69F00")+
  ylim(0, 0.55)

## 10 
prof <- read_csv("RAS_proffesors.csv")
prof %>% 
  select(name, year, sex, sci_field) %>%
  mutate(sci_field = str_replace(sci_field, "е ", "х "),
         sci_field = str_replace(sci_field, "науки", "наук"),
         sci_field = ifelse(sci_field == "энергетики, машиностроения, механики и процессов управления",
                            "энергетики, машиностроения,\nмеханики и процессов\nуправления",
                            sci_field),
         sci_field = ifelse(sci_field == "нанотехнологий и информационных технологий",
                            "нанотехнологий\nи информационных технологий",
                            sci_field),
         sci_field = ifelse(sci_field == "глобальных проблем и международных отношений",
                            "глобальных проблем\nи международных отношений",
                            sci_field)) %>% 
  filter(sci_field != "Отделение NA") %>% 
  filter(!is.na(sci_field)) %>% 
  count(sci_field, sex) %>% 
  spread(sex, n, fill = 0) %>% 
  na.omit() %>%
  mutate(total = f+m,
         ratio = f/total,
         sci_field = reorder(sci_field, -ratio)) %>%
  rowwise() %>%
  mutate(conf_l = binom.test(f, total)$conf.int[1],
         conf_h = binom.test(f, total)$conf.int[2]) %>% 
  ggplot(aes(sci_field, ratio, ymin = conf_l, ymax = conf_h)) +
  geom_pointrange(size = 1.5)+
  labs(x = "отделение",
       y = "доля женщин среди професcоров РАН",
       caption = "данные автоматически собраны с сайта ras.ru, 27.04.2019")+
  coord_flip()+
  geom_text(aes(label = round(ratio, 2)), y = -0.04)+
  ylim(-0.04, 1)


# stat._signiff -----------------------------------------------------------
# Разница между Россией и университетами стат значимо
chisq.test(matrix(c(106100000, 125800000, 226100, 177700), nrow = 2))
# X-squared = 17031, df = 1, p-value < 2.2e-16

# Разница между университетами и кандидатами наук НЕ стат значимо
chisq.test(matrix(c(226100, 177700, 5596, 4284), nrow = 2))
# X-squared = 1.6104, df = 1, p-value = 0.2044

# Разница между кандидатами и докторами наук стат значимо
chisq.test(matrix(c(5596, 4284, 1461, 1003), nrow = 2))
# X-squared = 5.566, df = 1, p-value = 0.01831

# Разница между докторами наук и проффесорами РАН стат значимо
chisq.test(matrix(c(1461, 1003, 401, 110), nrow = 2))
# X-squared = 65.674, df = 1, p-value = 5.321e-16

# Разница между проффесорами РАН и членами-корреспондентами стат значимо
chisq.test(matrix(c(401, 110, 852, 72), nrow = 2))
# X-squared = 54.811, df = 1, p-value = 1.327e-13

# Разница между членами-корреспондентами и директорами институтов стат значимо
chisq.test(matrix(c(852, 72, 209, 17), nrow = 2))
# X-squared = 2.0212e-28, df = 1, p-value = 1

# Разница между директорами институтов и академиками НЕ стат значимо
chisq.test(matrix(c(209, 17, 442, 21), nrow = 2))
# X-squared = 2.0578, df = 1, p-value = 0.1514


# hirsh -------------------------------------------------------------------

source <- "http://www.expertcorps.ru/science/whoiswho/ci86?sortby=ddg"
html <- read_html(source)

html %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  tibble(link = .) %>% 
  filter(str_detect(link, "info")) %>% 
  mutate(link = paste0("http://www.expertcorps.ru", link)) ->
  links

links$name <- NA

sapply(seq_along(links$link), function(i){
  print(i)
  html <- read_html(links$link[i])
  html %>% 
    html_nodes("h2") %>% 
    html_text() ->>
    links$name[i]
  html %>% 
    html_nodes("tr.x-desc:nth-child(7)") %>% 
    html_text() ->>
    links$hirsh[i]
})

links %>% 
  mutate(hirsh = str_extract(hirsh, "\\d{1,}"),
         hirsh = as.numeric(hirsh)) %>% 
  rename(link2 = link) ->
  links

df <- read_csv("/home/agricolamz/work/materials/2019.04.27-28_hakathon_novoy_gazeti/RAS_aggregated.csv")

df %>% 
  left_join(links) %>% 
  write_csv("/home/agricolamz/work/materials/2019.04.27-28_hakathon_novoy_gazeti/RAS_aggregated.csv")
