## codeshare ------- https://codeshare.io/2jYq3D

## Mexico, South Africa, Argentina, Turkey 

leagues_list <- c("https://www.soccerway.com/national/argentina/primera-division/20182019/regular-season/r47779/",
                  "https://www.soccerway.com/national/south-africa/psl/20182019/regular-season/r47760/",
                  "https://www.soccerway.com/national/mexico/primera-division/20182019/clausura/r48397/",
                  "https://www.soccerway.com/national/turkey/super-lig/20182019/regular-season/r48404/?ICID=TN_02_01_07")


league <- leagues_list[1]

## load packages 
require(tidyverse)
require(rvest)

## grab club url and league info 

club_information <- data.frame(stringsAsFactors = F)

for (league in leagues_list) {
  ws <- read_html(league)
  clubURL <- paste0("https://www.soccerway.com", 
         ws %>% html_nodes("#page_competition_1_block_competition_tables_7_block_competition_league_table_1_table .large-link a") %>%
        html_attr("href") %>% as.character(), "squad/")
  League <- ws %>% html_node("h1") %>% html_text() %>% as.character()
  club_info <- data.frame(url = clubURL, stringsAsFactors = F)
  club_info$League <- League
  club_information <- bind_rows(club_information, club_info)
  cat(".")
}

## grab the player information 

squad_database <- data.frame(stringsAsFactors = F)

for (cluburl in club_information$url) {
    ws <- read_html(cluburl)
    tables <- ws %>% html_table()
    table <- tables[[1]]
  
    if (NCOL(table) == 17){
    colnames(table) <- c("squad_number",
                         "spare",
                         "player",
                         "spare2",
                         "age",
                         "position",
                         "mins_played",
                         "apps",
                         "in_squad",
                         "sub_in",
                         "sub_out",
                         "warmed_bench",
                         "goals",
                         "assists",
                         "yc",
                         "yc2",
                         "rd")}else{
  
      colnames(table) <- c("squad_number",
                         "spare",
                         "player",
                         "spare2",
                         "age",
                         "position",
                         "mins_played",
                         "apps",
                         "in_squad",
                         "sub_in",
                         "sub_out",
                         "warmed_bench",
                         "goals",
                         "yc",
                         "yc2",
                         "rd")}
    table$club <- ws %>% html_node("h1") %>% html_text() %>% as.character()
    table$url <- cluburl
    table <- table %>% select(-spare, -spare2)
    squad_database <- bind_rows(squad_database, table)
    print(table)
}

### converting goals to assits to per 90 metrics 
#save <- squad_database

squad_database <- squad_database %>% mutate(g90 = (goals / mins_played) * 90,
                          a90 = (assists / mins_played) * 90,
                          sc90 = g90 + a90)

squad_database[is.na(squad_database)] <- 0

squad_database <- squad_database %>% mutate(g90 = round(g90, 2),
                          a90 = round(a90,2),
                          sc90 = round(sc90,2))

## add the leagues 

squad_database <- merge(squad_database, club_information, by="url") 
squad_database <- squad_database %>% select(-url)


### Simple Filters 
young_ones <- squad_database %>% filter(age <= 21 & mins_played > 630)


### Grab global ratings 
page <- "https://projects.fivethirtyeight.com/global-club-soccer-rankings/"
ws <- read_html(page)

tables <- ws %>% html_table()
rankings <- tables[[1]]

colnames(rankings) <- c("rank", "week_change", "club",
                        "league", "league_country", "off", "def", "spi")

rankings <- rankings[-1,]

rankings <- rankings %>% mutate(rank = as.numeric(as.character(rank)),
                    off = as.numeric(as.character(off)),
                    def = as.numeric(as.character(def)),
                    spi = as.numeric(as.character(spi)))

### reduce the rankings

countries_in_scope <- c("Argentina", "Mexico", "South Africa", "Turkey")
rankings <- rankings %>% filter(league_country %in% countries_in_scope)

##
## change the club names 
squad_database$club <- gsub("CA ", "", squad_database$club)
squad_database$club <- gsub("CD ", "", squad_database$club)
squad_database$club <- gsub("CF ", "", squad_database$club)
squad_database$club <- gsub("CSD ", "", squad_database$club)
squad_database$club <- gsub(" FC", "", squad_database$club)
rankings$club <- gsub("s UANL", "", rankings$club)
rankings$club <- gsub("León", "Club León", rankings$club)
squad_database$club <- gsub("Racing Club de Avellaneda", "Racing", squad_database$club)
squad_database$club <- gsub("Cruz Azul FC", "Cruz Azul", squad_database$club)
squad_database$club <- gsub("Club Santos Laguna", "Santos Laguna", squad_database$club)
squad_database$club <- gsub("Club Atlético Vélez Sarsfield", "Vélez Sarsfield", squad_database$club)
squad_database$club <- gsub("Talleres de Córdoba", "Talleres", squad_database$club)
squad_database$club <- gsub("Unión de Santa Fe", "Unión", squad_database$club)
squad_database$club <- gsub("Deportivo Toluca", "Toluca", squad_database$club)
squad_database$club <- gsub("Tucumán", "Atlético Tucumán", squad_database$club)
squad_database$club <- gsub("Club Necaxa", "Necaxa", squad_database$club)
squad_database$club <- gsub("Club Tijuana Xoloitzcuintles de Caliente", "Tijuana", squad_database$club)
squad_database$club <- gsub("Pumas de la Universidad Nacional Autonoma de Mexico", "Pumas UNAM", squad_database$club)
squad_database$club <- gsub("Colón de Santa Fe", "Colón", squad_database$club)
squad_database$club <- gsub("San Lorenzo de Almagro", "San Lorenzo", squad_database$club)
squad_database$club <- gsub("Club Estudiantes de La Plata", "Estudiantes", squad_database$club)
squad_database$club <- gsub("Godoy Cruz Antonio Tomba", "Godoy Cruz", squad_database$club)
squad_database$club <- gsub("Puebla FC", "Puebla", squad_database$club)
squad_database$club <- gsub("San Martín de San Juan", "San Martín", squad_database$club)
squad_database$club <- gsub("CSyD Atlas de Guadalajara", "Atlas", squad_database$club)
squad_database$club <- gsub("Mamelodi Sundowns", "M. Sundowns", squad_database$club)
squad_database$club <- gsub("Belgrano de Córdoba", "Belgrano", squad_database$club)
squad_database$club <- gsub("Monarcas Morelia", "Morelia", squad_database$club)
squad_database$club <- gsub("San Martín de Atlético Tucumán", "San Martín T.", squad_database$club)
squad_database$club <- gsub("Lobos de la BUAP", "Lobos BUAP", squad_database$club)
squad_database$club <- gsub("Gimnasia y Esgrima La Plata", "Gimnasia", squad_database$club)
squad_database$club <- gsub("Tiburones Rojos de Veracruz", "Veracruz", squad_database$club)
squad_database$club <- gsub("Patronato de la Juventud Católica", "Patronato", squad_database$club)
squad_database$club <- gsub("SuperSport United", "SuperSport Utd", squad_database$club)
squad_database$club <- gsub("Lamontville Golden Arrows", "Golden Arrows", squad_database$club)
squad_database$club <- gsub("Bloemfontein Celtic", "Bloem Celtic", squad_database$club)
squad_database$club <- gsub(" Spor Kulübü", "", squad_database$club)
squad_database$club <- gsub(" Jimnastik Kulübü", "", squad_database$club)
squad_database$club <- gsub("  Futbol Kulübü", "", squad_database$club)
squad_database$club <- gsub(" Kulübü", "", squad_database$club)
squad_database$club <- gsub(" Futbol", "", squad_database$club)
squad_database$club <- gsub("İstanbul Başakşehir", "Başakşehir", squad_database$club)
squad_database$club <- gsub("Atiker Konyaspor", "Konyaspor", squad_database$club)
squad_database$club <- gsub("Çaykur Rize", "Rizespor", squad_database$club)
squad_database$club <- gsub("Yeni Malatya", "Yeni Malatyaspor", squad_database$club)
squad_database$club <- gsub("Teleset Mobilya Akhisar", "Akhisarspor", squad_database$club)
squad_database$club <- gsub("Büyükşehir Belediye Erzurum", "Erzurumspor", squad_database$club)
squad_database$club <- gsub("Bursaspor Derneği", "Bursaspor", squad_database$club)
squad_database$club <- gsub("Kayseri", "Kayserispor", squad_database$club)
squad_database$club <- gsub("MKE Ankaragücü", "Ankaragücü", squad_database$club)

rankings$club %in% unique(squad_database$club)

squad_database <- merge(squad_database, rankings %>% select(-league_country, -week_change), by="club")

#### 

labels_df <- squad_database %>% filter(age <=21, mins_played > 1800, position %in% c("M", "A"))

ggplot() + 
  geom_point(data = squad_database, aes(x = off, y = mins_played), colour = "grey") + 
  geom_point(data = squad_database %>% filter(age <=21), aes(x = off, y = mins_played), colour = "red") + 
  theme_minimal() + annotate("text", x = labels_df$off, y = labels_df$mins_played, label=labels_df$player)


labels_df2 <- squad_database %>% filter(age <=21, mins_played > 600, sc90 >= 0.48, position %in% c("M", "A"))

ggplot() + geom_point(data = squad_database %>% filter(mins_played >= 600), aes(x = off, y = sc90), colour = "grey") + 
  geom_point( data = squad_database %>% filter(mins_played >= 600 & age <= 21), aes(x = off, y = sc90, size = mins_played), colour = "red") + 
 annotate("text", x = labels_df2$off, y = labels_df2$sc90, label=labels_df2$player)

