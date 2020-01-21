events2010 <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/2010-present-event.csv", stringsAsFactors = FALSE)
save(events2010, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/2010-present-event.RData")
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/2010-present-event.RData")
events2010$home_team <- substr(events2010$game_id, start=1, stop=3)
events2010$year <- as.numeric(substr(events2010$game_id, start=4, stop=7))
events2010$month <- as.numeric(substr(events2010$game_id, start=8, stop=9))
events2010$day <- as.numeric(substr(events2010$game_id, start=10, stop=11))
events2010$game_number <- as.numeric(substr(events2010$game_id, start=12, stop=12))
save(events2010, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/2010-present-event-new.RData")

events2000 <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/2000-present-event.csv", stringsAsFactors = FALSE)
save(events2000, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/2000-present-event.RData")
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/2000-present-event.RData")
events2000$home_team <- substr(events2000$game_id, start=1, stop=3)
events2000$year <- as.numeric(substr(events2000$game_id, start=4, stop=7))
events2000$month <- as.numeric(substr(events2000$game_id, start=8, stop=9))
events2000$day <- as.numeric(substr(events2000$game_id, start=10, stop=11))
events2000$game_number <- as.numeric(substr(events2000$game_id, start=12, stop=12))
save(events2000, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/2000-present-event-new.RData")

events1990 <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/1990-present-event.csv", stringsAsFactors = FALSE)
save(events1990, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1990-present-event.RData")
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/1990-event.RData")
events1990$home_team <- substr(events1990$game_id, start=1, stop=3)
events1990$year <- as.numeric(substr(events1990$game_id, start=4, stop=7))
events1990$month <- as.numeric(substr(events1990$game_id, start=8, stop=9))
events1990$day <- as.numeric(substr(events1990$game_id, start=10, stop=11))
events1990$game_number <- as.numeric(substr(events1990$game_id, start=12, stop=12))
save(events1990, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1990-present-event-new.RData")

events1980 <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/1980-present-event.csv", stringsAsFactors = FALSE)
save(events1980, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1980-present-event.RData")
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/1980-present-event.RData")
events1980$home_team <- substr(events1980$game_id, start=1, stop=3)
events1980$year <- as.numeric(substr(events1980$game_id, start=4, stop=7))
events1980$month <- as.numeric(substr(events1980$game_id, start=8, stop=9))
events1980$day <- as.numeric(substr(events1980$game_id, start=10, stop=11))
events1980$game_number <- as.numeric(substr(events1980$game_id, start=12, stop=12))
save(events1980, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1980-present-event-new.RData")

events1970 <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/1970-present-event.csv", stringsAsFactors = FALSE)
save(events1970, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1970-present-event.RData")
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/1970-present-event.RData")
events1970$home_team <- substr(events1970$game_id, start=1, stop=3)
events1970$year <- as.numeric(substr(events1970$game_id, start=4, stop=7))
events1970$month <- as.numeric(substr(events1970$game_id, start=8, stop=9))
events1970$day <- as.numeric(substr(events1970$game_id, start=10, stop=11))
events1970$game_number <- as.numeric(substr(events1970$game_id, start=12, stop=12))
save(events1970, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/1970-present-event-new.RData")

# retrosheet
player_manager_coach <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/player_manager_coach.csv", stringsAsFactors = FALSE)
save(player_manager_coach, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/player_manager_coach.RData")

# retrosheet
parkcode <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/parkcode.csv", stringsAsFactors = FALSE)
save(parkcode, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/parkcode.RData")

# retrosheet
current_names <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/current_names.csv", stringsAsFactors = FALSE)
save(current_names, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/current_names.RData")

# retrosheet
event_types <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/event_types.csv", stringsAsFactors = FALSE)
save(event_types, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/event_types.RData")

# retrosheet
positions <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/positions.csv", stringsAsFactors = FALSE)
save(positions, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/positions.RData")

# from wikipedia (https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums#Stadiums)
park_details <- read.csv("/Users/anthony.valencia/met/cs544/project/data/csv/park_details.csv", stringsAsFactors = FALSE)
save(park_details, file = "/Users/anthony.valencia/met/cs544/project/data/rdata/park_details.RData")
