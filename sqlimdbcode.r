library(RSQLite)

lean = dbConnect(drv = SQLite(), dbname = 'lean_imdbpy_2010_idx.db')

genre.year <- dbGetQuery( lean, 'SELECT title2.production_year, movie_info2.info, COUNT( *)
                          FROM movie_info2, title2
                          WHERE info_type_id = 3 
                          AND movie_info2.movie_id = title2.id 
                          AND title2.kind_id = 1
                          GROUP BY title2.production_year, movie_info2.info;')

genre.list <- unique( genre.year[, 2])
gy <- lapply(1:28, function(p){ 
  genre.year[ which( genre.year[, 2] == genre.list[ p]), c( 1, 3)]})
list.totals <- sapply(1:28, function(x) sum(gy[[x]][,2]))
ord.total <- order( list.totals)

# Distinct person_id from cast_info2 because it maps to names2 which has 1 entry per actor
actor.count <- dbGetQuery( lean, 'SELECT COUNT( DISTINCT( person_id)) FROM cast_info2 
                           WHERE role_id = 1 OR role_id = 2;')

# Don't distinct, raw count all titles that correlate as a movie
movie.count <- dbGetQuery( lean, 'SELECT COUNT( *) FROM title2 
                           WHERE kind_id = 1;')

#get range of movie years from title table
dbGetQuery( lean, 'SELECT MIN(production_year), MAX(production_year) FROM title2')


male <- dbGetQuery( lean, 'SELECT COUNT( DISTINCT( person_id)) 
                           FROM name2, cast_info2 
                    WHERE name2.id = cast_info2.person_id 
                    AND  gender = "m" 
                    AND ( cast_info2.role_id = 1 OR cast_info2.role_id = 2);')
female <- actor.count - male
female <- female / actor.count
male <- male / actor.count

kind.freq <- dbGetQuery( lean, 'SELECT kind_id, COUNT( *) FROM title2 GROUP BY kind_id')
kind.tot <- sum( kind.freq[, 2])
mov.freq <- kind.freq[1, 2] / kind.tot
tv.freq <- kind.freq[2, 2] / kind.tot

genre.inf <- dbGetQuery( lean, 'SELECT DISTINCT( info) 
                         FROM movie_info2 
                         WHERE info_type_id = 3;')
count.genre <- dim( genre.inf)[1]

top.gen <-
  dbGetQuery( lean, 'SELECT DISTINCT( info) Genres, COUNT( *) Frequency
                   FROM movie_info2, title2
                   WHERE info_type_id = 3 
                      AND movie_info2.movie_id = title2.id 
                      AND (title2.kind_id = 1 OR title2.kind_id = 3 OR title2.kind_id = 4)
                   GROUP BY info 
                   ORDER BY COUNT( *) DESC 
                   LIMIT 10;')

TopSpace <-
  dbGetQuery( lean, 'SELECT DISTINCT(name2.name) Name, title Title, production_year Year
              FROM movie_keyword2, keyword2, title2, cast_info2, name2
              WHERE movie_keyword2.keyword_id = keyword2.id
              AND movie_keyword2.movie_id = title2.id
              AND movie_keyword2.movie_id = cast_info2.movie_id
              AND cast_info2.person_id = name2.id
              AND keyword2.keyword = "space"
              AND nr_order BETWEEN 1 AND 5
              AND (role_id = 1 OR role_id = 2) 
              AND (kind_id = 1 OR kind_id = 3 OR kind_id = 4) 
              LIMIT 20;')

#genre pop over time
genre.year <- dbGetQuery( lean, 'SELECT title2.production_year, movie_info2.info, COUNT( *)
                                 FROM movie_info2, title2
                          WHERE movie_info2.movie_id = title2.id 
                          AND info_type_id = 3 
                          AND title2.kind_id = 1
                          GROUP BY title2.production_year, movie_info2.info;')

genre.year <- genre.year[ - which( is.na( genre.year[ ,1])), ]
genre.list <- unique( genre.year[, 2])
gy <- lapply(1:28, function(p){ 
  genre.year[ which( genre.year[, 2] == genre.list[ p]), c( 1, 3)]})
list.totals <- sapply(1:28, function(x) sum(gy[[x]][,2]))
ord.total <- order( list.totals)

plot1 <-
  plot( gy[[22]], type = "l", main = "Plot: 1st Tier of Genres -- Frequency over Time", 
        xlab = "Year", ylab = "Frequency")
points( gy[[9]], type = "l", lty = 2)
points( gy[[6]], type = "l", lty = 3)
points( gy[[8]], type = "l", lty = 4)
points( gy[[25]], type = "l", lty = 5)
legend( x = "topright", legend = genre.list[ ord.total[ 28:24]], lty = 1:5, cex = .65)

plot2 <-
  plot( gy[[14]], type = "l", main = "Plot: 2nd Tier of Genres -- Frequency over Time",
        xlab = "Year", ylab = "Frequency")
points( gy[[20]], type = "l", lty = 2)
points( gy[[1]], type = "l", lty = 3)
points( gy[[10]], type = "l", lty = 4)
points( gy[[11]], type = "l", lty = 5)
points( gy[[5]], type = "l", lty = 6)
points( gy[[17]], type = "l", lty = 7)
legend( x = "topright", legend = genre.list[ ord.total[ 23:17]], lty = 1:7, cex = .575)

plot3 <-
  plot( gy[[21]], type = "l", main = "Plot: 3rd Tier of Genres -- Frequency over Time",
        xlab = "Year", ylab = "Frequency")
points( gy[[4]], type = "l", lty = 2)
points( gy[[7]], type = "l", lty = 3)
points( gy[[3]], type = "l", lty = 4)
points( gy[[15]], type = "l", lty = 5)
points( gy[[13]], type = "l", lty = 6)
legend( x = "topright", legend = genre.list[ ord.total[ 16:11]], lty = 1:6, cex = .6)

plot4 <-  plot( gy[[18]], type = "l", main = "Plot: 4th Tier of Genres -- Frequency over Time",
                xlab = "Year", ylab = "Frequency")
points( gy[[26]], type = "l", lty = 2)
points( gy[[23]], type = "l", lty = 3)
points( gy[[16]], type = "l", lty = 4)
points( gy[[27]], type = "l", lty = 5)
points( gy[[2]], type = "l", lty = 6)
points( gy[[19]], type = "l", lty = 7)
points( gy[[24]], type = "l", lty = 8)
points( gy[[12]], type = "l", lty = 9)
points( gy[[27]], type = "l", lty = 10)
legend( x = "topright", legend = genre.list[ ord.total[ 10:1]], lty = 1:10, cex = .475)

#actors in the most movies
top.act <-
  dbGetQuery( lean, 'SELECT person_id, COUNT(*), name 
              FROM  cast_info2, name2, title2 
              WHERE role_id BETWEEN 1 AND 2 
              AND cast_info2.person_id = name2.id
              AND cast_info2.movie_id = title2.id
              AND title2.kind_id = 1
              GROUP BY person_id ORDER BY COUNT(*) DESC LIMIT 20;')

#who has the most top billing
dbGetQuery( lean, 'SELECT person_id, COUNT(*), name, MIN(production_year), MAX(production_year) 
            FROM  cast_info2, name2, title2 
            WHERE role_id BETWEEN 1 AND 2 
            AND cast_info2.person_id = name2.id
            AND cast_info2.movie_id = title2.id
            AND title2.kind_id = 1
            AND (nr_order = 1 OR nr_order = 2 OR nr_order = 3)
            GROUP BY person_id ORDER BY COUNT(*) DESC LIMIT 20;')

#most aliases
dbGetQuery( lean, 'SELECT aka_name2.person_id, COUNT(*), name2.name 
            FROM  cast_info2, name2, title2, aka_name2 
            WHERE role_id BETWEEN 1 AND 2 
            AND cast_info2.person_id = name2.id
            AND cast_info2.movie_id = title2.id
            AND aka_name2.person_id = name2.id
            AND title2.kind_id = 1
            GROUP BY aka_name2.person_id ORDER BY COUNT(*) DESC LIMIT 10;')

#shows with most movie stars
dbGetQuery( lean, 'SELECT COUNT( DISTINCT( person_id)) Count, title 
            FROM  cast_info2, title2 
            WHERE role_id BETWEEN 1 AND 2 
            AND cast_info2.movie_id = title2.id
            AND title2.kind_id = 2
            AND (nr_order = 1 OR nr_order = 2 OR nr_order = 3)
            GROUP BY title ORDER BY Count DESC LIMIT 20;')