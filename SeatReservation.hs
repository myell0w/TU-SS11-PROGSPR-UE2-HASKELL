-- Programmiersprachen, VL
-- SS 2011
-----------------------------------------
-- Matthias Tretter, 0726390
-- Philip Messlehner
-- Thomas Perl
-----------------------------------------
-- SeatReservation.hs
-----------------------------------------
-- Aufruf: hugs seatReservation.hs
--         Main> main
-----------------------------------------
-- Achtung, mögliche Fehlerquellen:
--    o) Einrückungen beachten
--    o) keine Tabulatoren im Code verwenden sondern nur Leerzeichen
-----------------------------------------


module Main (main)
where


-- Import of Libraries
import Data.List
import Data.Char
import Numeric
import Ix
import Directory (doesFileExist)


-- Constants
kFileName = "seatReservation.txt"


-- Type Definitions
type Station                          = Int
type StartStation                     = Station
type EndStation                       = Station
type WaggonNumber                     = Int
type PersonCount                      = Int
type WaggonCount                      = Int
type SeatNumber                       = Int
type SeatCountPerWaggon               = Int
type FreeSeatsWithoutReservationCount = Int
type TrainName                        = String


type Waggon          = (WaggonNumber, SeatCountPerWaggon)
type Train             = (TrainName, [Waggon], FreeSeatsWithoutReservationCount)

data SeatReservation = SingleReservation SeatNumber |
                          GroupReservation PersonCount deriving (Show, Read, Eq)

type Reservation      = (TrainName, WaggonNumber, StartStation, EndStation, SeatReservation)
type Database         = ([Train], [Reservation])


-- Main Program
main :: IO ()
main = do db <- loadDatabase kFileName
          mainLoop db
          saveDatabase db kFileName

-- does all the work
mainLoop :: Database -> IO ()
mainLoop db@(trains, reservations) = do
    command <- readCommand
    case command of
        1    -> callMinMaxSeats db
        2    -> callSeatReservedForStations db
        3    -> callGroupreservationForStations db
        _    -> putStrLn "Unknown Command!"
    checkAndContinue db command

-- continues the mainLoop as long as the command is != 0
checkAndContinue :: Database -> Int -> IO()
checkAndContinue db command  | command == 0   = putStrLn "End..." 
                             | otherwise      = mainLoop db

-- Loads the database with path fileName and returns it
-- if the file doesn't exists, return an empty database
loadDatabase :: String -> IO Database
loadDatabase fileName = catch (do fileContent <- readFile fileName    -- try to read db
                                  return (read fileContent))        -- cast db to Database
                              (\e -> return ([],[]))                -- on error return empty db (e.g. file doesn't exist)


-- Saves the database db into a file with name fileName
saveDatabase :: Database -> String -> IO ()
saveDatabase db fileName = writeFile fileName (show db)
    

-- reads the next command from stdin (Integer)
readCommand :: IO Int
readCommand = do putStr "Command: "
                 line <- getLine
                 return (read line :: Int)

-- reads the parameters for the call to min/max seats, computes them and prints them to stdout
callMinMaxSeats :: Database -> IO ()
callMinMaxSeats db = do putStr "Enter 'TrainName WaggonNumber StartStation EndStation': "
                        line <- getLine
                        printMinMaxSeats db line

-- calls min/max seats and prints it to stdout
printMinMaxSeats :: Database -> String -> IO ()
printMinMaxSeats db line  = putStrLn ("Min free seats: " ++ (show min) ++ ", max reserved seats: " ++ (show max))
                             -- compute the minimum, extract the parameters from the line that was input by the user
                            where (min,max) = queryMinMaxSeats db (arg (line,0)) (argInt (line,1)) (argInt (line,2)) (argInt (line,3))

callSeatReservedForStations :: Database -> IO ()
callSeatReservedForStations db = do putStr "Enter 'TrainName WaggonNumber SeatNumber': "
                                    line <- getLine
                                    printSeatReservedForStations db line

printSeatReservedForStations :: Database -> String -> IO ()
printSeatReservedForStations db line = putStrLn ("Seat is reserved for Stations: " ++ (concat (map show stations)))
                                       where stations = querySeatReservedForStations db (arg (line,0)) (argInt (line,1)) (argInt (line,2))


callGroupreservationForStations :: Database -> IO ()
callGroupreservationForStations db = do putStr "Enter 'TranName WaggonNumber': "
                                        line <- getLine
                                        printGroupereservationForStations db line

printGroupereservationForStations :: Database -> String -> IO ()
printGroupereservationForStations db line = putStrLn ("There exist following Groupreservations: " ++ (concat (map show groupres)))
                                            where groupres = queryGroupreservationForStations db (arg (line,0)) (argInt (line,1))

-- ########################
-- Selectors
-- ########################

-- get the word with index idx of the line and return it as string
arg :: (String,Int) -> String
arg (line,idx)  = (words line)!!idx

-- get the word with index idx of the line and return it as Int
argInt :: (String,Int) -> Int
argInt (line,idx)  = read ((words line)!!idx)::Int

-- Name of the Train
name :: Train -> TrainName
name (n,_,_)  =  n

-- Array of Waggons of the Train
waggons :: Train -> [Waggon]
waggons (_,w,_)  = w

-- Number offree Seats per Train that must retain without reservation
freeSeats :: Train -> FreeSeatsWithoutReservationCount
freeSeats (_,_,s)  =  s

-- Identifier for Waggon
waggonNumber :: Reservation -> WaggonNumber
waggonNumber (_,nr,_,_,_)    = nr

-- Startstation of Reservation
startStation :: Reservation -> StartStation
startStation (_,_,s,_,_)    = s

-- EndStation of Reservation
endStation :: Reservation -> EndStation
endStation (_,_,_,s,_)    = s

-- reserved Seats for Reservation
reservedSeats :: Reservation -> Int
reservedSeats (_, _, _, _, (SingleReservation _))   = 1           -- single reservations only reserve one seat
reservedSeats (_,_,_,_,(GroupReservation n))        = n           -- group reservation: number of People

-- seatNumber for Reservation
seatNumber :: Reservation -> Int
seatNumber (_, _, _, _, (SingleReservation s)) =  s
seatNumber (_, _, _, _, (GroupReservation _)) =  -1

groupSize :: Reservation -> Int
groupSize (_, _, _, _, (GroupReservation s)) =  s
groupSize (_, _, _, _, (SingleReservation _)) = 0

allStations :: Reservation -> [Station]
allStations (_, _, startStation, endStation, _) = range (startStation, endStation-1)

groupSizeAndStations :: Reservation -> (Int, [Station])
groupSizeAndStations r = (groupSize(r), allStations(r))

-- ########################
-- Queries
-- ########################


-- returns the number of reserved seats for the combination (Waggon,Station)
reservedSeatsForWaggonInStation :: [Reservation] -> WaggonNumber -> Station -> Int
                                                              -- get the array of reservations for the given waggonNr, compute the reserved seats for each reservation and sum up
                                                              -- a reservation counts for a station if the startStation was lower or equal than the queried station and the endStation was greater
                                                              -- that means a reservation from station 2 to 5 doesn't count for station 5 since the passangers leave the train there
reservedSeatsForWaggonInStation reservations waggonNr station  = sum (map reservedSeats [r | r <- reservations, waggonNumber(r) == waggonNr, startStation(r) <= station, endStation(r) > station])

-- queries the minimum count of free seats between two stations
queryMinMaxSeats :: Database -> TrainName -> WaggonNumber -> StartStation -> EndStation -> (Int,Int)
queryMinMaxSeats (_,[]) _ _ _ _                                                      = (0,0) -- No reservations means no seats
queryMinMaxSeats db@(trains,reservations) trainName waggonNr startStation endStation = (snd(waggon) - reservedSeatsInWaggon, reservedSeatsInWaggon)
      where train = [t | t <- trains, name(t) == trainName]!!0            -- get train with given Name
            waggon = [w | w <- waggons(train), fst(w) == waggonNr]!!0     -- get waggon of train with given Number
                 -- iterate through Stations and calculate Number of reserved Seats per Station
            reservedSeatsInWaggon = maximum (map (reservedSeatsForWaggonInStation reservations waggonNr) (range (startStation, endStation)))
            
querySeatReservedForStations :: Database -> TrainName -> WaggonNumber -> SeatNumber -> [Station]
querySeatReservedForStations (_,[]) _ _ _ = []
querySeatReservedForStations db@(trains,reservations) trainName waggonNr seatNr = if res == [] then nub (concat (map allStations res)) else []
      where train = [t | t <- trains, name(t) == trainName]!!0            -- get train with given Name
            waggon = [w | w <- waggons(train), fst(w) == waggonNr]!!0     -- get waggon of train with given Number
            res = [r | r <- reservations, seatNumber(r) == seatNr, waggonNumber(r) == waggonNr]

queryGroupreservationForStations :: Database -> TrainName -> WaggonNumber -> [(Int, [Station])]
queryGroupreservationForStations (_,[]) _ _ = []			           
queryGroupreservationForStations db@(trains,reservations) trainName waggonNr = (map groupSizeAndStations res)
     where train = [t | t <- trains, name(t) == trainName]!!0            -- get train with given Name
           waggon = [w | w <- waggons(train), fst(w) == waggonNr]!!0     -- get waggon of train with given Number
           res = [r | r <- reservations, groupSize(r) > 0, waggonNumber(r) == waggonNr]