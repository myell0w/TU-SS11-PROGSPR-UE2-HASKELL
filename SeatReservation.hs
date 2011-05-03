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
        0    -> putStrLn "Beenden..." 
        1    -> callMinFreeSeats db
        _    -> mainLoop db



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

-- reads the parameters for the call to min free seats, computes them and prints them to stdout
callMinFreeSeats :: Database -> IO ()
callMinFreeSeats db = do line <- readMinFreeSeats db
                         printMinFreeSeats db line

-- reads a line from stdin containing the parameters for the call to min free seats
readMinFreeSeats :: Database -> IO String
readMinFreeSeats db = do putStr "Enter 'TrainName WaggonNr StartStation EndStation': "
                         line <- getLine
                         return line

-- calls min free seats and prints it to stdout
printMinFreeSeats :: Database -> String -> IO ()
printMinFreeSeats db line  = putStrLn ("Min free seats: " ++ (show min))
                             -- compute the minimum, extract the parameters from the line that was input by the user
                             where min = queryMinFreeSeats db (arg (line,0)) (argInt (line,1)) (argInt (line,2)) (argInt (line,3))

-- Selectors

-- get the word with index idx of the line and return it as string
arg :: (String,Int) -> String
arg (line,idx)  = (words line)!!idx

-- get the word with index idx of the line and return it as Int
argInt :: (String,Int) -> Int
argInt (line,idx)  = read ((words line)!!idx)::Int

name :: Train -> TrainName
name (n,_,_)  =  n

waggons :: Train -> [Waggon]
waggons (_,w,_)  = w

freeSeats :: Train -> FreeSeatsWithoutReservationCount
freeSeats (_,_,s)  =  s

waggonNumber :: Reservation -> WaggonNumber
waggonNumber (_,nr,_,_,_)    = nr

startStation :: Reservation -> StartStation
startStation (_,_,s,_,_)    = s

endStation :: Reservation -> EndStation
endStation (_,_,_,s,_)    = s

reservedSeats :: Reservation -> Int
reservedSeats (_, _, _, _, (SingleReservation _))   = 1           -- single reservations only reserve one seat
reservedSeats (_,_,_,_,(GroupReservation n))        = n


-- returns the number of reserved seats for the combination (Waggon,Station)
reservedSeatsForWaggonInStation :: [Reservation] -> WaggonNumber -> Station -> Int
                                                              -- get the array of reservations for the given waggonNr, compute the reserved seats for each reservation and sum up
                                                              -- a reservation counts for a station if the startStation was lower or equal than the queried station and the endStation was greater
                                                              -- that means a reservation from station 2 to 5 doesn't count for station 5 since the passangers leave the train there
reservedSeatsForWaggonInStation reservations waggonNr station  = sum (map reservedSeats [r | r <- reservations, waggonNumber(r) == waggonNr, startStation(r) <= station, endStation(r) > station])

-- queries the minimum count of free seats between two stations
queryMinFreeSeats :: Database -> TrainName -> WaggonNumber -> StartStation -> EndStation -> Int
queryMinFreeSeats (_,[]) _ _ _ _      = 0
queryMinFreeSeats db@(trains,reservations) trainName waggonNr startStation endStation = snd(waggon) - reservedSeatsInWaggon
      where train = [t | t <- trains, name(t) == trainName]!!0
            waggon = [w | w <- waggons(train), fst(w) == waggonNr]!!0
            reservedSeatsInWaggon = maximum (map (reservedSeatsForWaggonInStation reservations waggonNr) (range (startStation, endStation)))
            