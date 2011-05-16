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


type Waggon           = (WaggonNumber, SeatCountPerWaggon)
type Train            = (TrainName, [Waggon], FreeSeatsWithoutReservationCount)

data SeatReservation  = SingleReservation SeatNumber |
                        GroupReservation PersonCount deriving (Show, Read, Eq)

type Reservation      = (TrainName, WaggonNumber, StartStation, EndStation, SeatReservation)
type Database         = ([Train], [Reservation])


-- Main Program
main :: IO ()
main = do db <- loadDatabase kFileName
          mainLoop db

-- does all the work
mainLoop :: Database -> IO ()
mainLoop db@(trains, reservations) = do
    command <- readCommand
    case (argInt (command, 0)) of
        1    -> callMinMaxSeats db (tail command)
        2    -> callSeatReservedForStations db (tail command)
        3    -> callGroupreservationForStations db (tail command)
        4    -> putStr "Making reservation..."
        5    -> putStr "Making reservation..."
        6    -> callPrintDB db
        0    -> putStr "Ending ...Bye bye!"
        _    -> putStrLn "Unknown Command!"
    let newDB = case (argInt (command, 0)) of
	    4    -> callMakeSingleReservation db (tail command)
	    5    -> callMakeGroupReservation db (tail command)
	    _    -> db
    if (argInt (command, 0)) /= 0 then mainLoop newDB else saveDatabase newDB kFileName

-- Loads the database with path fileName and returns it
-- if the file doesn't exists, return an empty database
loadDatabase :: String -> IO Database
loadDatabase fileName = catch (do fileContent <- readFile fileName    -- try to read db
                                  return (read fileContent))          -- cast db to Database
                              (\e -> return ([],[]))                  -- on error return empty db (e.g. file doesn't exist)


-- Saves the database db into a file with name fileName
saveDatabase :: Database -> String -> IO ()
saveDatabase db fileName = writeFile fileName (show db)
    

-- reads the next command from stdin (Integer)
readCommand :: IO String
readCommand = do putStrLn ""
                 putStrLn "1 .. Show minimum of free seats and maximum of occupied seats (Query 1)"
                 putStrLn "     Enter 'TrainName StartStation EndStation WaggonNumber'"
                 putStrLn "2 .. Show statistics for specific seat in a waggon (Query 2)"
                 putStrLn "     Enter 'TrainName WaggonNumber SeatNumber'"
                 putStrLn "3 .. Show statistics of group reservations for a specific waggon (Query 3)"
                 putStrLn "     Enter 'TrainName WaggonNumber'"
                 putStrLn "4 .. Make a single reservation"
                 putStrLn "     Enter 'TrainName WaggonNumber StartStation EndStation SeatNumber'"
                 putStrLn "5 .. Make a group reservation"
                 putStrLn "     Enter 'TrainName WaggonNumber StartStation EndStation GroupSize'"
                 putStrLn "6 .. Print Database"
                 putStrLn "0 .. Quit"
                 putStrLn "-----------------------------------------------------------------------------"
                 putStr "Command: "
                 line <- getLine
                 putStrLn ""
                 return line


-- calls min/max seats and prints it to stdout
callMinMaxSeats :: Database -> String -> IO ()
callMinMaxSeats db line  = putStrLn ("Min free seats: " ++ (show min) ++ ", max reserved seats: " ++ (show max))
                             -- compute the minimum, extract the parameters from the line that was input by the user
                            where (min,max) = queryMinMaxSeats db (arg (line,0)) (argInt (line,1)) (argInt (line,2)) (argInt (line,3))

callSeatReservedForStations :: Database -> String -> IO ()
callSeatReservedForStations db line = putStrLn ("Seat is reserved for Stations: " ++ (concat (map show stations)))
                                      where stations = querySeatReservedForStations db (arg (line,0)) (argInt (line,1)) (argInt (line,2))


callGroupreservationForStations :: Database -> String -> IO ()
callGroupreservationForStations db line = putStrLn ("There exist following Groupreservations: " ++ (concat (map show groupres)))
                                            where groupres = queryGroupreservationForStations db (arg (line,0)) (argInt (line,1))

callMakeSingleReservation :: Database -> String -> Database
callMakeSingleReservation db@(trains,reservations) line =  if isValid == True then (trains,reservations++[reservation]) else db
                                     where reservation = (arg (line,0), argInt (line,1), argInt (line,2), argInt (line,3), (SingleReservation (argInt (line,4))))
                                           isValid     = checkReservation db reservation

callMakeGroupReservation :: Database -> String -> Database
callMakeGroupReservation db@(trains,reservations) line =  if isValid == True then (trains,reservations++[reservation]) else db
                                     where reservation = (arg (line,0), argInt (line,1), argInt (line,2), argInt (line,3), (GroupReservation (argInt (line,4))))
                                           isValid     = checkReservation db reservation

callPrintDB :: Database -> IO ()
callPrintDB db@(trains,reservations) = do
            putStrLn ("-- Trains: --")
            putStrLn (callPrintTrains trains)
            putStrLn ("-- Reservations: --")
            putStrLn (callPrintReservations reservations)

callPrintTrains:: [Train] -> [Char]
callPrintTrains []     = []
callPrintTrains (s:xs) = "    " ++ (show s) ++ "\n" ++ (callPrintTrains xs)

callPrintReservations:: [Reservation] -> [Char]
callPrintReservations []     = []
callPrintReservations (s:xs) = "    " ++ (show s) ++ "\n" ++ (callPrintReservations xs)

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

-- Train of Reservation
trainForReservation :: Reservation -> TrainName
trainForReservation (t,_,_,_,_) = t

-- reserved Seats for Reservation
reservedSeats :: Reservation -> Int
reservedSeats (_, _, _, _, (SingleReservation _))   = 1           -- single reservations only reserve one seat
reservedSeats (_,_,_,_,(GroupReservation n))        = n           -- group reservation: number of People

-- seatNumber for Reservation
seatNumber :: Reservation -> Int
seatNumber (_, _, _, _, (SingleReservation s)) =  s
-- group reservations have no seat number assigned
seatNumber (_, _, _, _, (GroupReservation _)) =  -1


groupSize :: Reservation -> Int
groupSize (_, _, _, _, (GroupReservation s)) =  s
-- single reservations have a group size of 0
groupSize (_, _, _, _, (SingleReservation _)) = 0

allStations :: Reservation -> [Station]
allStations (_, _, startStation, endStation, _) = range (startStation, endStation-1)

groupSizeAndStations :: Reservation -> (Int, [Station])
groupSizeAndStations r = (groupSize(r), allStations(r))

-- ########################
-- Queries
-- ########################

-- returns the number of reserved seats for the combination (Train,Waggon,Station)
reservedSeatsForWaggonInStation :: [Reservation] -> TrainName -> WaggonNumber -> Station -> Int
                                                              -- get the array of reservations for the given waggonNr, compute the reserved seats for each reservation and sum up
                                                              -- a reservation counts for a station if the startStation was lower or equal than the queried station and the endStation was greater
                                                              -- that means a reservation from station 2 to 5 doesn't count for station 5 since the passangers leave the train there
reservedSeatsForWaggonInStation reservations trainName waggonNr station  = sum (map reservedSeats [r | r <- reservations, trainForReservation r == trainName, waggonNumber r == waggonNr, startStation r <= station, endStation r > station])

-- queries the minimum count of free seats and the maximum count of reserved seats between two stations
queryMinMaxSeats :: Database -> TrainName -> StartStation -> EndStation -> WaggonNumber -> (Int,Int)
queryMinMaxSeats (_,[]) _ _ _ _                                                      = (0,0) -- No reservations means no seats
 -- snd waggon gets the number of seats in the waggon
queryMinMaxSeats db@(trains,reservations) trainName startStation endStation waggonNr = (snd(waggon) - reservedSeatsInWaggon, reservedSeatsInWaggon)
      where train = [t | t <- trains, name(t) == trainName]!!0            -- get train with given Name
            waggon = [w | w <- waggons(train), fst(w) == waggonNr]!!0     -- get waggon of train with given Number
                 -- iterate through Stations and calculate Number of reserved Seats per Station and get the maximum of it
            reservedSeatsInWaggon = maximum (map (reservedSeatsForWaggonInStation reservations trainName waggonNr) (range (startStation, endStation)))
         
-- queries all stations, for which the seatNumber int the waggon is reserved   
querySeatReservedForStations :: Database -> TrainName -> WaggonNumber -> SeatNumber -> [Station]
querySeatReservedForStations (_,[]) _ _ _ = []
 -- if there are reservations for the given seat in the given waggon then get a duplicate-free array of all stations
querySeatReservedForStations db@(trains,reservations) trainName waggonNr seatNr = if res /= [] then nub (concat (map allStations res)) else []
      where res = [r | r <- reservations, seatNumber(r) == seatNr, waggonNumber(r) == waggonNr, trainForReservation r == trainName] -- get all reservations for this seat in this waggon

-- queries the pair (size of group reservation, array of stations) for all group reservations of a given waggon
-- we don't remove duplicates by design to not loose the information of the stations associated to the group size
-- that means there can be more pairs for the same stations, e.g. [(3,[1,2,3]), (2,[1,2,3]), (4,[1,2])]
queryGroupreservationForStations :: Database -> TrainName -> WaggonNumber -> [(Int, [Station])]
queryGroupreservationForStations (_,[]) _ _ = []
queryGroupreservationForStations db@(trains,reservations) trainName waggonNr = (map groupSizeAndStations res)
     where res = [r | r <- reservations, groupSize(r) > 0, waggonNumber(r) == waggonNr, trainForReservation r == trainName] -- all group reservations for waggon

-- ########################
-- Reservation-Queries
-- ########################

-- checks if the new reservation, that reserves 'countSeatsForReservation' doesn't collide with the min free seats that 
-- must stay free in the train.
checkMinFreeSeatsInTrainAvailable :: Database -> TrainName -> StartStation -> EndStation -> Int -> Bool   
 -- queries the min free seats between start and end and sums them up for all waggons
 -- first queryMinMaxSeats gets mapped onto all waggonNumbers, fst gets mapped to get the min free seats and these are summed up
 -- if these sum is greater or equal to the seats that must stay free in the train + the number of seats we want to reserve the requirement isn't violated
checkMinFreeSeatsInTrainAvailable db@(trains,reservations) trainName start end countSeatsForReservation = sum (map fst (map (queryMinMaxSeats db trainName start end) waggonNumbers)) >= freeSeats train + countSeatsForReservation
     where train = [t | t <- trains, name(t) == trainName]!!0            -- get train with given Name
           waggonNumbers = [fst w | w <- waggons train]                  -- get numbers of all waggons of the train

-- checks if a reservation is possible
checkReservation :: Database -> Reservation -> Bool
-- checks if a singleReservation is possible AND the requirement of the min free seats of the train isn't violated
checkReservation db (trainName, waggonNumber, startStation, endStation, (SingleReservation s)) = (checkSingleReservation db trainName waggonNumber s startStation endStation) && (checkMinFreeSeatsInTrainAvailable db trainName startStation endStation 1)
-- checks if a groupReservation is possible AND the requirement of the min free seats of the train isn't violated
checkReservation db (trainName, waggonNumber, startStation, endStation, (GroupReservation c))  = (checkGroupReservation db trainName waggonNumber c startStation endStation)  && (checkMinFreeSeatsInTrainAvailable db trainName startStation endStation c)
checkReservation _ _                                                                           = False

-- checks if a single reservation is possible
checkSingleReservation :: Database -> TrainName -> WaggonNumber -> SeatNumber -> StartStation -> EndStation -> Bool
checkSingleReservation db@(trains,reservations) trainName waggonNr seatNr startStation endStation
 -- are there free seats in this waggon (fst gets the min free seats) ?
    | fst(queryMinMaxSeats db trainName startStation endStation waggonNr) > 0 &&
 -- is the intersection of {startStation, startStation+1, ... endStation-1} and {all stations the seat with number seatNr is reserved in waggon waggonNr} empty?
 -- if yes -> reservation possible (this seat isn't already reserved for the given stations)
      intersect (range (startStation, endStation-1)) (querySeatReservedForStations db trainName waggonNr seatNr) == [] = True
    | otherwise = False

-- checks if a group reservation is possible
checkGroupReservation :: Database -> TrainName -> WaggonNumber -> PersonCount -> StartStation -> EndStation -> Bool
checkGroupReservation db@(trains,reservations) trainName waggonNr personCount startStation endStation
-- are there enough free seats is the waggon with given waggonNr?
-- fst gets the min free seats, if this is bigger or equal than the seats we want to reserve -> possible
   | fst(queryMinMaxSeats db trainName startStation endStation waggonNr) >= personCount = True
   | otherwise = False
