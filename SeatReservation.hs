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
import Directory (doesFileExist)
	
	
-- Constants
kFileName = "seatReservation.txt"
	
-- Type Definitions
type Station		 				  = Int
type StartStation    				  = Station
type EndStation		 				  = Station
type WaggonNumber	 				  = Int
type SeatNumber		 				  = Int
type SeatCountPerWaggon 			  = Int
type FreeSeatsWithoutReservationCount = Int
type TrainName		 				  = String


type Waggon          = (WaggonNumber, SeatCountPerWaggon, FreeSeatsWithoutReservationCount)
type Train			 = (TrainName, [Waggon])

data SeatReservation = SingleReservation WaggonNumber SeatNumber |
				   	   GroupReservation WaggonNumber deriving (Show, Read, Eq)

type Reservation 	 = (TrainName, SeatReservation, StartStation, EndStation)
type Database		 = ([Train], [Reservation])


-- Main Program
main :: IO ()
main = do db <- loadDatabase kFileName
          mainLoop db
          saveDatabase db kFileName

-- does all the work
mainLoop :: Database -> IO ()
mainLoop (trains, reservations) = do
    command <- readCommand
    case command of
        0    -> putStrLn "Beenden..."
        _    -> mainLoop (trains, reservations)



-- Loads the database with path fileName and returns it
-- if the file doesn't exists, return an empty database
loadDatabase :: String -> IO Database
loadDatabase fileName = catch (do fileContent <- readFile fileName	-- try to read db
                                  return (read fileContent))		-- cast db to Database
                              (\e -> return ([],[]))				-- on error return empty db (e.g. file doesn't exist)


-- Saves the database db into a file with name fileName
saveDatabase :: Database -> String -> IO ()
saveDatabase db fileName = writeFile fileName (show db)
	

-- reads the next command from stdin (Integer)
readCommand :: IO Int
readCommand = do putStr "Command: "
                 line <- getLine
                 return (read line :: Int)