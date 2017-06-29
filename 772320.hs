-- 
-- MATHFUN
-- 772320
--

import System.IO 

--
-- Types
type Film = (Title, Director, Year, Fans)
type Title = String
type Director = String
type Year = Int
type Fans = [String]

-- Define Film type here 
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"]),
				("The Fly", "David Cronenberg",1986, ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
				("Body Of Lies", "Ridley Scott", 2008, ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
				("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"]),
				("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"]),
				("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
				("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"]),
				("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"]),
				("Prometheus", "Ridley Scott", 2012, ["Kevin", "Tim", "Emma", "Jo", "Liz"]),
				("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"]),
				("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"]),
				("Jaws", "Steven Spielberg", 1975, ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"]),
				("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"]),
				("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"]),
				("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"]),
				("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"]),
				("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"]),
				("True Lies", "James Cameron", 1994, ["Sam", "Dave"]),
				("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"]),
				("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"]),
				("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"]),
				("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"]),
				("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"]),
				("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"]),
				("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"])]

                
--  Your functional code goes here

--Converts a given list output to string output
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs) = x ++ "\n" ++ listToString xs

--Makes sure elements in a list are unique
unique :: [String] -> [String]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Converts a film to a string format
filmToString :: Film -> String
filmToString (title, director, year, fans) = ("Name: " ++ title ++
												"\nDirector: " ++ director ++
												"\nYear: " ++ (show year) ++
												"\nFans: " ++ (show (length fans))) 

                                                
--1) Takes the film, and formats it to an appropriate string for the database
addFilm :: [Film] -> Film -> [Film]
addFilm films film = films ++ [film]

--2) Shows all films from the database
allFilms :: [Film] -> String
allFilms [] = ""
allFilms ((title, director, year, fans):xs) = ("Name: " ++ title ++
												"\nDirector: " ++ director ++
												"\nYear: " ++ (show year) ++
												"\nFans: " ++ (show (length fans))) 
												++ "\n \n" ++ allFilms xs
					
--3) Shows all films that were brought out in a given year					
allFilmsOnYear :: [Film] -> Int -> String
allFilmsOnYear [] targetYear = ""
allFilmsOnYear ((title, director, year, fans):xs) targetYear
	| year == targetYear = ("Name: " ++ title ++
						"\nDirector: " ++ director ++
						"\nYear: " ++ (show year) ++
						"\nFans: " ++ (show (length fans))) 
						++ "\n \n" ++ allFilmsOnYear xs targetYear
	| otherwise = allFilmsOnYear xs targetYear

--4) Shows all films that have a given fan	
allFilmsForFan :: [Film] -> String -> String
allFilmsForFan [] targetFan = ""   
allFilmsForFan ((title, director, year, []):xs) targetFan = allFilmsForFan xs targetFan -- If there are no fans, move onto the next film
allFilmsForFan ((title, director, year, (fan:fans)):xs) targetFan
	| fan == targetFan  = ("Name: " ++ title ++
					"\nDirector: " ++ director ++
					"\nYear: " ++ (show year) ++
					"\nFans: " ++ (show (length (fan:fans)))) 
					++ "\n \n" ++ allFilmsForFan xs targetFan
	| otherwise = allFilmsForFan ((title, director, year, (fans)):xs) targetFan

--5) Shows all fans of a given film	
allFansForFilm :: [Film] -> String -> [String]
allFansForFilm [] targetTitle = []
allFansForFilm ((title, director, year, []):xs) targetTitle = allFansForFilm (xs) targetTitle  -- If there are no fans, move onto the next film
allFansForFilm ((title, director, year, (fan:fans)):xs) targetTitle
	| targetTitle == title = [fan] ++ allFansForFilm ((title, director, year, (fans)):xs) targetTitle
	| otherwise = allFansForFilm (xs) targetTitle

--6) Takes a film and adds a new fan, returning a film value to be added to the database
addFanForFilm :: [Film] -> [Film] -> String -> String -> [Film]
addFanForFilm films [] targetTitle newFan = []
addFanForFilm films ((title, director, year, fans):xs) targetTitle newFan
	| title == targetTitle = (films ++ [(title, director, year, unique(fans ++ [newFan]))] ++ xs) --Adds the films previosly viewed & unaltered, with the film with the new fan, and the films not yet searched
	| otherwise = addFanForFilm (films ++ [((title, director, year, []))]) xs targetTitle newFan

--7) Shows all fans of a given director, needs to be called with unique
allFansForDirector :: [Film] -> String -> [String]
allFansForDirector [] targetDirector = []
allFansForDirector ((title, director, year, []):xs) targetDirector = allFansForDirector (xs) targetDirector -- If there are no fans, move onto the next film
allFansForDirector ((title, director, year, (fan:fans)):xs) targetDirector
	| targetDirector == director = [fan] ++ allFansForDirector ((title, director, year, (fans)):xs) targetDirector
	| otherwise = allFansForDirector (xs) targetDirector

--8a) Starts the count to see how many of each director film a fan likes
directorsList :: String -> [Film] -> String
directorsList name database = allDirectorsCompare (unique (allDirectorsFans database name)) (allDirectorsFans database name)
     
--8b) Gets a list of all directors with a given fan
allDirectorsFans :: [Film] -> String -> [String]
allDirectorsFans [] targetFan = []
allDirectorsFans ((title, director, year, fans):xs) targetFan = [director] ++ (allDirectorsFans xs targetFan)

--8c) Formats the list of director films a fan likes
allDirectorsCompare :: [String] -> [String] -> String
allDirectorsCompare [] (y:ys) = ""
allDirectorsCompare (x:xs) (y:ys) = x ++ " " ++ countElems x (y:ys) ++ "\n" ++ allDirectorsCompare xs (y:ys)

--8d) Counts how many times an element occurs in the list
countElems :: String -> [String] -> String
countElems i [] = show 0
countElems i (x:xs) 
 | i == x = show ((read(countElems i xs) :: Int) + 1)
 | otherwise = (countElems i xs)	
	
-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2017 film "Alien: Covenant"
--                   by "Ridley Scott" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films released after 2008
--demo 4  = putStrLn all films that "Liz" is a fan of
--demo 5  = putStrLn all fans of "Jaws"
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- Your user interface code goes here
--

-- Loads the films from the database
loadFilms :: IO [Film]
loadFilms = do
	films <- readFile("films.txt")
	let filmsList = (read films :: [Film])
	return filmsList

-- Will get the films from database and load the main menu	
main :: IO ()
main = do
	films <- loadFilms
	putStrLn (allFilms films)
	putStr ("Enter your name: ")
	name <- getLine
	start films name

-- Contains the	main menu
start :: [Film] -> String -> IO ()
start films name = do
	putStrLn("\n")    
	putStrLn("Main Menu \n")
	putStrLn("COMMANDS")
	putStrLn("1 - Add a new film")
	putStrLn("2 - Give all films")
	putStrLn("3 - Give all films in a given year")
	putStrLn("4 - Give all films a user likes\n")
	putStrLn("5 - Give all fans of a film")
	putStrLn("6 - Add a fan to a film")
	putStrLn("7 - Give all fans of a director")
	putStrLn("8 - List all directors a user is a fan of\n")
	putStrLn("exit - Stop program \n")
	putStr "Enter a demo: "
	demoNum <- getLine
	putStrLn ""
	demo films demoNum name

-- Contains how the system should react to the option from the main menu
demo :: [Film] -> String -> String -> IO ()
demo films selection name
	| selection == "1" = do
		putStr "Enter film name: "
		film <- getLine
		putStr "Enter director name: "
		director <- getLine
		putStr "Enter year released: "
		year <- getLine
		let films = addFilm testDatabase ((read ("\"" ++ film ++ "\""):: String),
												(read ("\"" ++ director ++ "\""):: String),
												(read year :: Int),
												([]))
		putStrLn (allFilms films)										
		putStrLn "Film added \nPress enter to continue..."
		getLine
		start films name
	| selection == "2" = do
		putStrLn (allFilms films ++ "Press enter to continue")
		getLine        
		start films name
	| selection == "3" = do
		putStr "Enter a target year: "
		year <- getLine
		putStrLn ""
		putStrLn (allFilmsOnYear testDatabase (read year :: Int) ++ "Press enter to continue")
		getLine
		start films name		
	| selection == "4" = do
		putStrLn ""
		putStrLn (allFilmsForFan testDatabase (read ("\"" ++ name ++ "\"") :: String))
		putStr "Press enter to continue..."
		getLine
		start films name
	| selection == "5" = do
		putStr "Enter a film name: "
		film <- getLine
		putStrLn ""
		putStrLn (listToString (allFansForFilm testDatabase (read ("\"" ++ film ++ "\""):: String)))
		putStr "Press enter to continue..."
		getLine
		start films name
    | selection == "6" = do
		putStr "Enter film name: "
		film <- getLine
		putStrLn ""
		let films = addFanForFilm [] testDatabase (read ("\"" ++ film ++ "\""):: String) (read ("\"" ++ name ++ "\""):: String)
		putStrLn (listToString (allFansForFilm films film))
		putStr "Press enter to continue..."
		getLine
		start films name
	| selection == "7" = do
		putStr "Enter a director name: "
		director <- getLine
		putStrLn ""
		putStrLn (listToString (unique (allFansForDirector testDatabase (read ("\"" ++ director ++ "\"")))))
		putStr "Press enter to continue..."
		getLine
		start films name
	| selection == "8" = do
		putStrLn ""
		putStrLn (directorsList (read ("\"" ++ name ++ "\"")) testDatabase)
		putStr "Press enter to continue..."
		getLine
		start films name
	| selection == "exit" = do
		writeFile "films.txt" (show films)
		return ()
	| otherwise = do
		putStrLn ("Command not recognised")
		getLine
		putStrLn ("")
		start films name