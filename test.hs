module Main where
import System.Environment

main :: IO ()
main = do
	putStrLn "Please print your name"
	name <- getLine
	putStrLn name
