import System.Console.ANSI
import System.IO (stdout)

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout

  if stdoutSupportsANSI
    then do
      clearScreen

      setCursorPosition 5 0
      setTitle "ANSI Terminal Short Example"

      setSGR
        [ SetConsoleIntensity BoldIntensity,
          SetColor Foreground Vivid Red
        ]
      putStr "Hello "

      setSGR
        [ SetConsoleIntensity NormalIntensity,
          SetColor Foreground Vivid White,
          SetColor Background Dull Blue
        ]
      putStrLn "World!"
    else
      putStrLn "Standard output does not support 'ANSI' escape codes."