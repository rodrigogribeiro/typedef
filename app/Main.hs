
module Main where    

import Options.Applicative
import System.FilePath    
import Solver (solver)

-- command line parser arguments    

data Config = Config {
                inputFile  :: FilePath
              , outputFile :: Maybe FilePath                
              } deriving Show

config :: Parser Config
config = Config <$> strOption
                      (  long "input-file"
                      <> short 'i'        
                      <> metavar "INPUT"
                      <> help "Core C or Constraint input file")
                <*> optional
                      (strOption
                         (  long "output-file"
                         <> short 'o'        
                         <> metavar "OUTPUT"
                         <> help "Output file with infered typedef's"))

opts :: ParserInfo Config
opts = info (config <**> helper)
            ( fullDesc
            <> progDesc "Infer missing typedef's for a constraint in INPUT file"
            <> header "Constraint solver for typedef inference" )                      
                    
            
-- writing results of the solver

makeFileName :: FilePath -> FilePath
makeFileName p = p -<.> "tdef" 

writeResults :: Config -> [String] -> IO ()                
writeResults (Config inp Nothing) ts = do
                                       writeFile (makeFileName inp)
                                                 (concat ts)
                                       mapM_ putStrLn ts          
writeResults (Config _ (Just o)) ts = writeFile o (concat ts)                      

-- main function

main :: IO ()
main = do
         cfg <- execParser opts
         c <- readFile (inputFile cfg)        
         ts <- solver c (inputFile cfg)
         either putStrLn (writeResults cfg) ts
