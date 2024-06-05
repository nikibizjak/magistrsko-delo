module ArgumentParser where
import Options.Applicative

data Options = Options
  { debugInterpreter :: Bool,
    debugDirectory :: Maybe String,
    inputFile :: String
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "debug-interpreter"
          <> help "If set, output each evaluation step to the screen."
      )
    <*> optional
      ( strOption
          ( long "debug-directory"
              <> metavar "DIRECTORY"
              <> help "If set, output each evaluation step to a file 'step{index}.html' inside of the DIRECTORY folder."
          )
      )
    <*> argument
      str
      ( metavar "INPUT"
          <> help "STG program input file that will be executed."
      )

parseArguments =
  execParser optsParser
  where
    optsParser =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Run the STG interpreter."
            <> header "Crochet - an interpreter for the STG language"
        )