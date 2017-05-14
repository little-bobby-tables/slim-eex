module Driver where
  import Parser
  import ElixirProcessor
  import Renderer

  import Text.Megaparsec (parse, parseErrorPretty)

  slimToEex :: String -> String
  slimToEex s =
    case (parse slim "source" s) of
      Right t -> renderTree (process t)
      Left e -> parseErrorPretty e
