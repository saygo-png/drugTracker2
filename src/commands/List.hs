module List (
  listDrugs,
) where

import ClassyPrelude
import Lib
import LoadConfig
import System.Console.ANSI
import Table
import Types

listDrugs :: ListArgs -> IO ()
listDrugs las = do
  rls <- loadRenderLines las.detailed
  putStrLn =<< prettyTable (handleArgs las.lines las.uniques rls)
  where
    handleArgs :: LinesArg -> Bool -> Vector RenderLine -> Vector RenderLine
    handleArgs la unique rls =
      if unique
        then filter (not . (.isOld)) rls
        else case la of
          LinesAll -> rls
          LinesInt n -> takeLast n rls
            where
              takeLast i = reverse . take i . reverse

prettyTable :: Vector RenderLine -> IO Text
prettyTable rls = do
  safeSetSGRCode <- getSafeSetSGRCode

  let header :: Vector Text
      header = fromList ["Nr", "Name", "Date"]

      color (StatusContext _) = White
      color (ListContext isEnabled isOld isMissed)
        | not isEnabled = Black
        | isOld = Green
        | isMissed = Red
        | not isMissed = Green
        | otherwise = White

      customJoin useColor cells =
        let delimiter =
              if useColor
                then safeSetSGRCode [SetDefaultColor Foreground] <> config.columnString
                else config.columnString
         in intercalate delimiter (take 3 cells) <> " " <> unwords (drop 3 cells)

      getInfo rl = RenderContext (ListContext rl.reminding rl.isOld rl.isMissed) vec
        where
          name = rl.drugLine.name
          idx = tshow rl.index
          vec = fromList [idx, name, rl.dateRel, rl.dateAbs]

  colorTableWithJoin customJoin color $ plainTable getInfo header rls
