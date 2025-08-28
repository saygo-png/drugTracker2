module List (
  listDrugs,
) where

import ClassyPrelude
import Lib
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
  let header :: Row
      header = Row $ fromList ["Nr", "Name", "Date"]

      color (StatusContext _) = White
      color (ListContext isEnabled isOld isMissed)
        | not isEnabled = Black
        | isOld = Green
        | isMissed = Red
        | not isMissed = Green
        | otherwise = White

      getInfo :: RenderLine -> RenderContext SemiRow
      getInfo rl = RenderContext (ListContext rl.reminding rl.isOld rl.isMissed) row
        where
          name = PureCell rl.drugLine.name
          idx = PureCell $ tshow rl.index
          date = SemiCell $ fromList [rl.dateRel, rl.dateAbs]
          row = SemiRow $ fromList [idx, name, date]

  colorTable color $ plainTable getInfo header rls
