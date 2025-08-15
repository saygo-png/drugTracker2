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
listDrugs ListArgs{..} = do
  rls <- loadRenderLines getDetailed
  putStrLn =<< prettyTable (handleArgs getLines getUniques rls)
  where
    handleArgs :: LinesArg -> Bool -> Vector RenderLine -> Vector RenderLine
    handleArgs la unique rls =
      if unique
        then filter (not . getIsOld) rls
        else case la of
          LinesAll -> rls
          LinesInt n -> takeLast n rls
            where
              takeLast i = reverse . take i . reverse

prettyTable :: Vector RenderLine -> IO Text
prettyTable rls = do
  safeSetSGRCode <- getSafeSetSGRCode

  let header = fromList ["Nr", "Name", "Date"]

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
                then safeSetSGRCode [SetDefaultColor Foreground] <> columnString config
                else columnString config
         in intercalate delimiter (take 3 cells) <> " " <> unwords (drop 3 cells)

      getInfo RenderLine{..} = RenderContext (ListContext getReminding' getIsOld getIsMissed) vec
        where
          name = getEntryName getDrugLine
          idx = tshow getIndex
          vec = fromList [idx, name, getDateRel, getDateAbs]

  colorTableWithJoin customJoin color $ plainTable getInfo header rls
