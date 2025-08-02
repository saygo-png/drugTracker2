{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Create (createDrugItem) where

import ClassyPrelude
import ConfigAndTypes
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Vector qualified as V
import Lib
import Path qualified as P
import Path.IO qualified as PI
import System.Exit (exitFailure)
import Text.Printf (printf)

createDrugItem :: DrugDefinition -> IO ()
createDrugItem d = do
  o <- getCsvDrugDefinitions
  let writeAndLogWith d' = writeWithHeader (cons d d') (P.fromAbsFile o) >> wroteInfo d

  getFileState o >>= \case
    FileNotExists -> (PI.createDirIfMissing True =<< getDataDir) >> writeAndLogWith V.empty
    FileEmpty -> writeAndLogWith V.empty
    FileHasContent -> do
      existingDefs <- loadDrugDefinitions
      let entryExists = any ((== getName d) . getName) existingDefs
      when entryExists $ do
        printf "Definition for \"%s\" already exists\n" $ getName d
        exitFailure
      writeAndLogWith existingDefs
  where
    wroteInfo DrugDefinition{..} =
      let period = getPeriod & tshow
       in printf "Created definition \"%s\" which should be taken every %s seconds\n" getName period

    writeWithHeader :: Vector DrugDefinition -> FilePath -> IO ()
    writeWithHeader drugDefs output = do
      let drugDefs' = toList drugDefs
      let header = definitionsToHeader csvDefinitionsHT
      let dataForWrite = Cassava.encodeByName header drugDefs'
      B8.writeFile output (toStrict dataForWrite)
