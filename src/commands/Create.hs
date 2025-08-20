{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Create (createDrugItem) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Vector qualified as V
import Lib
import Path qualified as P
import Path.IO qualified as PI
import System.Exit (exitFailure)
import Text.Printf (printf)
import Types

createDrugItem :: DrugDefinition -> IO ()
createDrugItem d = do
  o <- getCsvDrugDefinitions
  let writeAndLogWith d' = writeWithHeader (cons d d') (P.fromAbsFile o) >> wroteInfo d

  getFileState o >>= \case
    FileNotExists -> (PI.createDirIfMissing True =<< getDataDir) >> writeAndLogWith V.empty
    FileEmpty -> writeAndLogWith V.empty
    FileHasContent -> do
      (existingDefs, _) <- loadDrugDefinitions
      let entryExists = any ((== d.getName) . (.getName)) existingDefs
      when entryExists $ do
        printf "Definition for \"%s\" already exists\n" d.getName
        exitFailure
      writeAndLogWith existingDefs
  where
    wroteInfo DrugDefinition{..} =
      let period = getPeriod & tshow
       in printf "Created definition \"%s\" which should be taken every %s seconds\n" getName period

    writeWithHeader :: Vector DrugDefinition -> FilePath -> IO ()
    writeWithHeader drugDefs output =
      let header = definitionsToHeader csvDefinitionsHT
          dataForWrite = Cassava.encodeByName header $ toList drugDefs
       in B8.writeFile output $ toStrict dataForWrite
