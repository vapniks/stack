-- | Configuration options for building.

module Stack.Types.Config.Build
    (
      BuildOpts(..)
    , defaultBuildOpts
    , BuildOptsMonoid(..)
    , TestOpts(..)
    , defaultTestOpts
    , BenchmarkOpts(..)
    , defaultBenchmarkOpts
    , FileWatchOpts(..)
    , BuildSubset(..)
    )
    where

import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Stack.Types.FlagName
import           Stack.Types.PackageName
                 
data BuildOpts =
  BuildOpts {boptsTargets :: ![Text]
            ,boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsHaddock :: !Bool
            -- ^ Build haddocks?
            ,boptsHaddockDeps :: !(Maybe Bool)
            -- ^ Build haddocks for dependencies?
            ,boptsDryrun :: !Bool
            ,boptsGhcOptions :: ![Text]
            ,boptsFlags :: !(Map (Maybe PackageName) (Map FlagName Bool))
            ,boptsInstallExes :: !Bool
            -- ^ Install executables to user path after building?
            ,boptsPreFetch :: !Bool
            -- ^ Fetch all packages immediately
            ,boptsBuildSubset :: !BuildSubset
            ,boptsFileWatch :: !FileWatchOpts
            -- ^ Watch files for changes and automatically rebuild
            ,boptsKeepGoing :: !(Maybe Bool)
            -- ^ Keep building/running after failure
            ,boptsForceDirty :: !Bool
            -- ^ Force treating all local packages as having dirty files

            ,boptsTests :: !Bool
            -- ^ Turn on tests for local targets
            ,boptsTestOpts :: !TestOpts
            -- ^ Additional test arguments

            ,boptsBenchmarks :: !Bool
            -- ^ Turn on benchmarks for local targets
            ,boptsBenchmarkOpts :: !BenchmarkOpts
            -- ^ Additional test arguments
            ,boptsExec :: ![(String, [String])]
            -- ^ Commands (with arguments) to run after a successful build
            ,boptsOnlyConfigure :: !Bool
            -- ^ Only perform the configure step when building
            ,boptsReconfigure :: !Bool
            -- ^ Perform the configure step even if already configured
            ,boptsCabalVerbose :: !Bool
            -- ^ Ask Cabal to be verbose in its builds
            }
  deriving (Show)

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
    { boptsTargets = []
    , boptsLibProfile = False
    , boptsExeProfile = False
    , boptsHaddock = False
    , boptsHaddockDeps = Nothing
    , boptsDryrun = False
    , boptsGhcOptions = []
    , boptsFlags = Map.empty
    , boptsInstallExes = False
    , boptsPreFetch = False
    , boptsBuildSubset = BSAll
    , boptsFileWatch = NoFileWatch
    , boptsKeepGoing = Nothing
    , boptsForceDirty = False
    , boptsTests = False
    , boptsTestOpts = defaultTestOpts
    , boptsBenchmarks = False
    , boptsBenchmarkOpts = defaultBenchmarkOpts
    , boptsExec = []
    , boptsOnlyConfigure = False
    , boptsReconfigure = False
    , boptsCabalVerbose = False
    }

-- | An uninterpreted representation of build options.
-- Configurations may be "cascaded" using mappend (left-biased).
data BuildOptsMonoid = BuildOptsMonoid
    { buildMonoidTargets :: ![Text]
    , buildMonoidLibProfile :: !Bool
    , buildMonoidExeProfile :: !Bool
    , buildMonoidHaddock :: !Bool
    , buildMonoidHaddockDeps :: !(Maybe Bool)
    , buildMonoidDryrun :: !Bool
    , buildMonoidGhcOptions :: ![Text]
    , buildMonoidFlags :: !(Map (Maybe PackageName) (Map FlagName Bool))
    , buildMonoidInstallExes :: !Bool
    , buildMonoidPreFetch :: !Bool
    , buildMonoidBuildSubset :: !BuildSubset
    , buildMonoidFileWatch :: !FileWatchOpts
    , buildMonoidKeepGoing :: !(Maybe Bool)
    , buildMonoidForceDirty :: !Bool
    , buildMonoidTests :: !Bool
    , buildMonoidTestOpts :: !TestOpts
    , buildMonoidBenchmarks :: !Bool
    , buildMonoidBenchmarkOpts :: !BenchmarkOpts
    , buildMonoidExec :: ![(String, [String])]
    , buildMonoidOnlyConfigure :: !Bool
    , buildMonoidReconfigure :: !Bool
    , buildMonoidCabalVerbose :: !Bool
    } deriving (Show)

-- | Which subset of packages to build
data BuildSubset
    = BSAll
    | BSOnlySnapshot
    -- ^ Only install packages in the snapshot database, skipping
    -- packages intended for the local database.
    | BSOnlyDependencies
    deriving (Show, Eq)

-- | Options for the 'FinalAction' 'DoTests'
data TestOpts =
  TestOpts {toRerunTests :: !Bool -- ^ Whether successful tests will be run gain
           ,toAdditionalArgs :: ![String] -- ^ Arguments passed to the test program
           ,toCoverage :: !Bool -- ^ Generate a code coverage report
           ,toDisableRun :: !Bool -- ^ Disable running of tests
           } deriving (Eq,Show)

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts
    { toRerunTests = True
    , toAdditionalArgs = []
    , toCoverage = False
    , toDisableRun = False
    }

-- | Options for the 'FinalAction' 'DoBenchmarks'
data BenchmarkOpts =
  BenchmarkOpts {beoAdditionalArgs :: !(Maybe String) -- ^ Arguments passed to the benchmark program
                ,beoDisableRun :: !Bool -- ^ Disable running of benchmarks
                } deriving (Eq,Show)

defaultBenchmarkOpts :: BenchmarkOpts
defaultBenchmarkOpts = BenchmarkOpts
    { beoAdditionalArgs = Nothing
    , beoDisableRun = False
    }

data FileWatchOpts
  = NoFileWatch
  | FileWatch
  | FileWatchPoll
  deriving (Show,Eq)
