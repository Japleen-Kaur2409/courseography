{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (acquireDatabase,
    mockRequest,
    runServerPart,
    clearDatabase,
    releaseDatabase,
    runServerPartWithQuery,
    runServerPartWithCourseInfoQuery,
    runServerPartWithProgramQuery,
    runServerPartWithGraphGenerate,
    withDatabase)
    where

import Config (databasePath)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as Map
import Data.Text (unpack)
import Database.Database (setupDatabase)
import Database.Persist.Sqlite (Filter, SqlPersistM, deleteWhere)
import Database.Tables
import Happstack.Server (ContentType (..), HttpVersion (..), Input (..), Method (GET, PUT),
                         Request (..), Response, RqBody (..), ServerPart, inputContentType,
                         inputFilename, inputValue, simpleHTTP'')
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import Test.Tasty (TestTree, testGroup, withResource)

-- | Helper to create a query input parameter
createQueryInput :: String -> String -> Input
createQueryInput _ paramValue = Input
    { inputValue = Right (BSL8.pack paramValue)
    , inputFilename = Nothing
    , inputContentType = defaultContentType
    }

-- | Generalized function to create a mock request with query parameters
createMockRequest :: Method -> [String] -> String -> [(String, Input)] -> IO Request
createMockRequest method pathSegments uri queryInputs = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False
        , rqMethod          = method
        , rqPaths           = pathSegments
        , rqUri             = uri
        , rqQuery           = ""
        , rqInputsQuery     = queryInputs
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A mock request for running ServerPartWithCourseInfoQuery, specifically for courseInfo
mockRequestWithCourseInfoQuery :: String -> IO Request
mockRequestWithCourseInfoQuery dept = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False
        , rqMethod          = GET
        , rqPaths           = ["course-info"]
        , rqUri             = "/course-info"
        , rqQuery           = ""
        , rqInputsQuery     = [("dept", Input {
            inputValue = Right (BSL8.pack dept),
            inputFilename = Nothing,
            inputContentType = defaultContentType
          })]
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A mock request for running ServerPartWithProgramQuery, specifically for retrieveProgram
mockRequestWithProgramQuery :: String -> IO Request
mockRequestWithProgramQuery programCode = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False
        , rqMethod          = GET
        , rqPaths           = ["program"]
        , rqUri             = "/program"
        , rqQuery           = ""
        , rqInputsQuery     = [("code", Input {
            inputValue = Right (BSL8.pack programCode),
            inputFilename = Nothing,
            inputContentType = defaultContentType
          })]
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A mock request for the graph generate route, specifically for findAndSavePrereqsResponse
mockRequestWithGraphGenerate :: BSL.ByteString -> IO Request
mockRequestWithGraphGenerate payload = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    putMVar requestBody (Body payload)
    return Request
        { rqSecure          = False
        , rqMethod          = method
        , rqPaths           = pathSegments
        , rqUri             = uri
        , rqQuery           = ""
        , rqInputsQuery     = []
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A minimal mock request for running a ServerPart
mockRequest :: IO Request
mockRequest = createMockRequest GET [] "/" []

-- | A mock request with a query parameter for retrieveCourse
mockRequestWithQuery :: String -> IO Request
mockRequestWithQuery courseName = 
    createMockRequest GET ["course"] "/course" 
        [("name", createQueryInput "name" courseName)]

-- | A mock request with a query parameter for courseInfo
mockRequestWithCourseInfoQuery :: String -> IO Request
mockRequestWithCourseInfoQuery dept = 
    createMockRequest GET ["course-info"] "/course-info" 
        [("dept", createQueryInput "dept" dept)]

-- | A mock request with a query parameter for retrieveProgram
mockRequestWithProgramQuery :: String -> IO Request
mockRequestWithProgramQuery programCode = 
    createMockRequest GET ["program"] "/program" 
        [("code", createQueryInput "code" programCode)]

-- | A mock request with a body payload for the graph generate route
mockRequestWithGraphGenerate :: BSL.ByteString -> IO Request
mockRequestWithGraphGenerate payload = 
    createMockRequestWithBody PUT ["graph-generate"] "/graph-generate" payload

-- | Default content type for the MockRequestWithQuery, specifically for retrieveCourse
defaultContentType :: ContentType
defaultContentType = ContentType
    { ctType = "text"
    , ctSubtype = "html"
    , ctParameters = []
    }

-- | Generalized helper function to run a ServerPart with a request
runServerPartWith :: ServerPart Response -> IO Request -> IO Response
runServerPartWith sp requestIO = do
    request <- requestIO
    simpleHTTP'' sp request

-- | Helper function to run ServerPart Response
runServerPart :: ServerPart Response -> IO Response
runServerPart sp = runServerPartWith sp mockRequest

-- | Helper function to run ServerPart Response with a query parameter for retrieveCourse
runServerPartWithQuery :: ServerPart Response -> String -> IO Response
runServerPartWithQuery sp courseName = 
    runServerPartWith sp (mockRequestWithQuery courseName)

-- | Helper function to run ServerPart Response with a query parameter for courseInfo
runServerPartWithCourseInfoQuery :: ServerPart Response -> String -> IO Response
runServerPartWithCourseInfoQuery sp dept = do
    request <- mockRequestWithCourseInfoQuery dept
    simpleHTTP'' sp request

-- | Helper function to run ServerPartWithQuery Response for retrieveProgram
runServerPartWithProgramQuery :: ServerPart Response -> String -> IO Response
runServerPartWithProgramQuery sp programCode = do
    request <- mockRequestWithProgramQuery programCode
    simpleHTTP'' sp request
    
-- | Helper function to run ServerPartWithGraphGenerate for findAndSavePrereqsResponse
runServerPartWithGraphGenerate :: ServerPart Response -> BSL.ByteString -> IO Response
runServerPartWithGraphGenerate sp payload = 
    runServerPartWith sp (mockRequestWithGraphGenerate payload)

-- | Clear all the entries in the database
clearDatabase :: SqlPersistM ()
clearDatabase = do
    deleteWhere ([] :: [Filter Department])
    deleteWhere ([] :: [Filter Courses])
    deleteWhere ([] :: [Filter Meeting])
    deleteWhere ([] :: [Filter Times])
    deleteWhere ([] :: [Filter Breadth])
    deleteWhere ([] :: [Filter Distribution])
    deleteWhere ([] :: [Filter Database.Tables.Text])
    deleteWhere ([] :: [Filter Shape])
    deleteWhere ([] :: [Filter Path])
    deleteWhere ([] :: [Filter Post])
    deleteWhere ([] :: [Filter PostCategory])
    deleteWhere ([] :: [Filter Building])
    deleteWhere ([] :: [Filter Graph])

acquireDatabase :: IO ()
acquireDatabase = do
    setEnv "APP_ENV" "test"
    setupDatabase True

releaseDatabase :: () -> IO ()
releaseDatabase _ = do
    path <- databasePath
    removeFile $ unpack path
    unsetEnv "APP_ENV"

withDatabase ::  String -> [TestTree] -> TestTree
withDatabase label tests =
    withResource acquireDatabase releaseDatabase $ \_ ->
    testGroup label tests
