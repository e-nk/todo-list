module Task (Task(..), newTask) where

data Task = Task
    { taskId      :: Int
    , description :: String
    , isCompleted :: Bool
    } deriving (Show, Read)  -- Ensure 'Read' is derived

newTask :: Int -> String -> Task
newTask idDesc desc = Task idDesc desc False  -- Rename `taskId` to `idDesc`

