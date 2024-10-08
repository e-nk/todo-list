module TaskManager (addTask, viewTasks, markComplete, deleteTask, editTask) where

import Task (Task(..), newTask)
import Data.List (intercalate)

-- Function to add a new task to the list
-- In TaskManager.hs
addTask :: String -> [Task] -> [Task]
addTask desc tasks =
    let newId = if null tasks then 1 else (maximum (map taskId tasks) + 1)
    in Task newId desc False : tasks

-- Function to view tasks as a formatted string
viewTasks :: [Task] -> String
viewTasks tasks = intercalate "\n" (map show tasks)  -- Converts each task to a string and joins with newlines

markComplete :: [Task] -> Int -> [Task]
markComplete tasks taskIdValue = map (\t -> if taskId t == taskIdValue then t { isCompleted = True } else t) tasks

-- In TaskManager.hs

deleteTask :: Int -> [Task] -> [Task]
deleteTask _ [] = []
deleteTask taskId (task@(Task id _ _):tasks)
    | id == taskId = tasks  -- Skip this task to delete it
    | otherwise    = task : deleteTask taskId tasks


-- Assuming Task has a constructor: Task Int String Bool
-- In TaskManager.hs
editTask :: Int -> String -> [Task] -> [Task]
editTask taskId newDesc tasks = map updateTask tasks
  where
    updateTask task@(Task id _ completed) =
      if id == taskId then Task id newDesc completed else task
